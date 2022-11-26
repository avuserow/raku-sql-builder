unit class SQL::Cantrip;

my sub column(Str $s) {
    return $s;
}

my sub quote-value(Str $s) {
    my $q = $s.subst("'", "''");
    return "'$s'";
}

my sub quote-column(Str $s) {
    my $q = $s.subst('"', '""');
    return '"' ~ $s ~ '"';
}

my role SQLSyntax {
    method sql {...}
    method values {...}
}

class SQLStatement does SQLSyntax {
    has $.sql;
    has @.values;
}

# SQLSyntax representing a column or table name, quoted with the column rules (also works on tables)
class Identifier does SQLSyntax is export {
    has Str() $.relation;
    has Str() $.column;
    has Str() $.cast;

    constant relation-pattern = rx/^
        [$<table>=(<-[.]>+) '.']? # optional table/relation name
        $<column>=(<-[:]>+) # required column name
        ['::' $<cast>=(.+)]? # optional postgres-style cast
    $/;

    method new(Str $string) {
        $string ~~ relation-pattern;
        self.bless(:$<relation>, :$<column>, :$<cast>);
    }

    method sql {
        my $s = "";
        $s ~= "{quote-column($_)}." with $!relation;
        $s ~= quote-column($!column);
        $s ~= "::$_" with $!cast;
        return $s;
    }
}

# SQLSyntax representing a literal value. Numbers and booleans are formatted as expected. Strings are escaped by string rules (double quotes).
class Value does SQLSyntax is export {
    method sql {...}
}

# SQLSyntax representing a single placeholder. Always emits a '?' token
class Placeholder does SQLSyntax is export {
    has $.value;

    method new($value) {
        self.bless(:$value);
    }

    method sql {
        return '?';
    }
}

# SQLSyntax representing a raw value, unescaped in any way
class Raw does SQLSyntax is export {
    has $.value;

    method new($value) {
        self.bless(:$value);
    }

    method sql {
        return $!value;
    }
}

# SQLSyntax representing a function call. First argument is unescaped, all subsequent arguments are treated as columns (if not otherwise specified)
class Fn does SQLSyntax is export {
    method sql {!!!}
}

my sub fragment(SQLSyntax:U $default-class, $value) {
    $value.does(SQLSyntax) ?? $value !! $default-class.new($value);
}

my sub append-fragments(Str $sql is rw, @bind, SQLSyntax:U $default-class, @items, Str :$join = '') {
    for @items.kv -> $i, $value {
        my $item = $value.does(SQLSyntax) ?? $value !! $default-class.new($value);
        $sql ~= $join unless $i == 0;
        $sql ~= $item.sql;
        append @bind, .value when $item ~~ Placeholder;
    }
}

class ConditionClause {
    has Str $.mode = 'none';
    has @.clauses;

    submethod BUILD(:$mode = 'none', :@clauses) {
        $!mode = $mode;
        @!clauses = @clauses[0] ~~ Positional|Pair ?? @clauses !! @clauses.item;

        if $mode eq 'none' && @!clauses > 1 {
            die "internal error: mode=none but multiple clauses";
        }
    }

    method sql {
        my @parts;
        my @bind;

        for @!clauses -> $clause {
            given $clause {
                when Pair {
                    my $key = fragment(Identifier, $clause.key);
                    my $value = fragment(Placeholder, $clause.value);

                    push @parts, "{$key.sql} = {$value.sql}";
                    append @bind, ($key, $value).map({.value when Placeholder});
                }
                when Positional {
                    die "wrong number of args!!" unless $clause.elems == 3;

                    my $col = fragment(Identifier, $clause[0]);
                    my $op = fragment(Raw, $clause[1]);
                    my $value = fragment(Placeholder, $clause[2]);

                    push @parts, "{$col.sql} {$op.sql} {$value.sql}";
                    append @bind, ($col, $op, $value).map({.value when Placeholder});
                }
                when SQLSyntax {
                }
                default {
                    die "Don't know how to handle parameter of type {$clause.^name}: {$clause.raku}";
                }
            }
        }

        return @parts.join(" {$.mode.uc} "), @bind;
    }
}

class SelectBuilder {
    has @.select-columns;
    has SQLSyntax $.from;
    has ConditionClause $.where;
    has @.order-columns;
    has SQLSyntax $.limit-count;

    # Single pair
    multi method where(*%clauses where *.elems == 1) {
        $!where = ConditionClause.new(:clauses[%clauses.list]);
        self;
    }

    # Single clause (list)
    multi method where($clause) {
        $!where = ConditionClause.new(:clauses[$clause.flat]);
        self;
    }

    # Multiple clauses
    multi method where(@clauses, :$or!) {
        $!where = ConditionClause.new(:mode<or>, :@clauses);
        self;
    }
    multi method where(@clauses, :$and!) {
        $!where = ConditionClause.new(:mode<and>, :@clauses);
        self;
    }

    multi method select(:$all!) {
        @!select-columns = Raw.new('*');
        self;
    }
    multi method select(*@columns) {
        @!select-columns = @columns;
        self;
    }

    method order-by(*@columns) {
        @!order-columns = @columns;
        self;
    }

    method limit(Int $limit) {
        $!limit-count = Placeholder.new($limit);
        self;
    }

    method build {
        die "no columns to select" unless @!select-columns;

        my @bind;
        my $sql = 'SELECT ';
        append-fragments($sql, @bind, Identifier, @!select-columns, :join(', '));
        $sql ~= " FROM {$!from.sql}";
        append @bind, .value when $!from ~~ Placeholder;

        if $!where {
            my ($parts, @where-bind) = $!where.sql;
            $sql ~= ' WHERE ' ~ $parts;
            append @bind, @where-bind;
        }

        if @!order-columns {
            $sql ~= ' ORDER BY ';
            append-fragments($sql, @bind, Identifier, @!order-columns, :join(', '));
        }

        if $!limit-count {
            my ($parts, @limit-bind) = $!limit-count.sql;
            $sql ~= ' LIMIT ' ~ $parts;
            append @bind, @limit-bind;
        }

        return SQLStatement.new(:$sql, :values(@bind));
    }
}

# Multiple FROM sources NYI
multi method from(*%pairs where *.elems == 1) {
    my $pair = %pairs.head;
    my $alias = $pair.key;
    my $select = $pair.value.build;

    # XXX: teach SelectBuilder about this syntax instead
    my $from = SQLStatement.new(:sql("({$select.sql}) AS {quote-column($alias)}"), :values($select.values));
    return SelectBuilder.new(:$from);
}

multi method from(Str $table) {
    return SelectBuilder.new(:from(fragment(Identifier, $table)));
}


=begin pod

=head1 NAME

SQL::Cantrip - blah blah blah

=head1 SYNOPSIS

=begin code :lang<raku>

use SQL::Cantrip;

=end code

=head1 DESCRIPTION

SQL::Cantrip is ...

=head1 AUTHOR

Adrian Kreher <avuserow@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2022 Adrian Kreher

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
