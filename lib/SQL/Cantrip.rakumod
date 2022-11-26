unit class SQL::Cantrip;

my sub quote-value(Str $s) {
    my $q = $s.subst("'", "''");
    return "'$s'";
}

my sub quote-column(Str $s) {
    my $q = $s.subst('"', '""');
    return '"' ~ $s ~ '"';
}

# XXX: rename to SQLFragment?
class SQLStatement {
    has $.sql;
    has @.bind;
}

my role SQLSyntax {
    method build(--> SQLStatement) {...}
}

my sub fragment(SQLSyntax:U $default-class, $value) {
    $value.does(SQLSyntax) ?? $value !! $default-class.new($value);
}

# SQLSyntax representing a column or table name, quoted with the column rules (also works on tables)
class Identifier does SQLSyntax is export {
    has Str() $.relation;
    has Str() $.column;
    has Str() $.cast;

    constant relation-pattern = rx/^
        [$<relation>=(<-[.]>+) '.']? # optional table/relation name
        $<column>=(<-[:]>+) # required column name
        ['::' $<cast>=(.+)]? # optional postgres-style cast
    $/;

    method new(Str $string) {
        $string ~~ relation-pattern;
        self.bless(:$<relation>, :$<column>, :$<cast>);
    }

    method build {
        my $s = "";
        $s ~= "{quote-column($_)}." with $!relation;
        $s ~= quote-column($!column);
        $s ~= "::$_" with $!cast;
        SQLStatement.new(:sql($s));
    }
}

# SQLSyntax representing a literal value. Numbers and booleans are formatted as expected. Strings are escaped by string rules (double quotes).
class Value does SQLSyntax is export {
    method build {...}
}

# SQLSyntax representing a single placeholder. Always emits a '?' token
class Placeholder does SQLSyntax is export {
    has $.bind;

    method new($bind) {
        self.bless(:$bind);
    }

    method build {
        SQLStatement.new(:sql('?'), :$.bind);
    }
}


my sub append-fragments(Str $sql is rw, @bind, SQLSyntax:U $default-class, @items, Str :$join = '') {
    for @items.kv -> $i, $value {
        my $item = $value.does(SQLSyntax) ?? $value.build !! $default-class.new($value).build;
        $sql ~= $join unless $i == 0;
        $sql ~= $item.sql;
        append @bind, $item.bind;
    }
}
# SQLSyntax representing a raw value, unescaped in any way
class Raw does SQLSyntax is export {
    has $.value;
    has @.bind;

    method new($value, :@bind) {
        self.bless(:$value, :@bind);
    }

    method build {
        SQLStatement.new(:sql($!value), :@!bind);
    }
}

# SQLSyntax representing a function call. First argument is unescaped, all subsequent arguments are treated as columns (if not otherwise specified)
class Fn does SQLSyntax is export {
    has $.fn;
    has @.params;

    method new(Str $fn, *@params) {
        self.bless(:$fn, :@params);
    }

    method build {
        # XXX: should we render this at construction time? or later?
        my @bind;
        my $sql = $.fn ~ '(';
        append-fragments($sql, @bind, Identifier, @.params, :join(', '));
        $sql ~= ')';

        SQLStatement.new(:$sql, :@bind);
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

    method build {
        my @parts;
        my @bind;

        for @!clauses -> $clause {
            given $clause {
                when Pair {
                    my $key = fragment(Identifier, $clause.key).build;
                    my $value = fragment(Placeholder, $clause.value).build;

                    push @parts, "{$key.sql} = {$value.sql}";
                    append @bind, $key.bind;
                    append @bind, $value.bind;
                }
                when Positional {
                    die "wrong number of args!!" unless $clause.elems == 3;

                    my $col = fragment(Identifier, $clause[0]).build;
                    my $op = fragment(Raw, $clause[1]).build;
                    my $value = fragment(Placeholder, $clause[2]).build;

                    push @parts, "{$col.sql} {$op.sql} {$value.sql}";
                    append @bind, $col.bind;
                    append @bind, $op.bind;
                    append @bind, $value.bind;
                }
                when SQLSyntax {
                    die "SQLSyntax NYI!!";
                }
                default {
                    die "Don't know how to handle parameter of type {$clause.^name}: {$clause.raku}";
                }
            }
        }

        return SQLStatement.new(:sql(@parts.join(" {$.mode.uc} ")), :@bind);
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

        with $!from.?build {
            $sql ~= ' FROM ' ~ .sql;
            append @bind, .bind;
        }

        if $!where {
            with $!where.?build {
                $sql ~= ' WHERE ' ~ .sql;
                append @bind, .bind;
            }
        }

        if @!order-columns {
            $sql ~= ' ORDER BY ';
            append-fragments($sql, @bind, Identifier, @!order-columns, :join(', '));
        }

        if $!limit-count {
            with $!limit-count.?build {
                $sql ~= ' LIMIT ' ~ .sql;
                append @bind, .bind;
            }
        }

        return SQLStatement.new(:$sql, :@bind);
    }
}

# Multiple FROM sources NYI
multi method from(*%pairs where *.elems == 1) {
    my $pair = %pairs.head;
    my $alias = $pair.key;
    my $select = $pair.value.build;

    # XXX: teach SelectBuilder about this syntax instead
    my $from = Raw.new("({$select.sql}) AS {quote-column($alias)}", :bind($select.bind));
    return SelectBuilder.new(:$from);
}

multi method from(Str $table) {
    return SelectBuilder.new(:from(Identifier.new($table)));
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
