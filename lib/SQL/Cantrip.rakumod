unit class SQL::Cantrip;

my sub quote-value(Str $s) {
    my $q = $s.subst("'", "''");
    return "'$s'";
}

my sub quote-column(Str $s) {
    my $q = $s.subst('"', '""');
    return '"' ~ $s ~ '"';
}

class SQLFragment {
    has $.sql;
    has @.bind;
}

my role SQLSyntax {
    method build-fragment(--> SQLFragment) {...}
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
        ['::' $<cast>=(\S+)]? # optional postgres-style cast
    $/;

    method new(Str $string) {
        $string ~~ relation-pattern;
        self.bless(:$<relation>, :$<column>, :$<cast>);
    }

    method build-fragment {
        my $s = "";
        $s ~= "{quote-column($_)}." with $!relation;
        $s ~= quote-column($!column);
        $s ~= "::$_" with $!cast;
        SQLFragment.new(:sql($s));
    }
}

# SQLSyntax representing a literal value. Numbers and booleans are formatted as expected. Strings are escaped by string rules (double quotes).
class Value does SQLSyntax is export {
    has $.value;

    method new($value) {
        self.bless(:$value);
    }

    method build-fragment {
        SQLFragment.new(:sql(quote-value($!value)));
    }
}

# SQLSyntax representing a single placeholder. Always emits a '?' token
class Placeholder does SQLSyntax is export {
    has $.bind;

    method new($bind) {
        self.bless(:$bind);
    }

    method build-fragment {
        SQLFragment.new(:sql('?'), :$.bind);
    }
}

my sub append-fragments(Str $sql is rw, @bind, SQLSyntax:U $default-class, @items, Str :$join = '') {
    for @items.kv -> $i, $value {
        my $item = $value.does(SQLSyntax) ?? $value.build-fragment !! $default-class.new($value).build-fragment;
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

    method build-fragment {
        SQLFragment.new(:sql($!value), :@!bind);
    }

    method fmt($fmt, *@parts) {
        if any(@parts) !~~ SQLSyntax {
            die "Raw.fmt must receive only SQLSyntax as parameters";
        }

        my $pattern-count = $fmt.comb('{}').elems;
        if $pattern-count != @parts.elems {
            die "Raw.fmt: got $pattern-count replacement elements (\{\}), expected {@parts.elems}";
        }

        if $fmt ~~ /('{' <[^}]>+ '}')/ {
            die "Raw.fmt: found unsupported replacement element '{$/.Str}' (this syntax is reserved)";
        }

        my @bind;
        my $i = 0;
        my $value = $fmt.subst(:g, '{}', sub {
            my $sql = @parts[$i++].build-fragment;
            append @bind, $sql.bind;
            $sql.sql;
        });

        self.bless(:$value, :@bind);
    }
}

# SQLSyntax representing a function call. First argument is unescaped, all subsequent arguments are treated as columns (if not otherwise specified)
class Fn does SQLSyntax is export {
    has $.fn;
    has @.params;

    method new(Str $fn, *@params) {
        self.bless(:$fn, :@params);
    }

    method build-fragment {
        # XXX: should we render this at construction time? or later?
        my @bind;
        my $sql = $.fn ~ '(';
        append-fragments($sql, @bind, Identifier, @.params, :join(', '));
        $sql ~= ')';

        SQLFragment.new(:$sql, :@bind);
    }
}

class ConditionClause does SQLSyntax {
    has Str $.mode = 'none';
    has @.clauses;

    submethod BUILD(:$mode = 'none', :@clauses) {
        $!mode = $mode;
        @!clauses = @clauses[0] ~~ Positional|Pair ?? @clauses !! @clauses.item;

        if $mode eq 'none' && @!clauses > 1 {
            die "internal error: mode=none but multiple clauses";
        }
    }

    method build-fragment {
        my @parts;
        my @bind;

        for @!clauses -> $clause {
            given $clause {
                when Pair {
                    my $key = fragment(Identifier, $clause.key).build-fragment;

                    if $clause.value === Nil {
                        push @parts, "{$key.sql} IS NULL";
                        append @bind, $key.bind;
                    } else {
                        my $value = fragment(Placeholder, $clause.value).build-fragment;
                        push @parts, "{$key.sql} = {$value.sql}";
                        append @bind, $key.bind;
                        append @bind, $value.bind;
                    }
                }
                when Positional {
                    die "wrong number of args!!" unless $clause.elems == 3;

                    my $col = fragment(Identifier, $clause[0]).build-fragment;
                    my $op = fragment(Raw, $clause[1]).build-fragment;
                    my $value = fragment(Placeholder, $clause[2]).build-fragment;

                    push @parts, "{$col.sql} {$op.sql} {$value.sql}";
                    append @bind, $col.bind;
                    append @bind, $op.bind;
                    append @bind, $value.bind;
                }
                default {
                    die "Don't know how to handle parameter of type {$clause.^name}: {$clause.raku}";
                }
            }
        }

        return SQLFragment.new(:sql(@parts.join(" {$.mode.uc} ")), :@bind);
    }
}

my class Join does SQLSyntax {
    has $.mode;
    has $.table;
    has $.alias;
    has $.join-type;
    has ConditionClause $.on;
    has SQLSyntax $.using;

    submethod BUILD(:$mode, :$table, :@on, :$alias, :$using) {
        $!table = $table;
        $!mode = $mode;
        $!alias = $alias;

        # XXX: would be better to reuse the where clause logic somehow
        # most likely to push that logic into ConditionClause
        if @on {
            if @on[0] ~~ Pair && @on[0].key eq 'and'|'or' && @on[0].value ~~ Bool  {
                $!on = ConditionClause.new(:mode(@on[0].key), :clauses(@on[1..*]));
            } else {
                $!on = ConditionClause.new(:clauses(@on));
            }
        }

        if $using {
            $!using = fragment(Identifier, $using);
        }
    }

    method build-fragment {
        my @bind;
        my $sql = $!mode ?? "$!mode JOIN " !! 'JOIN ';

        with fragment(Identifier, $!table).build-fragment {
            $sql ~= .sql;
            append @bind, .bind;
        }

        if $!alias {
            $sql ~= " AS {quote-column($!alias)}";
        }

        if $!on {
            with $!on.build-fragment {
                $sql ~= ' ON ' ~ .sql;
                append @bind, .bind;
            }
        }

        if $!using {
            with $!using.build-fragment {
                $sql ~= " USING ({.sql})";
                append @bind, .bind;
            }
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}

class SelectBuilder does SQLSyntax {
    has @.select-columns;
    has SQLSyntax $.from;
    has Join @.join-items;
    has ConditionClause $.where;
    has @.order-columns;
    has @.group-by-columns;
    has SQLSyntax $.limit-count;

    method clone {
        nextwith (
            select-columns => @!select-columns.clone,
            from => $!from.clone,
            join-items => @!join-items.clone,
            where => $!where.clone,
            order-columns => @!order-columns.clone,
            group-by-columns => @!group-by-columns.clone,
            limit-count => $!limit-count.clone,
            |%_);
    }

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

    method group-by(*@columns) {
        @!group-by-columns = @columns;
        self;
    }

    # Don't use multi method here to avoid inheriting Cool.join
    method join($table, :@on, :$using, Str :$as, Bool :$inner, Bool :$left, Bool :$right, Bool :$full) {
        my @modes = (:$inner, :$left, :$right, :$full).grep({.value});
        if @modes > 1 {
            die "Multiple join types passed: must use only one of :inner :left :right :full (you used @modes[])";
        }

        unless one(@on, $using) {
            die "Must specify exactly one of :on or :using for join criteria";
        }

        my $mode = @modes[0].key.uc if @modes;

        push @!join-items, Join.new(:$mode, :$table, :@on, :$using, :alias($as));
        self;
    }

    method build-fragment {
        my $fragment = self.build;
        SQLFragment.new(:sql('(' ~ $fragment.sql ~ ')'), :bind($fragment.bind));
    }

    method build {
        die "no columns to select" unless @!select-columns;

        my @bind;
        my $sql = 'SELECT ';

        # Treat Pairs as alias clauses here only (e.g. SELECT column AS othername)
        my @cols = @!select-columns.map({
            when Pair {
                Raw.fmt('{} AS {}', fragment(Identifier, .value), fragment(Identifier, .key));
            }
            default {
                $_;
            }
        });
        append-fragments($sql, @bind, Identifier, @cols, :join(', '));

        with $!from.?build-fragment {
            $sql ~= ' FROM ' ~ .sql;
            append @bind, .bind;
        }

        if @!join-items {
            $sql ~= ' ';
            append-fragments($sql, @bind, Identifier, @!join-items, :join(' '));
        }

        if $!where {
            with $!where.?build-fragment {
                $sql ~= ' WHERE ' ~ .sql;
                append @bind, .bind;
            }
        }

        if @!group-by-columns {
            $sql ~= ' GROUP BY ';
            append-fragments($sql, @bind, Identifier, @!group-by-columns, :join(', '));
        }

        if @!order-columns {
            $sql ~= ' ORDER BY ';
            append-fragments($sql, @bind, Identifier, @!order-columns, :join(', '));
        }

        if $!limit-count {
            with $!limit-count.?build-fragment {
                $sql ~= ' LIMIT ' ~ .sql;
                append @bind, .bind;
            }
        }

        return SQLFragment.new(:$sql, :@bind);
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
