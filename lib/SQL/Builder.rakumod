unit class SQL::Builder;

my sub quote-value(Str $s) {
    my $q = $s.subst("'", "''");
    return qq!'$s'!;
}

my sub quote-column(Str $s) {
    my $q = $s.subst('"', '""');
    return qq!"$s"!;
}

class NoSinkError is Exception {
    has $.name = '(an unknown class)';

    method message() {
        "$.name() objects cannot be used in sink context - did you forget to use the result?";
    }
}

class SQLFragment {
    has $.sql;
    has @.bind;

    method sink() {
        NoSinkError.new(:name<SQLFragment>).throw;
    }
}

my role SQLSyntax {
    method build-fragment(--> SQLFragment) {...}
}

my role SQLStatement {
    method build(--> SQLFragment) {...}
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
        if @parts.grep({$_ !~~ SQLSyntax}) {
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

# aliased-column-list handles the Capture syntax used in the SELECT and RETURNING clauses to allow
# creation of column aliases.
sub aliased-column-list(@columns) {
    return @columns.map({
        when Pair {
            die 'Unexpected Pair as argument in column list';
        }
        when Capture {
            die 'unknown argument in column list, expected: \\($column-name, :as<alias>)' unless $_ ~~ :($, :$as!);
            Raw.fmt('{} AS {}', fragment(Identifier, .[0]), fragment(Identifier, .<as>));
        }
        default {
            $_;
        }
    });
}

# order-by-column-list handles the Capture syntax used in the ORDER BY clause to implement options
# for ordering (e.g. DESC, ASC)
sub order-by-column-list(@columns) {
    return @columns.map({
        when Capture {
            die 'unknown argument in order-by list, expected: \\($column-name, :desc|:asc)' unless $_ ~~ :($, :$desc) || $_ ~~ :($, :$asc);
            Raw.fmt('{} {}', fragment(Identifier, .[0]), fragment(Raw, .Hash.keys[0].uc));
        }
        default {
            $_;
        }
    });
}

class ConditionClause does SQLSyntax {
    has Str $.mode = 'none';
    has @.clauses;

    method new(+@c, *%k) {
        if @c.elems == 3 && @c[0] !~~ Positional|Pair|Capture {
            # If this is a simple ["foo", "=", "bar"] clause, it's already been flattened enough
            self.bless(:mode<and>, :clauses[@c, |%k.pairs.sort]);
        } else {
            self.bless(:mode<and>, :clauses[|@c, |%k.pairs.sort]);
        }
    }

    method new-or(+@c, *%k) {
        if @c.elems == 3 && @c[0] !~~ Positional|Pair|Capture {
            # If this is a simple ["foo", "=", "bar"] clause, it's already been flattened enough
            self.bless(:mode<or>, :clauses[@c, |%k.pairs.sort]);
        } else {
            self.bless(:mode<or>, :clauses[|@c, |%k.pairs.sort]);
        }
    }

    submethod BUILD(:$mode = 'none', :$clauses) {
        $!mode = $mode;
        @!clauses = $clauses.list;

        if $mode eq 'none' && @!clauses > 1 {
            die "where clause has multiple clauses, but no mode (e.g. 'and' or 'or')";
        }
    }

    method !make-pair(Pair $clause, @parts, @bind) {
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

    method build-fragment {
        my @parts;
        my @bind;

        for @!clauses -> $clause {
            given $clause {
                when Pair {
                    self!make-pair($clause, @parts, @bind);
                }
                when $_ ~~ Positional && .elems == 1 && .[0] ~~ Pair {
                    self!make-pair($clause[0], @parts, @bind);
                }
                when Capture {
                    die "unexpected arguments (expected only one of :and/:or): {$clause.raku}" if $clause.elems;
                    my $pairs = $clause.pairs;
                    die "unexpected arguments (expected only one of :and/:or): {$clause.raku}" if $pairs.elems != 1;
                    my $pair = $pairs.head;
                    my $key = $pair.key;

                    if $key eq 'or' {
                        my $inner = ConditionClause.new-or($pair.value).build-fragment;
                        push @parts, '(' ~ $inner.sql ~ ')';
                        append @bind, $inner.bind;
                    } elsif $key eq 'and' {
                        my $inner = ConditionClause.new($pair.value).build-fragment;
                        push @parts, '(' ~ $inner.sql ~ ')';
                        append @bind, $inner.bind;
                    } else {
                        die "unknown key (expected 'and' or 'or'): $key";
                    }
                }
                when Positional {
                    die "wrong number of args (needed 3): {$clause.raku}" unless $clause.elems == 3;

                    my $col = fragment(Identifier, $clause[0]).build-fragment;
                    my $op = fragment(Raw, $clause[1]).build-fragment;
                    my $value = fragment(Placeholder, $clause[2]).build-fragment;

                    push @parts, "{$col.sql} {$op.sql} {$value.sql}";
                    append @bind, $col.bind;
                    append @bind, $op.bind;
                    append @bind, $value.bind;
                }
                when Hash {
                    die "Hash not implemented - did you mean a List of Pairs?";
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

    submethod BUILD(:$mode, :$table, :$on, :$alias, :$using) {
        $!table = $table;
        $!mode = $mode;
        $!alias = $alias;

        if $using {
            $!using = fragment(Identifier, $using);
        } elsif $on ~~ Capture {
            $!on = ConditionClause.new($on);
        } else {
            $!on = ConditionClause.new($on.List);
        }

    }

    method build-fragment {
        my @bind;
        my $sql = $!mode ?? "$!mode JOIN " !! 'JOIN ';

        given fragment(Identifier, $!table).build-fragment {
            $sql ~= .sql;
            append @bind, .bind;
        }

        if $!alias {
            $sql ~= " AS {quote-column($!alias)}";
        }

        if $!on {
            given $!on.build-fragment {
                $sql ~= ' ON ' ~ .sql;
                append @bind, .bind;
            }
        }

        if $!using {
            given $!using.build-fragment {
                $sql ~= " USING ({.sql})";
                append @bind, .bind;
            }
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}

class SelectBuilder does SQLSyntax does SQLStatement {
    has @.select-columns;
    has SQLSyntax $.from;
    has Join @.join-items;
    has ConditionClause $.where;
    has @.order-by-columns;
    has @.group-by-columns;
    has ConditionClause $.having;
    has SQLSyntax $.limit-count;
    has SQLSyntax $.offset-count;

    proto method new(|) {*}

    # Multiple FROM sources NYI
    multi method new(SQLStatement $inner, :$as!) {
        my $from = Raw.fmt('{} AS {}', $inner, fragment(Identifier, $as));
        return self.bless(:$from);
    }

    multi method new(*%pairs where *.elems == 1) {
        die 'Unsupported use of Pair for query aliasing';
    }

    multi method new(Str $table) {
        return self.bless(:from(Identifier.new($table)));
    }

    multi method new(Identifier $table) {
        return self.bless(:from($table));
    }

    method clone {
        nextwith :select-columns(@!select-columns.clone),
            :from($!from.clone),
            :join(@!join-items.clone),
            :where($!where.clone),
            :order-by-columns(@!order-by-columns.clone),
            :group-by-columns(@!group-by-columns.clone),
            :having($!having.clone),
            |(:limit-count(.clone) with $!limit-count),
            |(:offset-count(.clone) with $!offset-count),
            |%_;
    }

    method where(|c) {
        $!where = ConditionClause.new(|c);
        self;
    }

    method where2(|c) {
        $!where = ConditionClause.new(|c);
        self;
    }

    method having(|c) {
        $!having = ConditionClause.new(|c);
        self;
    }

    # XXX: replace with either `select-all` or `select(Whatever)`?
    multi method select(:$all!) {
        @!select-columns = Raw.new('*');
        self;
    }

    # No named parameters permitted
    multi method select(*@c, *% where .elems == 0) {
        @!select-columns = |@c;
        self;
    }

    # multi method select(*@columns, *%pairs) {
    #     @!select-columns = |@columns, |%pairs.sort;
    #     self;
    # }

    method order-by(*@columns) {
        @!order-by-columns = @columns;
        self;
    }

    method limit(Int $limit) {
        $!limit-count = Placeholder.new($limit);
        self;
    }

    method offset(Int $offset) {
        $!offset-count = Placeholder.new($offset);
        self;
    }

    method group-by(*@columns) {
        @!group-by-columns = @columns;
        self;
    }

    # Don't use multi method here to avoid inheriting Cool.join
    # TODO: add proto method to let us use multi methods without calling Cool.join
    method join($table, :$on, :$using, Str :$as, Bool :$inner, Bool :$left, Bool :$right, Bool :$full) {
        my @modes = (:$inner, :$left, :$right, :$full).grep({.value});
        if @modes > 1 {
            die "Multiple join types passed: must use only one of :inner :left :right :full (you used @modes[])";
        }

        unless one($on, $using) {
            die "Must specify exactly one of :on or :using for join criteria";
        }

        my $mode = @modes[0].key.uc if @modes;

        push @!join-items, Join.new(:$mode, :$table, :$on, :$using, :alias($as));
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

        append-fragments($sql, @bind, Identifier, aliased-column-list(@!select-columns), :join(', '));

        given $!from.build-fragment {
            $sql ~= ' FROM ' ~ .sql;
            append @bind, .bind;
        }

        if @!join-items {
            $sql ~= ' ';
            append-fragments($sql, @bind, Identifier, @!join-items, :join(' '));
        }

        if $!where {
            given $!where.build-fragment {
                $sql ~= ' WHERE ' ~ .sql;
                append @bind, .bind;
            }
        }

        if @!group-by-columns {
            $sql ~= ' GROUP BY ';
            append-fragments($sql, @bind, Identifier, @!group-by-columns, :join(', '));
        }

        if $!having {
            given $!having.build-fragment {
                $sql ~= ' HAVING ' ~ .sql;
                append @bind, .bind;
            }
        }

        if @!order-by-columns {
            $sql ~= ' ORDER BY ';
            append-fragments($sql, @bind, Identifier, order-by-column-list(@!order-by-columns), :join(', '));
        }

        if $!limit-count {
            given $!limit-count.build-fragment {
                $sql ~= ' LIMIT ' ~ .sql;
                append @bind, .bind;
            }
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}

# TODO: implement table/column helpers
# class Condition {
#     has $.left;
#     has $.op;
#     has $.right;
# }
# class Column is Identifier {
#     method EQ($other) {
#         if $other.defined {
#             return Condition.new(:left(self), :op('='), :right(fragment(Placeholder, $other)));
#         } else {
#             return Condition.new(:left(self), :op('IS'), :right(Raw.new('NULL')));
#         }
#     }
# 
#     method NE($other) {
#         if $other.defined {
#             return Condition.new(:left(self), :op('!='), :right(fragment(Placeholder, $other)));
#         } else {
#             return Condition.new(:left(self), :op('IS NOT'), :right(Raw.new('NULL')));
#         }
#     }
# 
#     method GT($other) {
#         Condition.new(:left(self), :op('>'), :right(fragment(Placeholder, $other)));
#     }
# }
# 
# class ColumnSpec is export {
#     has %!columns is built;
# 
#     method new($table-name, :@columns) {
#         my %columns = @columns.map({$_ => Column.new($table-name ~ '.' ~ $_)});
#         self.bless(:%columns);
#     }
# 
#     method FALLBACK($name) {
#         if %!columns{$name}:exists {
#             return %!columns{$name};
#         }
# 
#         die "no such column: '$name'";
#
#     }
# }

method from(|c) {
    SelectBuilder.new(|c);
}

class UpdateBuilder does SQLSyntax {
    has SQLSyntax $.table;
    has @!values;
    has @!columns;
    has $!query;
    has @!returning;
    has ConditionClause $.where;

    proto method new(|) {*}
    multi method new(Str $table) {
        return self.bless(:table(Identifier.new($table)));
    }

    multi method new(Identifier $table) {
        return self.bless(:$table);
    }

    method set(*@values, *%values) {
        @!values = @values;
        append @!values, %values.sort;
        self;
    }

    method returning(*@columns, *%pairs) {
        @!returning = |@columns, |%pairs.pairs.sort;
        self;
    }

    method where(|c) {
        $!where = ConditionClause.new(|c);
        self;
    }

    method build-fragment {
        my $fragment = self.build;
        SQLFragment.new(:sql('(' ~ $fragment.sql ~ ')'), :bind($fragment.bind));
    }

    method build {
        die "refusing to UPDATE without a WHERE clause" unless $!where;

        my @bind;
        my $sql = 'UPDATE ';

        given $!table.build-fragment {
            $sql ~= .sql;
            append @bind, .bind;
        }

        $sql ~= ' SET ';

        for @!values.kv -> $i, $pair {
            my $col = fragment(Identifier, $pair.key).build-fragment;
            my $val = fragment(Placeholder, $pair.value).build-fragment;

            $sql ~= ', ' if $i > 0;
            $sql ~= "{$col.sql} = {$val.sql}";
            append @bind, $col.bind;
            append @bind, $val.bind;
        }

        given $!where.?build-fragment {
            $sql ~= ' WHERE ' ~ .sql;
            append @bind, .bind;
        }

        if @!returning {
            $sql ~= ' RETURNING ';
            append-fragments($sql, @bind, Identifier, aliased-column-list(@!returning), :join(', '));
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}

method update(|c) {
    UpdateBuilder.new(|c);
}

class InsertBuilder does SQLSyntax does SQLStatement {
    has SQLSyntax $.table;
    has @!values;
    has @!columns;
    has $!query;
    has @!returning;
    # has ... $.on-conflict;

    proto method new(|) {*}
    multi method new(Str $table) {
        return self.bless(:table(Identifier.new($table)));
    }

    multi method new(Identifier $table) {
        return self.bless(:$table);
    }

    method returning(*@columns, *%pairs) {
        @!returning = |@columns, |%pairs.pairs.sort;
        self;
    }

    method data(*@values, *%values) {
        my @data = |@values, |%values.sort;
        die "all arguments passed to 'data' must be Pairs" if @data.first({$_ !~~ Pair}, :k);
        @!columns = @data.map(*.key);
        @!values = [@data.map(*.value),];
        $!query = Any;
        self;
    }

    method columns(*@columns) {
        @!columns = @columns;
        self;
    }

    method values(*@values) {
        $!query = Any;
        @!values = [@values,];
        self;
    }

    method multi-values(@multi-values) {
        die "argument to 'multi-values' must be list of lists" unless @multi-values.all ~~ Positional;
        $!query = Any;
        @!values = @multi-values;
        self;
    }

    method query(SQLStatement $query) {
        # XXX: should we validate that this query is either a SELECT statement,
        # or has a RETURNING clause somehow?
        $!query = $query;
        @!values = ();
        self;
    }

    method build-fragment {
        my $fragment = self.build;
        SQLFragment.new(:sql('(' ~ $fragment.sql ~ ')'), :bind($fragment.bind));
    }

    method build {
        my @bind;
        my $sql = 'INSERT INTO ';

        given $!table.build-fragment {
            $sql ~= .sql;
            append @bind, .bind;
        }

        if @!values {
            $sql ~= ' (';
            append-fragments($sql, @bind, Identifier, @!columns, :join(', '));
            $sql ~= ') VALUES ';

            for @!values.kv -> $i, $v {
                $sql ~= ', ' if $i != 0;
                $sql ~= '(';
                append-fragments($sql, @bind, Placeholder, $v, :join(', '));
                $sql ~= ')';
            }
        } elsif @!columns && $!query {
            $sql ~= ' (';
            append-fragments($sql, @bind, Identifier, @!columns, :join(', '));
            $sql ~= ') ';

            given $!query.build {
                $sql ~= .sql;
                append @bind, .bind;
            }
        } else {
            die "cannot INSERT without values (or a query)";
        }

        if @!returning {
            $sql ~= ' RETURNING ';
            append-fragments($sql, @bind, Identifier, aliased-column-list(@!returning), :join(', '));
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}

method insert-into(|c) {
    InsertBuilder.new(|c);
}

class DeleteBuilder does SQLSyntax does SQLStatement {
    has SQLSyntax $.table;
    has @.returning;
    has ConditionClause $.where;

    proto method new(|) {*}
    multi method new(Str $table) {
        return self.bless(:table(Identifier.new($table)));
    }

    multi method new(Identifier $table) {
        return self.bless(:$table);
    }

    method returning(*@columns, *%pairs) {
        @!returning = |@columns, |%pairs.pairs.sort;
        self;
    }

    method where(|c) {
        $!where = ConditionClause.new(|c);
        self;
    }

    method build-fragment {
        my $fragment = self.build;
        SQLFragment.new(:sql('(' ~ $fragment.sql ~ ')'), :bind($fragment.bind));
    }

    method build {
        die "refusing to DELETE without a WHERE clause" unless $!where;

        my @bind;
        my $sql = 'DELETE FROM ';

        given $!table.build-fragment {
            $sql ~= .sql;
            append @bind, .bind;
        }

        given $!where.build-fragment {
            $sql ~= ' WHERE ' ~ .sql;
            append @bind, .bind;
        }

        if @!returning {
            $sql ~= ' RETURNING ';
            append-fragments($sql, @bind, Identifier, aliased-column-list(@!returning), :join(', '));
        }

        return SQLFragment.new(:$sql, :@bind);
    }
}


method delete-from(|c) {
    DeleteBuilder.new(|c);
}

method fn {
    return Fn;
}

method raw {
    return Raw;
}

method placeholder {
    return Placeholder;
}

=begin pod

=head1 NAME

SQL::Builder - build SQL statements

=head1 SYNOPSIS

=begin code :lang<raku>
use SQL::Builder;

my $sql = SQL::Builder.new;

# all SELECT queries start with from
my $q1 = $sql.from('users').select(<name address>).where(['email', '=', 'foo@example.com']);
my $statement = $q1.build;
# $statement.sql: SELECT name, address FROM users WHERE email = ?
# $db.execute($statement.sql, |$statement.bind);

# many SQL fragments are supported:
my $q2 = $sql.from('songs').
    select(['album', :albumlength(Fn.new('SUM', 'length'))]).
    where([:online(True), ['year', '>=', 2020]]).
    group-by('album').
    order-by('album');

# subselects too:
my $q3 = $sql.from($q2, :as<inner>).select(Fn.new('MAX', 'inner.albumlength'));

# joins:
my $q4 = $sql.from('songs').
    join(:left, 'ratings', :using('song-id')).
    select(<title artist album rating>);
=end code

=head1 DESCRIPTION

SQL::Builder generates SQL statements using an approach modeled after the 'builder' pattern. This
allows us to support a wide variety of SQL syntax, including sub-queries, while allowing each option
to remain intuitive.

This module is partially inspired by Python's SQLAlchemy Core as well as Perl 5's C<SQL::Abstract>.
This module forgoes an ORM in favor of generating raw SQL, giving you the power to build complex
queries and not lose execution speed.

This module also prioritizes safety. Each option has rules on how it handles an untyped string, and
it defaults to either a quoted C<Identifier> or a C<Placeholder>, depending on the ergonomics of
each function.

SQL::Builder can represent a large amount of SQL statements, and is best suited for statements of
medium complexity with reasonably high amounts of runtime changes (e.g. where clause, optional joins
or sub-selects).

=head1 TYPE SYSTEM

SQL::Builder has a basic type system to ensure the right behavior when a value is provided. Each
builder option has a default way to handle a string, typically either as either Identifier or
Placeholder and each method documents its expectations.

These are all subclasses of C<SQLSyntax> and are:

=item C<Identifier>: used for table and column names, and will be split into pieces and quoted. See C<Identifier> below for full details.

=item C<Placeholder>: always generates a single C<?> placeholder and puts its value into the list of bind values.

=item C<Value>: used for a string value when C<Placeholder> is not an option. Always fully quoted.

=item C<Raw>: this is a raw fragment of SQL. Use C<Raw.fmt> to safely write raw fragments.

=item C<Fn>: used to help write function calls. The first argument is unquoted. Subsequent arguments default to C<Identifier>, and are put in a parenthesized list.

=head2 Identifier

The C<Identifier> type is the most common way to handle untyped input. It is used to represent
tables and columns, with an optional type cast. If a dotted value is specified (e.g.
C<table.column>), then this is split into two bits before quoting. Additionally, if it ends with
C<::>, then that suffix is allowed as a PostgreSQL-style type cast.

Example input and output for the SQL portion:

=item C<Identifier.new("foo")> -> C<"foo">
=item C<Identifier.new("foo.bar")> -> C<"foo"."bar">
=item C<Identifier.new("foo::int")> -> C<"foo"::int>
=item C<Identifier.new("foo.bar::uuid[]")> -> C<"foo"."bar"::uuid[]>

=head2 Placeholder

This is a simple SQL placeholder. It is used as in the right-hand side of various conditionals. The resulting fragment is always a single C<?>, and the value is put in the bind list.

No conversion is done to the value, so you can pass in values of type C<Str>, C<Int>, C<DateTime>, C<Bool>, as well as C<Array>. Currently, Arrays do not have any special logic to create additional placeholders or otherwise flatten the array. See the Cookbook section to see how to handle this.

=head2 Value

The value is escaped and wrapped in single quotes.

=head2 Raw

No escaping is performed. The C<Raw.fmt> method provides a safer way to use this.

=head2 Raw.fmt(Str $template, SQLSyntax $a, $b, ...)

The C<fmt> method lets you safely build bits of SQL by providing a template containing C<{}>
sequences that are replaced by the following arguments. You must have the same number of C<{}>
replacements as arguments, and all arguments must be one of the types in this section (e.g.
subclasses of C<SQLSyntax>).

Examples:

=begin code :lang<raku>
Raw.fmt('COUNT({}) AS {}', Identifier.new("artist"), Identifier.new("artistcount"));
# sql: COUNT("artist") AS "artistcount"
# bind: []

Raw.fmt('unnest({}::uuid[]) WITH ORDINALITY t(id, ord)', Placeholder.new([1, 2, 3]));
# sql: unnest(?::uuid[]) WITH ORDINALITY t(id, ord)
# bind: [@ids,]

Raw.fmt('date_trunc({}, {})', Value.new('day'), Identifier.new('song-start'));
# sql: date_trunc('day', "song-start")
# bind: []
=end code

=head2 Fn

Fn (function) is a helper to make function calls. The first item is taken as a Raw value, and all
following items default to Identifiers. This can be used for any function-like syntax in SQL, not
just real functions.

Examples:

=begin code :lang<raku>
Fn.new('COUNT', 'artists');
# sql: COUNT("artists")
# bind: []

Fn.new('ANY', my @stuff);
# sql: ANY(?)
# bind: [@my-stuff,]
=end code

=head1 SELECT QUERIES

Select queries are created with the C<from> method on the C<SQL::Builder> object. All other options
can be passed in any order. All options except C<join> overwrite the current value. Each option
returns the SelectBuilder instance, allowing for a chain style:
C<$sql.from('foo').select('bar').group-by('baz')>

Multiple tables in the C<from> clause are not yet supported. Use C<join> instead.

Select queries support the following options:

=head2 from(Str $table)

Creates a C<SelectBuilder> from the given table.

=head2 from(SQLStatement, :$as!)

Creates a subselect from the provided SQLStatement, aliased to the value of C<$as>. Contrived example:

=begin code :lang<raku>
my $inner-q = $sql.from('foo').select('bar');
my $q = $sql.from($inner-q, :as<inner>).select('bar');
# SELECT "bar" FROM (SELECT "bar" FROM "foo") AS "inner"
=end code

=head2 select(:all)

Emits a C<SELECT *>.

=head2 select(*@columns)

Specifies the list of values to return. Each column defaults to C<Identifier>.

A column may be aliased with the use of a Capture in the form C<\("column-name", :as<alias>)>.

=begin code :lang<raku>
# Preferred form: using a capture explicitly:
$sql.from('table').select(\("bar", :as<foo>));
# sql: SELECT "bar" AS "foo" FROM "table"
=end code

Note that due to Raku's handling of Pairs, if you mix Positional and non-Positional arguments, the
Pairs will always be at the end. You can avoid this by passing an Array, or parenthesizing the
Pairs:

=begin code :lang<raku>
$sql.from('table').select(<foo bar>, :a<b>, :c<d>);
# SELECT "foo", "bar", "b" AS "a", "d" AS "c" FROM table

$sql.from('table').select(:a<b>, <foo bar>, :c<d>);
# SELECT "foo", "bar", "b" AS "a", "d" AS "c" FROM table

# Instead, pass all values as positional elements, in any of the following ways:
$sql.from('table').select([:a<b>, "foo", "bar", :c<d>]);
$sql.from('table').select([:a<b>, <foo bar>.flat, :c<d>]);
$sql.from('table').select((:a<b>), "foo", "bar", (:c<d>));
# SELECT "b" AS "a", "foo", "bar", "d" AS "c" FROM table
=end code

=head2 where(@where)

Provide a C<WHERE> clause with one or more values. These clauses are combined with C<AND> logic by
default. The values are used as a C<ConditionClause>, see the documentation below for the details.

=begin code :lang<raku>
$sql.from('users').select('email').where(:username<ak>);
# sql: SELECT "email" FROM "users" WHERE "username" = ?
# bind: ["ak",]

$sql.from('users').select('email').where(["username", "=", "ak"]);
# sql: SELECT "email" FROM "users" WHERE "username" = ?
# bind: ["ak",]

$sql.from('users').select('email').where([["email", "LIKE", "%gmail.com"], ["email", "LIKE", "ak%"]]);
# sql: SELECT "email" FROM "users" WHERE "email" LIKE ? AND "email" LIKE ?
# bind: ["%gmail.com", "ak%"]

# Use a Capture to make a sub-group to switch to OR logic:
$sql.from('users').select('email').where(\(:or[["email", "LIKE", "%gmail.com"], ["email", "LIKE", "%googlemail.com"]]));
# sql: SELECT "email" FROM "users" WHERE ("email" LIKE ? OR "email" LIKE ?)
# bind: ["%gmail.com", "%googlemail.com"]
=end code

=head2 join($table, :@on, :$using, Str :$as, :$inner/:$left/:$right/:$full)

Provides a C<JOIN> clause to this query. C<$table> should be either the name of a table (treated as
an Identifier), or a C<SelectBuilder> to use a sub-query.

Exactly one of C<:$using> or C<:@on> must be specified, which determines how to join the table.
C<:$using> is for when the columns match exactly. C<:@on> is passed to C<ConditionClause> and lets
you fully control the join logic. See the C<ConditionClause> documentation below. These correspond
to the C<USING(column)> and C<ON expr> portions of the SQL join expression, respectively.

The C<:$as> parameter is optional which controls the join expression's alias. This is needed if you
use a sub-query or the same table is joined multiple times. This is treated as an Identifier.

Finally, one of C<:$inner/:$left/:$right/:$full> may be specified to control the type of JOIN. This
is optional.

Multiple JOINs are supported and are processed in the order they are added.

There is currently no way to clear the list of JOINs from a query.

Examples:

=begin code :lang<raku>
$sql.from('t1').join('t2', :using<id>).select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t1" JOIN "t2" USING("id")

my $inner = $sql.from('t1').select(:bar<foo>, 'id');
$sql.from('t2').
    join(:left, $inner, :as<inner>, :on['inner.id', '=', Identifier.new('t2.id')]).
    select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t2" LEFT JOIN (SELECT "id", "foo" AS "bar" FROM "t1") AS "inner" ON "inner"."id" = "t2"."id"

$sql.from('t1').
    join('t2', :on(["t1.id", "=", Identifier.new("t2.id")], ["t1.foo", "<", "t2.foo"])).
    select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t1" JOIN "t2" ON "t1.id" = "t2.id" AND "t1"."foo" < "t2"."foo"
=end code

=head2 limit(Int $n)

Provides a C<LIMIT> clause (with the specified value as a placeholder):

=begin code :lang<raku>
$sql.from('table').select(<foo bar>).limit(1);
# sql: SELECT "foo", "bar" FROM "table" LIMIT ?
# bind: 1
=end code

=head2 offset(Int $n)

Provides a C<OFFSET> clause (with the specified value as a placeholder):

=begin code :lang<raku>
$sql.from('table').select(<foo bar>).limit(1).offset(2);
# sql: SELECT "foo", "bar" FROM "table" LIMIT ? OFFSET ?
# bind: [1, 2]
=end code

=head2 group-by(*@columns)

Provides a C<GROUP BY> clause on the specified columns:

=begin code :lang<raku>
$sql.from('songs').select(Fn.new('SUM', 'length'), 'artist', 'year').group-by('artist', 'year');
# SELECT SUM("length"), "artist", "year" FROM songs GROUP BY "artist", "year"
=end code

=head2 having(@having)

Provides a C<HAVING> clause. This is handled identical to a C<WHERE> clause, see the documentation above.

=head2 order-by(*@columns)

Provides an C<ORDER BY> clause on the specified columns. Each value is interpreted as an C<Identifier>, though you may specify another subclass of C<SQLSyntax> to use an expression instead:

=begin code :lang<raku>
# pick 10 shortest shortest songs
$sql.from('songs').select('title').order-by('length').limit(10);
# sql: SELECT "title" FROM "songs" ORDER BY "length" limit ?
# bind: 10

# pick 10 random items
$sql.from('songs').select('title').order-by(Fn.new('RANDOM')).limit(10);
# sql: SELECT "title" FROM "songs" ORDER BY RANDOM() limit ?
# bind: 10
=end code

A C<Capture> may be specified to use descending order, by providing the column name (or expression) and C<:desc>. C<:asc> is also supported if you wish to be explicit about ascending order.

=begin code :lang<raku>
# pick 10 longest songs
$sql.from('songs').select('title').order-by(\("length", :desc)).limit(10);
# sql: SELECT "title" FROM "songs" ORDER BY "length" DESC limit ?
# bind: 10
=end code

=head2 build()

Converts this C<SelectBuilder> into an C<SQLFragment> that has C<.sql> and C<.bind> methods. This
method may be called more than once, but it is not recommended. The SQL and bind values may not
appear in the same order across invocations.

=head2 clone()

Returns a new C<SelectBuilder> in the same state. Useful if you want to have a common set of
options, and then use many times. (Note that if you do not clone, then the original builder would be
modified, which may not be what you want.)

=begin code :lang<raku>
sub getuser {
    state $q = $sql.from('users').select(<username email address>);
    $q.clone;
}

# multiple times later:
my $username = "whoever";
my $st2 = getuser().where(:$username).build;
# $db.query($st2.sql, |$st2.bind);
=end code

=head1 INSERT QUERIES

Insert queries are created with the C<insert-into> method on the C<SQL::Builder> object. All other
options can be passed in any order. All options overwrite the current value. Each option returns the
InsertBuilder instance, allowing for a chain style:

=begin code :lang<raku>
$sql.insert-into("table").values([:a(1), :b(2)])
# INSERT INTO "table" ("a", "b") VALUES (?, ?)
=end code

The Insert query requires some values to insert. You may specify this with the C<values> clause to
provide values verbatim, or the combination of C<columns> and C<query> to get values from a subquery
(typically a sub-select).

=head2 new(Str $table)

Creates a C<InsertBuilder> for the given table.

=head2 values(@values)

Set the data to be inserted. The data is a List of Pairs, where the key is the column name (as an
C<Identifier>), and the value is the value (interpreted as a C<Placeholder>). This may be passed in
a variety of styles, all equivalent:

=begin code :lang<raku>
$sql.insert-into("table").values([:a(1), :b(2)])
$sql.insert-into("table").values(:a(1), :b(2))
$sql.insert-into("table").values([:a(1)], :b(2))
# sql: INSERT INTO "table" ("a", "b") VALUES (?, ?)
# bind: [1, 2]
=end code

This only supports inserting a single row.

=head2 columns(@columns)

Sets the columns to insert, as a List of columns (interpreted as C<Identifier>s). Used in
combination with C<query>, see below for an example.

=head2 query(SQLStatement $query)

Provides the values to insert from the result of a query. This query is typically a Select statement
but may be other queries depending on database support.

=begin code :lang<raku>
my $inner = $sql.from("t1").select("a", "b").order-by("a").limit(1);
$sql.insert-into("t2").columns("c", "d").query($inner);
# INSERT INTO "t2" ("c", "d") SELECT "a", "b" FROM "t1" ORDER BY "a" LIMIT 1
=end code

=head2 returning(@columns)

Provides a C<RETURNING> clause, with list of columns (or other expressions) to return. This works
identically to the C<select> clause of a Select query, see that documentation above.

=begin code :lang<raku>
$sql.insert-into("table").values(:a(1), :b(2), :c(3)).returning("b", Fn.new("LOWER", "c"))
# INSERT INTO "table" WHERE "a" = ? RETURNING "b", LOWER("c")
=end code

=head UPDATE QUERIES

Update queries are created with the C<update> method on the C<SQL::Builder> object. All other
options can be passed in any order. All options overwrite the current value. Each option returns the
UpdateBuilder instance, allowing for a chain style:

=begin code :lang<raku>
$sql.update("table").set(:a(1)).where(["b", "=", 2])
# UPDATE "table" SET a = ? WHERE "a" = ?
=end code

=head2 new(Str $table)

Creates a C<UpdateBuilder> for the given table.

=head2 set(@values)

Set the data to be updated. The data is a List of Pairs, where the key is the column name (as an
C<Identifier>), and the value is the value (interpreted as a C<Placeholder>). This may be passed in
a variety of styles, all equivalent:

=begin code :lang<raku>
$sql.update("table").set([:a(1), :b(2)]).where(:c(3))
$sql.update("table").set(:a(1), :b(2)).where(:c(3))
$sql.update("table").set([:a(1)], :b(2)).where(:c(3))
# sql: UPDATE "table" SET "a" = ?, "b" = ? WHERE "c" = ?
# bind: [1, 2, 3]
=end code

If you want to provide an expression for a column, use a C<Fn> or C<Raw> value:

=begin code :lang<raku>
my $fn = Fn.new("MAX", "a", "d");
my $expr = Raw.fmt('{} + {}', Identifier.new("c"), Placeholder.new(1234));
$sql.update("table").set([:a($fn), :b($expr)]).where(:c(3))
# sql: UPDATE "table" SET "a" = MAX("a", "d"), "b" = "c" + ? WHERE "c" = ?
# bind: [1234, 3]
=end code

=head2 where($where) / where(@where)

Provides a C<WHERE> clause with one or more values. This works identically to C<where> for Select
queries, and is used to make a ConditionClause. See documentation for C<where> above, and on
C<ConditionClause> below.

Unlike Select queries, this is required, even if you want to update all rows in a table.

=head2 returning(@columns)

Provides a C<RETURNING> clause, with list of columns (or other expressions) to return. This works
identically to the C<select> clause of a Select query, see that documentation above.

=head1 DELETE QUERIES

Delete queries are created with the C<delete-from> method on the C<SQL::Builder> object. All other
options can be passed in any order. All options overwrite the current value. Each option returns the
DeleteBuilder instance, allowing for a chain style:

=begin code :lang<raku>
$sql.delete-from("table").where(["a", "=", 1])
# DELETE FROM "table" WHERE "a" = ?
=end code

=head2 new(Str $table)

Creates a C<DeleteBuilder> for the given table.

=head2 where($where) / where(@where)

Provides a C<WHERE> clause with one or more values. This works identically to C<where> for Select
queries, and is used to make a ConditionClause. See documentation for C<where> above, and on
C<ConditionClause> below.

Unlike Select queries, this is required, even if you want to delete all rows in a table.

=head2 returning(@columns)

Provides a C<RETURNING> clause, with list of columns (or other expressions) to return. This works
identically to the C<select> clause of a Select query, see that documentation above.

=begin code :lang<raku>
$sql.delete-from("table").where(["a", "=", 1]).returning("b", Fn.new("LOWER", "c"))
# DELETE FROM "table" WHERE "a" = ? RETURNING "b", LOWER("c")
=end code

=head1 ConditionClause

The C<ConditionClause> syntax is how C<SQL::Builder> encodes the options of the
C<WHERE> clause, the C<HAVING> clause, and the C<JOIN ... ON> clause.

Multiple items are combined with C<AND> logic. To use C<OR> logic, use a
sub-group (Capture), documented below.

At its core, this syntax is a list of items. Each item can be one of three values:

=head2 3-Valued List

The main item is a list of exactly three items, typically a column (C<Identifier>), an operator (C<Raw>), and a C<Placeholder> value.

=begin code :lang<raku>
$q.where(["a", "=", 1]);
# sql: ... WHERE a = ?
# bind: [1,]
=end code

Any of these three can be replaced by an explicit type from the "Type System" section above:

=begin code :lang<raku>
$q.where([Fn.new("lower", "a"), "=", Fn.new("lower", "b")]);
# sql: ... WHERE lower("a") = lower("b")

$q.where(["b", ">=", Raw.fmt('{} / 2.0', Placeholder.new(3))]);
# sql: ... WHERE "b" >= ? / 2.0
# bind: [3,]
=end code

=head2 Pair

As a convenience, a Pair is used for equality.

=begin code :lang<raku>
$q.where([:a(1)]);
# sql: ... WHERE "a" = ?
# bind: [1,]

my $a = 1;
$q.where([:$a]);
# sql: ... WHERE "a" = ?
# bind: [$a,]
=end code

If the value is exactly C<Nil>, then C<IS NULL> is used instead. C<Nil> is the only undefined value
with this special treatment:

=begin code :lang<raku>
$q.where([:a(Nil)]);
# sql: ... WHERE "a" IS NULL

# other values are not special:
$q.where([:a(Any)]);
# sql: ... WHERE "a" = ?
# bind: [Any,]
=end code

=head2 Sub-group (Capture)

To represent a parenthesized sub-group, use a C<Capture> with the C<\(...)> syntax. This Capture
must contain a single Pair, with the key of C<and> or C<or>, depending on which you want, and the
value is another list that creates another C<ConditionClause>.

=begin code :lang<raku>
$q.where([:a<b>, \(:or[
    :c<d>, :e<f>
])]);
# sql: ... WHERE "a" = ? AND (c = ? OR e = ?)
# bind: ["b", "d", "f"]
=end code

This syntax is chosen to avoid difficulties with flattening of lists in Raku. It also avoids some
confusion between Pairs and single-item Hashes.

Subgroups also provide a way to switch from C<AND> to C<OR> logic:

=begin code :lang<raku>
$q.where(\(:or[
    :c<d>, :e<f>
]));
# sql: ... WHERE (c = ? OR e = ?)
# bind: ["d", "f"]
=end code

=head1 COMPATIBILITY

The author primarily uses this module with PostgeSQL, however it should work fine for most database
engines. Some syntax may not be readily accessible outside of the C<Raw.fmt> syntax.

=head1 AUTHOR

Adrian Kreher <avuserow@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2022 - 2023 Adrian Kreher

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
