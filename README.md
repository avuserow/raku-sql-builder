[![Actions Status](https://github.com/avuserow/raku-sql-builder/actions/workflows/test.yml/badge.svg)](https://github.com/avuserow/raku-sql-builder/actions)

NAME
====

SQL::Builder - build SQL statements

SYNOPSIS
========

```raku
use SQL::Builder;

my $sql = SQL::Builder.new;

# all SELECT queries start with from
my $q1 = $sql.from('users').select(<name address>).where(['email', '=', 'foo@example.com']);
my $statement = $q1.build;
$db.execute($statement.sql, |$statement.bind);
# SELECT name, address FROM users WHERE email = ?

# many SQL fragments are supported:
my $q2 = $sql.from('songs').
    select('album', (:albumlength(Fn.new('SUM', 'length')))).
    where(:and, [:online(True), ['year', '>=', 2020]]).
    group-by('album').
    order-by('album');

# subselects too:
my $q3 = $sql.from(:inner($q2)).select(Fn.new('MAX', 'inner.albumlength'));

# joins:
my $q4 = $sql.from('songs').
    join(:left, 'ratings', :using('song-id')).
    select(<title artist album rating>);
```

DESCRIPTION
===========

SQL::Builder generates SQL statements using an approach modeled after the 'builder' pattern. This allows us to support a wide variety of SQL syntax, including sub-queries, while allowing each option to remain intuitive.

This module is partially inspired by Python's SQLAlchemy Core as well as Perl 5's `SQL::Abstract`. This module forgoes an ORM in favor of generating raw SQL, giving you the power to build complex queries and not lose execution speed.

This module also prioritizes safety. Each option has rules on how it handles an untyped string, and it defaults to either a quoted `Identifier` or a `Placeholder`, depending on the ergonomics of each function.

SQL::Builder can represent a large amount of SQL statements, and is best suited for statements of medium complexity with reasonably high amounts of runtime changes (e.g. where clause, optional joins or sub-selects).

TYPE SYSTEM
===========

SQL::Builder has a basic type system to ensure the right behavior when a value is provided. Each builder option has a default way to handle a string, typically either as either Identifier or Placeholder and each method documents its expectations.

These are all subclasses of `SQLSyntax` and are:

  * `Identifier`: used for table and column names, and will be split into pieces and quoted. See `Identifier` below for full details.

  * `Placeholder`: always generates a single `?` placeholder and puts its value into the list of bind values.

  * `Value`: used for a string value when `Placeholder` is not an option. Always fully quoted.

  * `Raw`: this is a raw fragment of SQL. Use `Raw.fmt` to safely write raw fragments.

  * `Fn`: used to help write function calls. The first argument is unquoted. Subsequent arguments default to `Identifier`, and are put in a parenthesized list.

Identifier
----------

The `Identifier` type is the most common way to handle untyped input. It is used to represent tables and columns, with an optional type cast. If a dotted value is specified (e.g. `table.column`), then this is split into two bits before quoting. Additionally, if it ends with `::`, then that suffix is allowed as a PostgreSQL-style type cast.

Example input and output for the SQL portion:

  * `Identifier.new("foo")` -> `"foo"`

  * `Identifier.new("foo.bar")` -> `"foo"."bar"`

  * `Identifier.new("foo::int")` -> `"foo"::int`

  * `Identifier.new("foo.bar::uuid[]")` -> `"foo"."bar"::uuid[]`

Placeholder
-----------

This is a simple SQL placeholder. It is used as in the right-hand side of various conditionals. The resulting fragment is always a single `?`, and the value is put in the bind list.

No conversion is done to the value, so you can pass in values of type `Str`, `Int`, `DateTime`, `Bool`, as well as `Array`. Currently, Arrays do not have any special logic to create additional placeholders or otherwise flatten the array. See the Cookbook section to see how to handle this.

Value
-----

The value is escaped and wrapped in single quotes.

Raw
---

No escaping is performed. The `Raw.fmt` method provides a safer way to use this.

Raw.fmt(Str $template, SQLSyntax $a, $b, ...)
---------------------------------------------

The `fmt` method lets you safely build bits of SQL by providing a template containing `{}` sequences that are replaced by the following arguments. You must have the same number of `{}` replacements as arguments, and all arguments must be one of the types in this section (e.g. subclasses of `SQLSyntax`).

Examples:

```raku
Raw.fmt("COUNT({}) AS {}", Identifier.new("artist"), Identifier.new("artistcount"))
# sql: COUNT("artist") AS "artistcount"
# bind: []

Raw.fmt('unnest({}::uuid[]) WITH ORDINALITY t(id, ord)', Placeholder.new(@ids))
# sql: unnest(?::uuid[]) WITH ORDINALITY t(id, ord)
# bind: [@ids,]

Raw.fmt('date_trunc({}, {})', Value.new('day'), Identifier.new('song-start'))
# sql: date_trunc('day', "song-start")
# bind: []
```

Fn
--

Fn (function) is a helper to make function calls. The first item is taken as a Raw value, and all following items default to Identifiers.

Examples:

```raku
Fn.new('COUNT', 'artists')
# sql: COUNT("artists")
# bind: []

Fn.new('ANY', @my-stuff)
# sql: ANY(?)
# bind: [@my-stuff,]
```

SELECT QUERIES
==============

Select queries are created with the `from` method on the `SQL::Builder` object. All other options can be passed in any order. All options except `join` overwrite the current value. Each option returns the SelectBuilder instance, allowing for a chain style: `$sql.from('foo').select('bar').group-by('baz')`

Multiple tables in the `from` clause are not yet supported. Use `join` instead.

Select queries support the following options:

from(Str $table)
----------------

Creates a `SelectBuilder` from the given table.

from(Pair[Str, SelectBuilder])
------------------------------

Creates a subselect from the provided SelectBuilder, aliased to the provided Str. Contrived example:

```raku
my $inner-q = $sql.from('foo').select('bar');
my $q = $sql.from(:inner($inner-q)).select('bar');
# SELECT "bar" FROM (SELECT "bar" FROM "foo") AS "inner"
```

select(:all)
------------

Emits a `SELECT *`.

select(*@columns)
-----------------

Specifies the list of values to return. Each column defaults to `Identifier`.

If a `Pair` is provided, then this is treated as a column alias:

```raku
$sql.from('table').select(:foo<bar>);
# sql: SELECT "bar" AS "foo" FROM "table"
```

Note that due to Raku's handling of Pairs, if you mix Positional and non-Positional arguments, the Pairs will always be at the end. You can avoid this by passing an Array, or parenthesizing the Pairs:

```raku
$sql.from('table').select(<foo bar>, :a<b>, :c<d>)
# SELECT "foo", "bar", "b" AS "a", "d" AS "c" FROM table

$sql.from('table').select(:a<b>, <foo bar>, :c<d>)
# SELECT "foo", "bar", "b" AS "a", "d" AS "c" FROM table

# Instead, pass all values as positional elements, in any of the following ways:
$sql.from('table').select([:a<b>, "foo", "bar", :c<d>])
$sql.from('table').select([:a<b>, <foo bar>.flat, :c<d>])
$sql.from('table').select((:a<b>), "foo", "bar", (:c<d>))
# SELECT "b" AS "a", "foo", "bar", "d" AS "c" FROM table
```

where($clause)
--------------

Provide a `WHERE` clause with a single value:

```raku
$sql.from('users').select('email').where(:username<ak>)
# sql: SELECT "email" FROM "users" WHERE "username" = ?
# bind: ["ak",]

$sql.from('users').select('email').where(["username", "=", "ak"])
# sql: SELECT "email" FROM "users" WHERE "username" = ?
# bind: ["ak",]
```

The parameter is used as a single-item `ConditionClause`. See the `ConditionClause` documentation below for all the options.

where(:and/:or @where)
----------------------

Provide a `WHERE` clause with many values. You must pass either `:and` or `:or`, which determines how the list of values is joined. The values are used as a `ConditionClause`, see the documentation below for the details.

```raku
$sql.from('users').select('email').where([["email", "LIKE", "%gmail.com"], ["email", "LIKE", "%googlemail.com"]])
# sql: SELECT "email" FROM "users" WHERE "email" LIKE ? OR "email" LIKE ?
# bind: ["%gmail.com", "%googlemail.com"]
```

Note that the `@where` clause must be a singular `List` when passing multiple clauses. The above example cannot be written as:

```raku
$sql.from('users').select('email').where(["email", "LIKE", "%gmail.com"], ["email", "LIKE", "%googlemail.com"])
# ERROR ERROR ERROR
```

join($table, :@on, :$using, Str :$as, :$inner/:$left/:$right/:$full)
--------------------------------------------------------------------

Provides a `JOIN` clause to this query. `$table` should be either the name of a table (treated as an Identifier), or a `SelectBuilder` to use a sub-query.

Exactly one of `:$using` or `:@on` must be specified, which determines how to join the table. `:$using` is for when the columns match exactly. `:@on` is passed to `ConditionClause` and lets you fully control the join logic. See the `ConditionClause` documentation below. These correspond to the `USING(column)` and `ON expr` portions of the SQL join expression, respectively.

The `:$as` parameter is optional which controls the join expression's alias. This is needed if you use a sub-query or the same table is joined multiple times. This is treated as an Identifier.

Finally, one of `:$inner/:$left/:$right/:$full` may be specified to control the type of JOIN. This is optional.

Multiple JOINs are supported and are processed in the order they are added.

There is currently no way to clear the list of JOINs from a query.

Examples:

```raku
$sql.from('t1').join('t2', :using<id>).select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t1" JOIN "t2" USING("id")

my $inner = $sql.from('t1').select(:bar<foo>, 'id');
$sql.from('t2').
    join(:left, $inner, :as<inner>, :on['inner.id', '=', Identifier.new('t2.id')]).
    select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t2" LEFT JOIN (SELECT "id", "foo" AS "bar" FROM "t1") AS "inner" ON "inner"."id" = "t2"."id"

$sql.from('t1').
    join('t2', :on[:and, ["t1.id", "=", Identifier.new("t2.id")], ["t1.foo", "<", "t2.foo"]]).
    select(<t1.foo t2.bar>);
# sql: SELECT "t1"."foo", "t2"."bar" FROM "t1" JOIN "t2" ON "t1.id" = "t2.id" AND "t1"."foo" < "t2"."foo"
```

limit(Int $n)
-------------

Provides a `LIMIT` clause (with the specified value as a placeholder):

```raku
$sql.from('table').select(<foo bar>).limit(1)
# sql: SELECT "foo", "bar" FROM "table" LIMIT ?
# bind: 1
```

offset(Int $n)
--------------

Provides a `OFFSET` clause (with the specified value as a placeholder):

```raku
$sql.from('table').select(<foo bar>).limit(1).offset(2)
# sql: SELECT "foo", "bar" FROM "table" LIMIT ? OFFSET ?
# bind: [1, 2]
```

group-by(*@columns)
-------------------

Provides a `GROUP BY` clause on the specified columns:

```raku
$sql.from('songs').select(Fn.new('SUM', 'length'), 'artist', 'year').group-by('artist', 'year')
# SELECT SUM("length"), "artist", "year" FROM songs GROUP BY "artist", "year"
```

having($clause)
---------------

Provides a `HAVING` clause. This is handled identical to a `WHERE` clause, see the documentation above.

having(:and/:or @having)
------------------------

Provides a `HAVING` clause. This is handled identical to a `WHERE` clause, see the documentation above.

order-by(*@columns)
-------------------

Provides an `ORDER BY` clause on the specified columns:

```raku
# pick 10 shortest shortest songs
$sql.from('songs').select('title').order-by('length').limit(10)
# sql: SELECT "title" FROM "songs" ORDER BY "length" limit ?
# bind: 10

# pick 10 longest songs
$sql.from('songs').select('title').order-by(Raw.fmt('{} DESC', Identifier.new('length'))).limit(10)
# sql: SELECT "title" FROM "songs" ORDER BY "length" DESC limit ?
# bind: 10

# pick 10 random items
$sql.from('songs').select('title').order-by(Fn.new('RANDOM')).limit(10)
# sql: SELECT "title" FROM "songs" ORDER BY RANDOM() limit ?
# bind: 10
```

build()
-------

Converts this `SelectBuilder` into an `SQLFragment` that has `.sql` and `.bind` methods. This method may be called more than once, but it is not recommended. The SQL and bind values may not appear in the same order across invocations.

clone()
-------

Returns a new `SelectBuilder` in the same state. Useful if you want to have a common set of options, and then use many times:

```raku
sub getuser {
    state $q = $sql.from('users').select(<username email address>);
    $q.clone;
}

# multiple times later:
$db.query(.sql, |.bind) given getuser().where(:$username).build;
```

ConditionClause
===============

The `ConditionClause` syntax is how `SQL::Builder` encodes the options of the `WHERE` clause and the `JOIN ... ON` clause.

At its core, this syntax is a list of items. Each item can be one of three values:

3-Valued List
-------------

The main item is a list of exactly three items, typically a column (`Identifier`), an operator (`Raw`), and a `Placeholder` value.

```raku
$q.where(["a", "=", 1])
# sql: ... WHERE a = ?
# bind: [1,]
```

Any of these three can be replaced by an explicit type from the "Type System" section above:

```raku
$q.where([Fn.new("lower", "a"), "=", Fn.new("lower", "b")])
# sql: ... WHERE lower("a") = lower("b")

$q.where(["b", ">=", Raw.fmt("{} / 2.0", Placeholder.new(3))])
# sql: ... WHERE "b" >= ? / 2.0
# bind: [3,]
```

Pair
----

As a convenience, a Pair is used for equality.

```raku
$q.where([:a(1)])
# sql: ... WHERE "a" = ?
# bind: [1,]

$q.where([:$a])
# sql: ... WHERE "a" = ?
# bind: [$a,]
```

If the value is exactly `Nil`, then `IS NULL` is used instead. `Nil` is the only undefined value with this special treatment:

```raku
$q.where([:a(Nil)])
# sql: ... WHERE "a" IS NULL
```

Sub-group (Capture)
-------------------

To represent a parenthesized sub-group, use a `Capture` with the `\(...)` syntax. This Capture must contain a single Pair, with the key of `and` or `or`, depending on which you want, and the value is another list that creates another `ConditionClause`.

```raku
$q.where(:and, [:a<b>, \(:or[
    :c<d>, :e<f>
])])
# sql: ... WHERE "a" = ? AND (c = ? OR e = ?)
# bind: ["b", "d", "f"]
```

This syntax is chosen to avoid difficulties with flattening of lists in Raku. It also avoids some confusion between Pairs and single-item Hashes.

AND, OR, and single items
-------------------------

If you pass multiple items in a clause, you must choose whether to use `AND` logic or `OR` logic:

```raku
$q.where([["a", "=", 1], :b(2)])
# ERROR: don't know whether to use AND or OR

$q.where(:and, [["a", "=", 1], :b(2)])
# sql: a = ? AND b = ?

$q.where(:or, [["a", "=", 1], :b(2)])
# sql: a = ? OR b = ?

# same applies for JOIN ... ON:
$q.join("table", :on(:or, ["a", "=", 1], [:b(2)]))
# sql: ... JOIN "table" ON a = ? OR b = ?
```

Mixed AND/OR are not permitted without using a Capture to represent a subgroup.

If you pass a single item, you can avoid the outer list in the `where` method, as well as avoiding a meaningless `:and/:or`:

```raku
# these are the same:
$q.where(["a", "=", 1])
$q.where([["a", "=", 1]])
$q.where([["a", "=", 1],])

# also the same:
$q.where(:a(1))
$q.where([:a(1)])
$q.where((:a(1)))
```

COMPATIBILITY
=============

The author primarily uses this module with PostgeSQL, however it should work fine for most database engines. Some syntax may not be readily accessible outside of the `Raw.fmt` syntax.

AUTHOR
======

Adrian Kreher <avuserow@gmail.com>

COPYRIGHT AND LICENSE
=====================

Copyright 2022 - 2023 Adrian Kreher

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

