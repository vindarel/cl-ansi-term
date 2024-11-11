# cl-ansi-term

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/cl-ansi-term.svg?branch=master)](https://travis-ci.org/mrkkrp/cl-ansi-term)
[![Quicklisp](http://quickdocs.org/badge/cl-ansi-term.svg)](http://quickdocs.org/cl-ansi-term/)

`cl-ansi-term` allows to print various primitives on ANSI-compliant
terminals. It also supports coloration and effects. `cl-ansi-term` is not
something like `ncurses`, because it works with primitives that you can
output in your terminal, as well as redirect to a file. In other words, it's
more about good ol' textual interface than *emulation of GUI* in terminal.
An example of user interface created with `cl-ansi-term`
is [here](https://github.com/lisp-mirror/shtookovina).

cl-ansi-term uses the concept of **style sheet** to manage coloration of
output. Define styles, give them names, specify foreground colors,
background colors, and effects for every style.

The library is capable of *detecting whether the output goes to a terminal or a
file*. If the latter case takes place, no escape sequences will be outputted.
It's also possible to disable all effects and coloration.

`cl-ansi-term` can print the following things:

* colorized text
* horizontal lines: `hr`
* progress bars (see also [progressons](https://github.com/vindarel/progressons))
* unordered lists: `u-list`
* ordered lists: `o-list`
* tables: `table`

Hooks are applied before and after each printing primitive, see our documentation.

and (new as of November, 2024 and in testing)

* vertical space: `vspace`
* titles in banners: `banner`

print lists of plists and hash-tables:

* pretty-print a list of plists in a table: `plists-table` and `plists-vtable`
* pretty-print a list of hash-tables in a table: `hts-table` and `hts-vtable`
* pretty-print a single plist: `plist-table` and `plist-vtable`
* pretty-print a single hash-table: `ht-table` and `ht-vtable`


## Installation

Via Quicklisp (recommended):

```common-lisp
(ql:quickload "cl-ansi-term")
```

Then you can use the `term` global nickname.

cl-ansi-term depends on:

* alexandria
* serapeum (new as of Nov, 2024)
* anaphora (will be removed)
* cl-str


## Documentation

See the `doc` directory. The documentation is also available online:

https://vindarel.github.io/cl-ansi-term

Quick snippets:

### Print ordered and unordered lists: `o-list`, `u-list`

~~~lisp
(term:o-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two))
1. ONE
   1. ONE-A
   2. ONE-B
      1. ONE-B-1
      2. ONE-B-2
2. TWO

(term:u-list '((:one one-a (:one-b :one-b-1 :one-b-2)) :two)
              :bullet \"+-\")
+ ONE
  - ONE-A
  - ONE-B
    + ONE-B-1
    + ONE-B-2
+ TWO
~~~

### Print tables: `table`

~~~lisp
(term:table (list '("name" "age")
                  '("me" "7")))
+---------+---------+
|name     |age      |
+---------+---------+
|me       |7        |
+---------+---------+

;; A long cell is truncated to :column-width, 10 by default.

(term:table '(("name" "age" "email")
              ("me" 7 "some@blah")
              ("me" 7 "some@with-some-longer.email")))
+---------+---------+---------+
|name     |age      |email    |
+---------+---------+---------+
|me       |7        |some@blah|
+---------+---------+---------+
|me       |7        |some@w(…)|
+---------+---------+---------+

;; Each column can have a different length.

(term:table '(("name" "age" "email")
              ("me" 7 "some@blah")
              ("me" 7 "some@with-some-longer.email"))
             :column-width '(10 4 20))
+---------+---+-------------------+
|name     |age|email              |
+---------+---+-------------------+
|me       |7  |some@blah          |
+---------+---+-------------------+
|me       |7  |some@with-some-l(…)|
+---------+---+-------------------+
~~~

### Print horizontal lines: `hr`

~~~
(term:hr :filler "=")
================================================================================

(term:cat-print '(:abc :def :ghi) :align :center)
                                   ABCDEFGHI
~~~

### Print vertical space: `vspace`

Use `vspace`, arguments:

* `space`: defaults to 3 newlines
* `stream`: defaults to stdout.

### Print a title in a banner: `banner`

Print a title in between 2 horizontal lines, with vertical space before and after.

```lisp
(banner "My title" :space 1)

--------------------------------------------------------------------------------
     My title
--------------------------------------------------------------------------------


```

### Pretty-print a plist: `plist-table` and `plist-vtable`

`plist-table`:

Print PLIST as a table: the plist keys as the headers row, the plist values as one row below.

COLS allows to limit the number of columns.

Other arguments are passed to the TABLE function.

Example:

```
    (plist-table '(:a 1 :b 2 :c 3))

  =>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+
```

`plist-vtable`:

Print PLIST as a table, where the first column is the keys, the second column is the value.

  Example:

    (plist-vtable '(:a 1 :b 2 :c 3))

  =>

    +---------+---------+
    |A        |1        |
    +---------+---------+
    |B        |2        |
    +---------+---------+
    |C        |3        |
    +---------+---------+

### Pretty-print a hash-table: `ht-table` and `ht-vtable`

They have the same signature and output than the functions for plist above.

### Pretty-print a list of hash-tables: `hts-table` and `hts-vtable`

`hts-table`:

Print the list of HASH-TABLES as a vertical table, where the first column is the keys,
the other columns are the values of each hash-table.

Example:


~~~lisp
(hts-vtable (list (dict :a 1 :b 2 :c 3)
                  (dict :a 10 :b 20 :c 30)))
~~~

`hts-vtable` is similar, only it prints the keys vertically, in the first column.


### Pretty-print a list of plists: `PLISTS-TABLE` and `PLISTS-VTABLE`

`plists-table`:

Print a list of plists as a TABLE.

The first row shows the plist keys, taken from the first plist object.
All other rows show the values of all the plist objects.

If KEYS is given, the table will show only the values, and as many columns, for these keys.
If EXCLUDE is given, the plist values and associated columns are ignored.

Example:

```
(plists-table '((:A 1 :B 2 :C 3) (:A 10 :B 20 :C 30)))

=>

+---------+---------+---------+
|A        |B        |C        |
+---------+---------+---------+
|1        |2        |3        |
+---------+---------+---------+
|10       |20       |30       |
+---------+---------+---------+
```


`plists-vtable`

Print a list of plists as a VTABLE.

The first colum (and not row) shows the plist keys, taken from the first plist object.
All other rows show the values of all the plist objects.

If KEYS is given, the table will show only the values, and as many rows, for these keys.
If EXCLUDE is given, the plist values and associated rows are ignored.

Example:

```
(plists-vtable '((:A 1 :B 2 :C 3) (:A 10 :B 20 :C 30)))

=>

+---------+---------+---------+
|A        |1        |10       |
+---------+---------+---------+
|B        |2        |20       |
+---------+---------+---------+
|C        |3        |30       |
+---------+---------+---------+
```


### Stylesheets and colorized text

Please see our online documentation.

### Docstrings

#### `table`

```
Print a table filling cells with OBJECTS. OBJECTS must be a list of list
designators with equal lengths.

If BORDER-STYLE is NIL, no border will be
printed, otherwise BORDER-STYLE is expected to be a keyword that denotes
the style in which borders of the table should be printed.

HEADER-STYLE will be
applied to the first row of the table (also to the first column if
COL-HEADER is not NIL) and CELL-STYLE will be applied to all other rows. If
CELL-STYLE is a list, its elements will be used to differently render every
column.

Objects that end with MARK-SUFFIX will be printed using MARK-STYLE.

COLUMN-WIDTH is 10 by default. It can be an integer that applies to
all columns, or a list designator to set a different
width for every column. A cell content is truncated to fit the width. See `str:*ellipsis*'
for the ellusion string, `(…)' by default.

ALIGN controls the alignmet inside a cell. It can take the values :LEFT (default value), :CENTER, and :RIGHT.

MARGIN, an integer, is the left margin of the whole table.

Output goes to STREAM.
```

## See also

- https://github.com/AccelerationNet/data-table
- https://github.com/telephil/cl-ascii-table/

Blog posts:

- http://40ants.com/lisp-project-of-the-day/2020/05/0082-data-table.html
- http://40ants.com/lisp-project-of-the-day/2020/05/0084-cl-ascii-table.html

## License

Copyright © 2015–2018 Mark Karpov
Copyright © 2018–2024 Vindarel

Distributed under GNU GPL, version 3.
