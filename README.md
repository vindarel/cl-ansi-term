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

### Print tables: `table` and `vtable`

`table` and `vtable` accept:

- a list of regular lists, composed of string-designators
- a list of hash-tables
- a list of property-lists
  - use `:plists-p t` or `(setf *prefer-plists-in-tables* t)` to help the function distinguish between a regular list of lists
- a single hash-table or a single property list.

~~~lisp
;; The first list, headers, as the first row:
(term:table (list '("name" "age")
                  '("me" "7")))
+---------+---------+
|name     |age      |
+---------+---------+
|me       |7        |
+---------+---------+

;; Headers as the first column:
(term:vtable (list '("name" "age")
                         '("me" "7")))
+---------+---------+
|name     |me       |
+---------+---------+
|age      |7        |
+---------+---------+
~~~

Also print lists of hash-tables or lists of plists:

~~~lisp
(defparameter *my-plist* '(:a 1 :b 2 :c 3))
(table (list *my-plist* *my-plist*) :plists-p t)
+---------+---------+---------+
|A        |B        |C        |
+---------+---------+---------+
|1        |2        |3        |
+---------+---------+---------+
|1        |2        |3        |
+---------+---------+---------+

(vtable (list *my-plist* *my-plist*) :plists-p t)
+---------+---------+---------+
|A        |1        |1        |
+---------+---------+---------+
|B        |2        |2        |
+---------+---------+---------+
|C        |3        |3        |
+---------+---------+---------+
~~~

See also `hts-table`, `hts-vtable`, `plists-table` and `plists-table`.

You can choose a set of keys (headers) or exclude some of them:

- `:keys` is a list of keys to display
- `:exclude` is a list of keys to not display.

*(those can be a single element)*

~~~lisp
(table (list plist plist) :plists-p t :exclude :c)
=>
+---------+---------+
|A        |B        |
+---------+---------+
|1        |2        |
+---------+---------+
|1        |2        |
+---------+---------+
~~~

A long cell is truncated to :column-width, 10 by default.

~~~lisp
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
~~~

Each column can have a different length:

~~~lisp
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



### Stylesheets and colorized text

Please see our online documentation.

### Docstrings

#### `table`

```
Print a table filling cells with OBJECTS.

  OBJECTS can be:

  - a list of lists of string designators with equal lengths
.   - generally, the first list is a list of headers.
  - a list of hash-tables
    - the table displays the first hash-table keys and all the hash-tables values.
    - see also HTS-TABLE
  - a list of property-lists
    - the table prints the keys and all the plists' values
    - to help the TABLE understand the arguments are plists
      and not regular lists, set the PLISTS-P key argument to T
      or the variable *prefer-plists-in-tables* to T.
    - see also PLISTS-TABLE
  - a single hash-table
  - a single plist.

  KEYS is a list of keys to display (only applicable for hash-tables and plists). The associated rows or columns will be displayed.

  EXCLUDE is a list of keys to NOT display (only applicable for hash-tables and plists).
Example:

   (table '((:A :B :C) (1 2 3)))

=>

    +---------+---------+---------+
    |A        |B        |C        |
    +---------+---------+---------+
    |1        |2        |3        |
    +---------+---------+---------+
    |10       |20       |30       |
    +---------+---------+---------+


See VTABLE to print the table vertically.

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
