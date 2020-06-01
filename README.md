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
is [here](https://github.com/mrkkrp/shtookovina).

cl-ansi-term uses the concept of **style sheet** to manage coloration of
output. Define styles, give them names, specify foreground colors,
background colors, and effects for every style.

The library is capable of *detecting whether the output goes to a terminal or a
file*. If the latter case takes place, no escape sequences will be outputted.
It's also possible to disable all effects and coloration.

`cl-ansi-term` can print the following things:

* colorized text
* horizontal lines
* progress bars
* unordered lists
* ordered lists
* tables

## Installation

Via Quicklisp (recommended):

```common-lisp
(ql:quickload "cl-ansi-term")
```

Then you can use the `term` nickname.


## Documentation

See the `doc` directory. The documentation is also available online:

https://vindarel.github.io/cl-ansi-term

Quick snippets:

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


(term:hr :filler "=")
================================================================================

(term:cat-print '(:abc :def :ghi) :align :center)
                                   ABCDEFGHI
~~~

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
Copyright © 2018–2020 Vindarel

Distributed under GNU GPL, version 3.
