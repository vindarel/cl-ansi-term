# cl-ansi-term

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
<!-- [![Build Status](https://travis-ci.org/mrkkrp/cl-ansi-term.svg?branch=master)](https://travis-ci.org/mrkkrp/cl-ansi-term) -->
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

Print tables:

* tables: `table` and `vtable` (headers in the first column)
  * accepts:
    * lists of lists
    * lists of hash-tables
    * lists of property-lists (see the `:plist t` argument and the `plists-table` function)
    * a single element

```
+---------+---------+---------+
|A        |1        |1        |
+---------+---------+---------+
|B        |2        |2        |
+---------+---------+---------+
|C        |3        |3        |
+---------+---------+---------+
```

Hooks are applied before and after each printing primitive, see our documentation.

and

* vertical space: `vspace`
* titles in banners: `banner`


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
  - use `:plist t` or `(setf *prefer-plists-in-tables* t)` to help the function distinguish between a regular list of lists and a list of property-lists.
- a single hash-table or a single property list.

The KEYS and EXCLUDE arguments allow to filter in or filter out the rows to display.


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

Print lists of hash-tables or lists of plists:

~~~lisp
(defparameter *my-plist* '(:a 1 :b 2 :c 3))
(table (list *my-plist* *my-plist*) :plist t)
+---------+---------+---------+
|A        |B        |C        |
+---------+---------+---------+
|1        |2        |3        |
+---------+---------+---------+
|1        |2        |3        |
+---------+---------+---------+

(vtable (list *my-plist* *my-plist*) :plist t)
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
(table (list plist plist) :plist t :exclude :c)
=>
+---------+---------+
|A        |B        |
+---------+---------+
|1        |2        |
+---------+---------+
|1        |2        |
+---------+---------+
~~~

#### Column width

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

### Print a title in a banner: `banner`, `banner-fmt`

Print a title in between 2 horizontal lines, with vertical space before and after.

```lisp
(banner "My title" :space 1)

--------------------------------------------------------------------------------
     My title
--------------------------------------------------------------------------------


```

`banner-fmt` accepts TITLE with FORMAT control strings and calls FORMAT on it with ARGS.

         (banner-fmt \"file ~a\" \"test.csv\")

`title` is a `banner` with no borders.


### Print text centered, with margin, with stylesheets: `cat-print` and `print-styled`

Print text with CAT-PRINT, but apply CONTROL-STRING with the arguments from ARGS, where each tilde character of CONTROL-STRING is replaced with an argument.

A special syntax can be used to apply styles.

Example:

~~~lisp
(term:print-styled "~ and ~" :args '("foo" "bar") :align :center)
~~~

is equivalent to

~~~lisp
(term:cat-print "foo and bar" :align :center)
~~~

Any region of text in CONTROL-STRING can be printed in a
specified style following this pattern:

    [text](:name-of-style)

where :name-of-style is a downcase keyword in the style sheet.

The style of the rest of the output defaults to BASE-STYLE.

**ALIGN** can be :LEFT (default), :CENTER, and :RIGHT.

**MARGIN** is the length of the left margin.

**FILL-COLUMN** sets the column width:

~~~lisp
(term:print "~ and ~" :args '("foo" "bar") :align :center :fill-column 10)
                                    foo and
                                      bar
~~~

Output goes to STREAM."

### Print a progress bar

~~~lisp
(term:progress-bar "test" 82)
test   ##################################################################################
~~~

On an interactive terminal, next calls erase the progress bar to print it again and have an effect of… progress.

The progress bar respects styles with BAR-STYLE, LABEL-STYLE and NUM-STYLE.

See also [progressons](https://github.com/vindarel/progressons).


### Stylesheets and colorized text

Start by defining your stylesheet.

~~~lisp
(term:update-style-sheet
 '((:header :cyan   :underline)
   (:mark   :red    :reverse)
   (:term   :yellow :bold)))
~~~

`:header`, `:mark` and `:term` are your own vocabulary. Anytime you
use functions that accept a style, reference them.

Example:

~~~lisp
(term:table (list '(:name :age) '(:me 7)) :header-style :header)
~~~

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
      and not regular lists, set the PLIST key argument to T
      or the variable *prefer-plists-in-tables* to T.
    - see also PLISTS-TABLE
  - a single hash-table
  - a single plist.

  KEYS is a list of keys to display. The associated rows or columns will be displayed.
    With list of regular lists, the default headers are considered to be in the first list.

  EXCLUDE is a list of keys to NOT display.
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

## Lisp?!

- https://lispcookbook.github.io/cl-cookbook/
- https://github.com/CodyReichert/awesome-cl
- https://lisp-journey.gitlab.io/
- [Learn Common Lisp in videos with a code-first approach: learn CLOS, macros, condition handling, the interactive workflow and much more](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358)
  - thanks for your support!

> I have done some preliminary Common Lisp exploration prior to this course but had a lot of questions regarding practical use and development workflows. This course was amazing for this! I learned a lot of useful techniques for actually writing the code in Emacs, as well as conversational explanations of concepts that had previously confused me in text-heavy resources. Please keep up the good work and continue with this line of topics, it is well worth the price!

Preston

## License

Copyright © 2015–2018 Mark Karpov
Copyright © 2018–2024 Vindarel

Distributed under GNU GPL, version 3.
