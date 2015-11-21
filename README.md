# cl-ansi-term

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Quicklisp](http://quickdocs.org/badge/cl-ansi-term.svg)](http://quickdocs.org/cl-ansi-term/)

`cl-ansi-term` allows to print various primitives on ANSI-complaint
terminals. It also supports coloration and effects. `cl-ansi-term` is not
something like `ncurses`, because it works with primitives that you can
output on your terminal, as well as redirect to a file without any loss. In
other words it's more about good ol' textual interface than *emulation of
GUI* in terminal. An example of user interface created with `cl-ansi-term`
is [here](https://github.com/mrkkrp/shtookovina).

`cl-ansi-term` can print the following things:

* colorized text
* horizontal lines
* progress bars
* unordered lists
* ordered lists
* tables

`cl-ansi-term` uses concept of style sheet to manage coloration of
output. Define styles, give them names, specify foreground colors,
background colors, and effects for every style.

The library is able to detect whether output goes to a terminal or to a
file. If the latter case takes place, no escape sequences will get into the
file. It's also possible to disable all effects and coloration.

## Installation

Download or clone the repository and put it into some place where ASDF can
find it.

Via Quicklisp (recommended):

```common-lisp
(ql:quickload "cl-ansi-term")
```

## Documentation

See contents of directory `doc`. Documentation is also available online:

https://mrkkrp.github.io/cl-ansi-term

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
