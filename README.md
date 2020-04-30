# cl-ansi-term

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/mrkkrp/cl-ansi-term.svg?branch=master)](https://travis-ci.org/mrkkrp/cl-ansi-term)
[![Quicklisp](http://quickdocs.org/badge/cl-ansi-term.svg)](http://quickdocs.org/cl-ansi-term/)

`cl-ansi-term` allows to print various primitives on ANSI-complaint
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

## License

Copyright © 2015–2018 Mark Karpov
Copyright © 2018–2020 Vindarel

Distributed under GNU GPL, version 3.
