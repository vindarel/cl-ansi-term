# cl-ansi-term

`cl-ansi-term` allows to print various primitives on ANSI-complaint
terminals. It also supports coloration and effects. `cl-ansi-term` is not
like `ncurses`, it works with primitives that you can output on your
terminal, as well as redirect to a file without any loss.

`cl-ansi-term` can print the following things:

* colorized text;
* horizontal lines;
* progress bars;
* unordered lists;
* ordered lists;
* tables.

`cl-ansi-term` uses concept of style sheet to manage coloration of
output. Define styles, give them names, specify foreground colors,
background colors, and effects for every style.

`cl-ansi-term` provides hooks to give more control over the library.

`cl-ansi-term` is able to detect whether output goes to a terminal or to a
file. If the latter case takes place, no escape sequences will get into the
file. It's also possible to disable all effects and coloration.

## Installation

Download or clone the repository and put it into some place where ASDF can
find it.

Via Quicklisp:

```
(ql:quickload "cl-ansi-term")
```

## Documentation

See contents of directory `doc`. Documentation is also available online:

https://mrkkrp.github.io/cl-ansi-term

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
