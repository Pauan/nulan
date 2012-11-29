How to run
==========

Make sure you have Node.js installed (I've only tested with version 0.8.5)

Then just use ``./nulan`` to get to a REPL


Features
========

* A full-blown Lisp, which means programs are parsed as S-expressions, and there's a very heavy emphasis on "code is data is code"

* Pattern matching for function/macro arguments

* Hyper-static scope at both the global and function level

* Hygienic macros which are even easier to use than Arc/Common Lisp macros

* Syntax to make common idioms shorter and easier to read

* The compiler is written in JavaScript, and is capable of running in a browser: you can incrementally compile/eval Nulan programs at runtime

* Compiles ahead-of-time to extremely fast JavaScript: it should be just as fast or faster than handwritten JS code
