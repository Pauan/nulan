How to install
==============

::

  git clone --recursive https://github.com/Pauan/nulan.git nulan


How to run
==========

Browser
-------

Load ``doc/tutorial.html`` in a modern web browser (I've only tested with Google Chrome 23)


Node.js
-------

Use ``./nulan`` to get to a REPL (I've only tested with version 0.8.5)


Features
========

* A full-blown Lisp, which means programs are parsed as S-expressions, and there's a very heavy emphasis on "code is data is code"

* `Pattern matching <nulan/blob/javascript/notes/Pattern%20matching.rst>`_

* `Hyper-static scope <nulan/blob/javascript/notes/Hyper-static%20scope.rst>`_ at both the global and function level

* `Hygienic macros <nulan/blob/javascript/notes/Hygienic%20macros.rst>`_ which are even easier to use than Arc/Common Lisp macros

* `Customizable syntax <nulan/blob/javascript/notes/Customizable%20syntax.rst>`_ to make common idioms shorter and easier to read

* The compiler is written in JavaScript and is capable of running in a browser: you can incrementally compile/eval Nulan programs at runtime

* Compiles ahead-of-time to extremely fast JavaScript: it should be just as fast or faster than handwritten JS code

* Includes an awesome IDE (built using `CodeMirror <http://codemirror.net/>`_) that runs right in the browser: just open ``doc/tutorial.html``


Examples
========

::

  # This is a dictionary
  box foo = [ bar 5 qux 10 ]

  # Key lookup
  foo.bar
  foo.qux

  # Key assignment
  foo.bar <= 20
  foo.qux <= 30

  # This is a function
  def set -> a
    | box x = foo[a]  # Lookup by expression
    | foo[a] <= 50    # Assign by expression
    | x

  set "bar"
  set "qux"

  foo.bar
  foo.qux

::

  # Simulating a `for` loop using a `while` loop
  $mac for -> init test incr body
    'w/new-scope
       | init
       | while test
           | body
           | incr

  for (box i = 0) (i < 10) (++ i)
    prn i

::

  # Simulating a `do..while` loop using a `while` loop
  $mac do -> body {('while) test}
    'while %t
       | body
       | if ~ test
           &break;

  box i = 0
  do
    | prn i
    | ++ i
    while (i < 10)

::

  # Infinite loop; be careful, the only way to stop it is to shut down the terminal!
  $mac 5ever -> body
    'while %t
       body

  5ever
    | prn 1
    | prn 2
    | prn 3
    | prn 4
    | prn "das mor den 4ever"

::

  # Macro to iterate over the elements of any list or string
  $mac w/each -> {('=) x y} body
    w/uniq i len
      w/complex y
        'w/box len = y.length
           for (box i = 0) (i ~= len) (++ i)
             w/box x = y[i]
               body

  w/each x = {1 2 3}
    | prn x
    | prn x + 5
    | prn;

::

  # Macro to iterate over the elements of any list or string in reverse order
  $mac w/each-rev -> {('=) x y} body
    w/uniq i
      w/complex y
        'w/box i = y.length
           while i
             w/box x = y[-- i]
               body

  w/each-rev x = {1 2 3}
    | prn x
    | prn x + 5
    | prn;

::

  # The built-in Array methods work very nicely with Nulan's -> syntax
  {1 2 3}.for-each -> x
    | prn x
    | prn x + 5
    | prn;

  {1 2 3}.map -> x
    x + 5

  {1 2 3}.reduce -> x y
    "(@x @y)"

::

  # An example of an unhygienic macro
  # Just like in Arc, it binds the symbol `it` to the test condition
  $mac aif -> test @rest
    w/box it = sym "it"
      'w/box it = test
         if it ,@:if rest.length >= 2
                    w/box {x @rest} = rest
                      'x (aif ,@rest)
                    rest

  aif 1 + 2
    it
    it

  aif %f
    it
    it

::

  def foo -> x y
    x + y

  $syntax-infix foo

  1 foo 2    # Custom infix syntax

  (foo) 1 2  # Wrapping in parens disables syntax

::

  # Array comprehensions
  box in

  $mac for -> x {('in) n y}
    'y.map -> n x

  $syntax-infix for 0 [ order "right" ]
  $syntax-infix in  0 [ order "right" ]

  (x + 2) for x in {1 2 3}

::

  #! /usr/bin/env nulan

  # A shell script that creates a simple HTTP server
  # Taken from http://nodejs.org/
  box net = require "net"

  box server = net.create-server -> o
                 | o.write "Echo server\r\n"
                 | o.pipe o

  server.listen 1337 "127.0.0.1"


FAQ
===

* **Q:** Why doesn't this work?!

  ::

    def foo -> x
      bar x + 1

    def bar -> x
      x + 5

    foo 20

  **A:** Nulan uses hyper-static scope, so you need to rearrange it so ``foo`` is defined after ``bar``::

    def bar -> x
      x + 5

    def foo -> x
      bar x + 1

    foo 20

* **Q:** Well, okay, but what about this?

  ::

    $mac foo ->
      '1 + 2

    prn foo

  **A:** Nulan has a *very* strict separation between compile-time and run-time: things that exist at compile-time **cannot** be used at run-time in any way, shape, or form. And vice versa: things that exist at run-time cannot be used at compile-time.

  Certain macros like ``$mac`` are prefixed with ``$`` which indicates that they are evaluated at compile-time. To make the above example work, you have to evaluate the expression at compile-time by using ``$run``::

    $mac foo ->
      '1 + 2

    $run
      prn foo

* **Q:** If there's such a strict separation between the two, why does this work?

  ::

    def foo -> x
      x + 1

    $mac bar -> x
      'foo x

    bar 10

  **A:** Nulan replaces symbols with boxes. The *value* of the symbol ``foo`` is not available, but the *box* is.

  The ``'`` macro returns boxes, which means that the ``bar`` macro returns the *box* for ``foo``, not the *value* for ``foo``. This is the **only** way that you can use run-time stuff at compile-time.

  However, this would not work...

  ::

    $mac bar -> x
      foo x

  ...because it's trying to use the *value* of the ``foo`` symbol, which doesn't exist at compile-time.

  In addition, if a *macro* is the first element of a list, it is evaluated at compile-time, which is why ``bar 10`` works. But ``prn bar 10`` would **not** work, because the macro ``bar`` isn't the first element of the list

* **Q:**
