How to run
==========

Make sure you have Node.js installed (I've only tested with version 0.8.5)

Then just use ``./nulan`` to get to a REPL


Examples
========

::

  var foo = [ bar 5 qux 10 ]

  foo.bar
  foo.qux

  foo.bar <= 20
  foo.qux <= 30

  def fn -> a
    let x = foo[a]
      | foo[a] <= 50
      | x

  fn "bar"
  fn "qux"

  foo.bar
  foo.qux

::

  $mac w/complex -> x body
    w/uniq tmp u
      'let tmp = x
         let x = u
           'let u = tmp
              body

  $mac w/each -> x body
    w/var i   = uniq;
          l   = uniq;
          u   = uniq;
          y   = x.1
          x   = x.0
          tmp = y
          y   = u
      'w/var u = tmp
             l = y."length"
         for (var i = 0) (i ~= l) (++ i)
           w/var x = get y i
             body

  $mac w/each -> {(sym "=") x y} body
    w/uniq i l u
      w/var v = y
            y = u
        'w/var u = v
               l = y."length"
           for (var i = 0) (i ~= l) (++ i)
             w/var x = get y i
               body


  syntax-rule - 0 -> l s {y @r}
    if l.length == 0
      {{s y} @r2}
      let {@l x} l
        {@l {s x y} @r}

  syntax-rule = 0 -> l s r
    {@l s @(parse-line r)}

  syntax-rule -> 0 -> l s {@args body}
    {@l {s {list @args} body}}

  &eval
    w/namespace value
      x + y


  $mac w/each -> {(sym "=") x y} body
    w/uniq i l
      w/complex y
        'w/var l = y.length
           for (var i = 0) (i ~= l) (++ i)
             w/var x = y[i]
               body

  w/each x = {1 2 3}
    | prn x
    | prn x + 5

  {1 2 3}.for-each -> x
    | prn x
    | prn x + 5


  $mac loop -> body
    'while %t
       body

  loop
    | prn 1
    | prn 2
    | prn 3


Features
========

* A full-blown Lisp, which means programs are parsed as S-expressions, and there's a very heavy emphasis on "code is data is code"

* `Pattern matching <nulan/blob/javascript/notes/Pattern%20matching.rst>`_ for function/macro arguments

* `Hyper-static scope <nulan/blob/javascript/notes/Hyper-static%20scope.rst>`_ at both the global and function level

* `Hygienic macros <nulan/blob/javascript/notes/Hygienic%20macros.rst>`_ which are even easier to use than Arc/Common Lisp macros

* `Customizable syntax <nulan/blob/javascript/notes/Customizable%20syntax.rst>`_ to make common idioms shorter and easier to read

* The compiler is written in JavaScript and is capable of running in a browser: you can incrementally compile/eval Nulan programs at runtime

* Compiles ahead-of-time to extremely fast JavaScript: it should be just as fast or faster than handwritten JS code


FAQ
===

* Q: Why doesn't this work?!

  ::

    def foo -> x
      bar x + 1

    def bar -> x
      x + 5

    bar 20

  A: Nulan uses hyper-static scope, so you need to rearrange it to be like this::

    def bar -> x
      x + 5

    def foo -> x
      bar x + 1

    bar 20

* Q: Well, okay, but what about this?

  ::

    $mac foo ->
      '1 + 2

    prn foo

  A: Nulan has a strict separation between compile-time and run-time: variables defined at compile-time **cannot** be seen at run-time in any way, shape, or form. Certain macros like ``$mac`` are prefixed with a ``$`` which indicates that they are evaluated at compile-time.

  To make the above example work, you have to evaluate the expression at compile-time by using ``$eval``::

    $mac foo ->
      '1 + 2

    $eval
      prn foo

* Q: If there's such a strict separation between the two, why does this work?

  ::

    def foo -> x
      x + 1

    $mac bar -> x
      '(foo x)

    bar 10

  A: Variables defined at compile-time **absolutely cannot** be used at run-time, but run-time variables **can** be used at compile-time.

  To be more specific, Nulan wraps every variable in a box. This box is available at compile-time, but the *value* of the box is **not** available. This is for the obvious reason that at compile-time, the expression has not been evaluated yet, so the box cannot have a value.

  So, in the macro ``bar``, it expands to the *box* ``foo`` rather than the *value* ``foo``, so it all works out just fine.

  As for ``bar 10``, if a macro is the first element of a list, it is evaluated at compile-time, so this works. But this would **not** work: ``prn bar 10`` because the macro ``bar`` isn't the first element of the list.
