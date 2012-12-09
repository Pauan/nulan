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
    let x = foo.a
      | foo.a <= 50
      | x

  fn "bar"
  fn "qux"

::

  mac w/complex -> x body
    uniqs tmp u
      'let tmp = x
         let x = u
           'let u = tmp
              body

  # {(id is) x y}
  mac each -> x body
    | var y = x.2
    | x <= x.1
    | uniqs i l u
        let tmp = y
          let y = u
            'let u = tmp
               let l = y.length
                 for (var i = 0) (isnt i l) (++ i)
                   let x = get y i
                     body

  (is (var foo) (bar (qux corge nou)))


  x (sym "=") @y

  syntax-precedence (+) 50

  mac each -> {x y} body
    uniqs i l
      w/complex y
        'let l = y."length"
           for (var i = 0) (i ~= l) (++ i)
             let x = y.i
               body

  syntax-infix ~= 0
  syntax-infix == 0
  syntax-infix + 0
  syntax-infix - 0
  syntax-infix * 0
  syntax-infix / 0

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

  each x = {1 2 3}
    | prn x
    | prn x + 5

  {1 2 3}.for-each -> x
    prn x


  mac loop -> body
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

* Syntax to make common idioms shorter and easier to read

* The compiler is written in JavaScript and is capable of running in a browser: you can incrementally compile/eval Nulan programs at runtime

* Compiles ahead-of-time to extremely fast JavaScript: it should be just as fast or faster than handwritten JS code
