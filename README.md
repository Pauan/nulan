Overview
========

Nulan is a Lisp with syntax, pattern matching, and fexprs.

For a quick taste of how it looks, here's some standard list utilities:

    $def! map: type list? fn?
      [X | R] F -> [(F X) | (map R F)]
      X       ~ -> X

    $def! each: type list? fn?
      [X | R] F -> (F X) : each R F

    $def! fold: type list? fn?
      [X Y | R] F -> fold [(F X Y) | R] F
      X         ~ -> X

    $def! foldr: type list? fn?
      [X Y | R] F -> F X: F Y: foldr R F
      X         ~ -> X

    $def! join: type list? ~
      [X | R] Y -> [X | (join R Y)]
      [X]     Y -> [X | Y]

If you've used a Lisp before, you might have noticed something: there's very few parens in the above code!

This is because Nulan uses significant whitespace (like Python and Haskell) and two special syntax operators: `:` and `;`.

This lets you write Lisp code in a style that has very few parens, but has a very simple translation to S-expressions, which means it works predictably with macros and fexprs.

Great, but how does it all work?
================================

* Getting started
  Explains how to set up the REPL.

* Beginner tutorial
  For people who have very little to no experience with Lisp or functional programming.

* Intermediate tutorial
  For people who have some experience with Lisp.

* Advanced tutorial
  For people who've read the intermediate tutorial.
