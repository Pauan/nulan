Overview
========

Nulan is a Lisp with syntax, pattern matching, and fexprs.

For a quick taste of how it looks, here's some standard list utilities:

    $def! map: type list? fn?
      [X | R] F -> [(F X) | (map R F)]
      X       ~ -> X

    $def! each: type list? fn?
      [X | R] F -> F X; each R F

    $def! fold:  type list? fn?
      [X Y | R] F -> fold [(F X Y) | R] F
      X         ~ -> X

    $def! foldr: type list? fn?
      [X Y | R] F -> F X: F Y: foldr R F
      X         ~ -> X

    $def! join: type list? ~
      [X | R] Y -> [X | (join R Y)]
      [X]     Y -> [X | Y]

If you've used a Lisp before, you might have noticed something: there's very few parens in the above code!

This is because Nulan uses significant whitespace (like Python and Haskell) and has two special syntax operators: `:` and `;`

This lets you write Lisp code in a style that has very few parens, yet it has a simple translation to S-expressions, which means it works predictably with fexprs (or macros).

Great, but how does it all work?
================================

* Getting started explains how to set up the REPL.

* The beginner tutorial is for people who have very little to no experience with Lisp or functional programming.

* The intermediate tutorial is for people who have some experience with Lisp.

* The advanced tutorial is for people who've read the intermediate tutorial.
