Overview
========

For a quick taste of how it looks, here's some standard list utilities written in Nulan:

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

Features
========

* A full-blown Lisp, which means programs are parsed as S-expressions, and there's a very heavy emphasis on "code is data is code"

* Computation based on vau rather than lambda

* Pattern matching for vau arguments

* First-class environments

* A very simple (yet powerful) module system based on first-class environments

* Emphasizes functional programming (specifically, referential transparency), but still has some support for side effects

* Lots of syntax! This allows you to write programs in a shorter and clearer way

Planned features
================

* A simple, easy, yet powerful way for users to create new data types that can be constructed, inspected, and used in pattern matching. It'll probably be based on algebraic data types, or similar

* Overloaded `$set!` that works on any compound data structure

* Putting data structures (like hash tables, lists, etc.) into functional position

Acknowledgements
================

Although Nulan has been entirely my own work, I have taken many ideas from a large variety of other people, and I am deeply indebted to that. Here are some of the projects that inspired Nulan, listed from most inspiring to least:

1. Kernel
  * Using vau rather than lambda as the basis of computation
  * Explicit `eval` rather than implicit `eval`
  * First-class environments
  * Using `$` to indicate vaus
  * Using `!` and `?` more consistently than in other languages like Scheme

2. Shen
  * Pattern matching in vau arguments
  * `->` syntax for functions
  * `[ | ]` syntax for lists
  * The idea that a blend of Lisp and functional programming (Haskell, SML, etc.) is possible, clean, and elegant

3. Arc
  * The names of the operators `$def!`, `$fn`, `keep`, `rem`, `each`, `do`, `is?`, and `iso?`
  * The general idea of "more concise code is usually better"
  * `:` syntax, though Nulan's syntax is *much* more powerful than Arc's
  * Overloaded `$set!` that works on any compound data structure
  * Putting data structures (like hash tables, lists, etc.) into functional position

4. Clojure
  * Generic sequence type that is used uniformly everywhere
  *

5. http://www.dwheeler.com/readable/
  * For the idea that a clean, readable, general Lisp with syntax and less parens is possible. The parser Nulan uses is actually very different from the parser described in that page

6. Ruby
  * For being awesome all-around
  * For making me appreciate clean syntax more, which inspired me to get rid of as many parens as I could in Nulan

7. Scheme
  * No ideas were taken directly, but Scheme has had a massive impact on other languages like Kernel, and so there's an indirect debt

8. Lisp
  * For the original idea of "code is data is code", which in turn inspired a wide variety of different Lisp dialects, all of which are interesting in their own ways

9. PyPy
  * For being awesomesauce, even if it *is* written in a subset of Python, rather than, say, Ruby

Great, but how does it all work?
================================

* Getting started explains how to set up the REPL.

* The beginner tutorial is for people who have very little to no experience with Lisp or functional programming.

* The intermediate tutorial is for people who have some experience with Lisp.

* The advanced tutorial is for people who've read the intermediate tutorial.
