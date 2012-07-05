Overview
========

For a quick taste of how it looks, here's some standard list utilities written in Nulan:

    # Called map in other languages.
    #
    # It's easier to just define it explicitly, rather than using cons + sumr.
    $def! each
      [X | R] F -> [(F X) | (each R F)]
      X       ~ -> X


    # Called foldl/reduce in other languages.
    #
    # Takes 2 to 3 arguments:
    #
    #   => sum [1 2 3] seq
    #   [[1 2] 3]
    #
    #   => sum 0 [1 2 3] seq
    #   [[[0 1] 2] 3]
    #
    $def! sum
      [X | R] F   -> sum1 X R F
      I       X F -> sum1 I X F

    $def! sum1
      I [X | R] F -> sum1 (F I X) R F
      I ~       ~ -> I


    # Called foldr/rreduce in other languages.
    #
    # Takes 2 to 3 arguments:
    #
    #   => sumr [1 2 3] seq
    #   [1 [2 3]]
    #
    #   => sumr [1 2 3] 0 seq
    #   [1 [2 [3 0]]]
    #
    $def! sumr
      [X | R] F   -> F X: sumr R F
      X       ~   -> X
      X       I F -> sumr1 X I F

    $def! sumr1
      [X | R] I F -> F X: sumr R I F
      ~       I ~ -> I


    # Whee, function composition implemented with sum
    $def! compose
      |Fns -> sum Fns: X Y -> |Args -> X: Y | Args


    # Takes two seqs and returns a single seq.
    #
    # Linear in time to the length of the first argument.
    # This is because it simply conses it directly onto the second argument.
    # This is okay because Nulan doesn't have seq mutation.
    $def! join
      [X | R] Y -> [X | (join R Y)]
      [X]     Y -> [X | Y]

Features
========

* A full-blown Lisp, which means programs are parsed as S-expressions, and there's a very heavy emphasis on "code is data is code"

* Computation based on vau rather than lambda

* Pattern matching for vau arguments

* First-class environments

* A very simple (yet powerful) module system based on first-class environments

* Emphasizes functional programming (specifically, referential transparency), but still has some support for side effects, which means Nulan is an impure functional language

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

5. http://www.dwheeler.com/readable/
  * For the idea that a clean, readable, general Lisp with syntax and less parens is possible. The parser Nulan uses is written from scratch and is a bit different from the parser described in that page: in particular, Nulan is missing curly infix and modern expressions. In addition, two newlines in a row only end the expression in the REPL, not in a file

6. Ruby
  * For being awesome all-around, major props to Matz and everybody who made Ruby awesome
  * For making me appreciate clean syntax more, which inspired me to get rid of as many parens as I could in Nulan

7. Scheme
  * No ideas were taken directly, but Scheme has had a massive impact on other languages like Kernel, and so there's an indirect debt

8. Lisp
  * For the original idea of "code is data is code", which in turn inspired a wide variety of different Lisp dialects, all of which are interesting in their own ways. No Lisp means no Scheme which means no Arc/Kernel/Common Lisp/Shen which means no Nulan

9. PyPy
  * For being awesomesauce, even if it *is* written in a subset of Python, rather than, say, Ruby. Without PyPy (and to a much lesser degree Python), Nulan wouldn't have gotten as far as it did

10. Python
  * I am not fond. But it's what PyPy uses, and it's better than Java, at least...

Great, but how does it all work?
================================

* Getting started explains how to set up the REPL.

* The beginner tutorial is for people who have very little to no experience with Lisp or functional programming.

* The intermediate tutorial is for people who have some experience with Lisp.

* The advanced tutorial is for people who've read the intermediate tutorial.
