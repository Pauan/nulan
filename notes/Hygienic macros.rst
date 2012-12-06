Over in the Lisp world, macros are very highly valued. After all, they are one of the few major benefits that Lisp has that most non-Lisps don't.

However, there is a great divide between the Scheme camp and the Common Lisp camp. The Scheme camp believes that macros should be hygienic, whereas the Common Lisp camp prefers the raw power and simplicity of unhygienic macros.

These two camps appear at first to be irreconcilable, with both sides thinking the other side is wrong.

It turns out, however, that it's possible to reconcile this difference. This idea was not originally developed by me, but has been independently discovered by various people, including John Shutt who created Kernel, and Andrew Wilcox on the Arc Forum.

To explain further, I will need to go into details as to what "hygiene" is, why the Scheme camp wants it, and why it is more complicated than the unhygienic macros of Common Lisp.

I'm not a fan of text-book definitions, so instead I'll give an example to demonstrate what a *lack* of hygiene is, using the Arc language::

  (mac foo (y)
    `(let x 10
       (+ x ,y)))

  (foo 20)

The above creates a macro ``foo`` that replaces ``(foo y)`` with ``(let x 10 (+ x y))``

We then call the macro, which will be expanded to ``(let x 10 (+ x 20))``.  This works just fine and returns ``30``

But now consider this::

  (let x 50
    (foo x))

The above will not return ``60`` as you might have expected, but instead returns ``20``. If we expand the macro, it becomes obvious why::

  (let x 50
    (let x 10
      (+ x x)))

This unexpected behavior, which is called a "hygiene violation" can be solved through the use of a unique variable that is guaranteed to not conflict with any other variable. Using a unique variable, the macro ``foo`` can be rewritten to this::

  (mac foo (y)
    (w/uniq x
      `(let ,x 10
         (+ ,x ,y))))

And now everything works correctly. But there is still a problem with the macro::

  (let + "a string"
    (foo 20))

The above throws an error. Expanding it, we can see why::

  (let + "a string"
    (let x 10
      (+ x 20)))

In this case, we're using ``let`` to change the definition of ``+``, which certainly isn't what we were expecting to happen! In general, this kind of hygiene violation **cannot** be solved in Arc.

The Scheme macro system uses syntax objects and all kinds of buffoonery to fix these hygiene problems, but the end result is a system that is **very** complicated.

This problem of correctness vs simplicity is at the core of the Scheme vs Common Lisp debate. However, after understanding the problem better, it is possible to design a system that is even simpler than Common Lisp macros, yet has the same correctness as Scheme macros.

But first, I would like to point out that this problem of "hygiene" is not specific to Lisp macros. In particular, the same exact thing happens with functions in languages that use dynamic scope. As explained in `hyper-static scope <Hyper-static%20scope.rst>`_, the definition of dynamic vs lexical can be summed up like this:

- A language uses dynamic scope if the body of a function is evaluated in the environment where the function is *called*.

- A language uses lexical scope if the body of a function is evaluated in the environment where the function was *defined*.

Of course, we're using macros rather than functions, but the principle is the same. The problem in all of the examples above is that variables like ``x``, ``let``, and ``+`` are resolved according to where the macro is *called*. We want it to resolve the variables where the macro is *defined*.

So "lack of hygiene" really just means "dynamic scope" and "hygienic macros" really just means "lexically scoped macros". This alone is a massive clarification, and I think it's a shame that the Lisp community has treated hygiene as a separate topic, rather than a specific example of dynamic vs lexical.

Now that we understand the problem that we're trying to solve, how do we actually solve it? Scheme does so with syntax objects that keep track of the lexical information, but we don't want such a complicated system.

Macros are really just functions that accept an S-expression and return an S-expression. And functions are lexically scoped. So why can't we use the lexical scope of functions to make macros lexically scoped?

The problem is that macros are written using ``quote``, which returns an unevaluated expression. In particular, in the expression ```(let x 10 x)``, the symbols ``let`` and ``x`` are not evaluated. The macro is then expanded, and the symbols are evaluated in the environment where the macro is called.

So, why don't we just use ``,`` to evaluate everything in the environment where the macro is defined? In other words, we would write ```(,let ,x ,10 ,x)``. So, that's it, right? We just don't use ``quote`` and we get lexical scope, right? Well, yes, with a few caveats:

- In Scheme, you can't access the *value* of a macro, so macros **must** be referred to with a symbol.

  Arc, thankfully, doesn't have this problem, but the compiler assumes that macros are always symbols. However, it's *very very* easy to change the Arc compiler so it accepts macros as values.

  It would also be easy to change Scheme so that it's possible to access the value of a macro.

- With this new system, macros would be completely static. For instance, consider this macro::

    (def foo () 5)

    (mac bar ()
      `(,foo))

  If we call ``(bar)`` it'll correctly return ``5``. But if we now redefine ``foo``...

  ::

    (def foo () 10)

  ...then ``(bar)`` will still return ``5``, rather than ``10``. Nulan solves this problem by having every variable be a *box* rather than a *value*. So the macro ``bar`` inserts a box that refers to ``foo``, rather than inserting ``foo`` directly. And now any changes to the box will show up in the macro.

- Having to use ``,`` on every variable is not only very verbose and ugly, but it's error-prone: if you forget to use it, your macro is now dynamically scoped! It would be better to have ``,`` be the default, so you can write ```(let x 10 x)`` and have it be lexical. This is an easy change to make, but a lot of people are used to having ``quote`` be the default, so there would be a painful transition period.

- Having macros be lexically scoped by default is great, but there are some situations where you intentionally want a variable to be dynamic. If ``,`` is the default, then how do you write unhygienic macros? The answer is actually really simple::

    `(let ,'x 5 ,'x)

  What the above does is, it uses ``,`` to splice in an evaluated expression. That evaluated expression happens to be the unevaluated symbol ``x``. Thus, it only takes two characters to make a variable dynamically scoped, and thus "unhygienic macros" are very easy to write in this system, so you don't lose any power compared to Arc/Common Lisp macros.

Nulan implements all of the above changes, except:

- There is no ````` operator, only ``'``, and ``'`` evaluates symbols::

    '(foo bar qux) -> (list foo bar qux)

- The ``'`` operator supports ``,`` just like ````` in other Lisps::

    '(foo (bar) qux)  -> (list foo (list bar) qux)
    '(foo ,(bar) qux) -> (list foo (bar) qux)

- If you want to write an "unhygienic macro", you need to use the ``sym`` function, which converts a string to a symbol::

    '(let ,(sym "x") 5 ,(sym "x"))

    (let x (sym "x")
      '(let x 5 x))
