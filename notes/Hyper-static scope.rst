There are different ways to describe and understand the concept of "hyper-static scope".

One way is to view hyper-static scope as being a particular kind of lexical scoping. This is true, but the phrase "lexical scope" itself isn't particularly well defined, so I think this way of thinking is mostly useless, especially because it doesn't help us to understand how hyper-static scope differs from other lexical scoping strategies.

So, let's start with something more concrete. In older languages, especially older Lisps, the concept of dynamic scope was popular. It was easy to implement, and lots of time was invested into making it fast. What is dynamic scope? Rather than covering the text-book definition, I will show an example. First, some JavaScript code::

  var x = 1

  function foo() {
    x = 2
  }

  function bar() {
    var x = 3
    foo()
    return x
  }

The code is fairly straightforward:

- We have a global variable ``x``
- A function ``foo`` that assigns to ``x``
- And a function ``bar`` that creates a local variable ``x``, calls the ``foo`` function, then returns ``x``

Now, the question is, what is the value of the variable ``x``?

- In a dynamically scoped language, ``bar`` would return ``2``, and the global ``x`` would be ``1``

- In a lexically scoped language, ``bar`` would return ``3``, and the global ``x`` would be ``2``

This phenomenom is quite easy to explain with the concept of "environments", which are data structures that map variables to values:

- A language uses dynamic scope if the body of a function is evaluated in the environment where the function is *called*.

- A language uses lexical scope if the body of a function is evaluated in the environment where the function was *defined*.

This is the key distinction between dynamic and lexical scope: whether variables are resolved according to where the function is *called* or *defined*.

Languages like JavaScript use lexical scope for functions, but that's not the whole story... globals are special and are treated different. That is, you can have a function that refers to a global that is not yet defined, and then create the global later::

  function foo() {
    return bar()
  }

  ... many lines later ...

  function bar() {
    return 1
  }

In other words, globals are looked up at the time that the function is *called*, not at the time the function is *defined*, thus globals in JavaScript are dynamic.

Even local variables in JavaScript are a bit funky because of var hoisting. That is, this works::

  function qux() {
    return x
    var x = 5
  }

What happened is that JavaScript re-arranges ``var`` so that it always occurs at the top of the function's body. In other words, the above function was automatically re-written into this::

  function foo() {
    var x
    return x
    x = 5
  }

And so, even though JavaScript's functions use "lexical scope", they treat locals as being almost-kinda dynamic in some ways, and global variables are fully dynamic.

Nulan, however, uses hyper-static scope *everywhere*, at both the global and the local level. Another way of putting it is to say that Nulan enforces strict lexical scope everywhere.

The idea of hyper-static scope is not new (http://c2.com/cgi/wiki?HyperStaticGlobalEnvironment) but most mainstream languages don't support it.

How does hyper-static scope work? Using our previous definitions of dynamic/lexical scope, hyper-static scope simply means that an expression can only refer to variables that exist at the time the expression is defined. As an example, assuming JavaScript used hyper-static scope, these would be errors::

  function foo() {
    return bar()
  }

  function bar() {
    return 1
  }

  function qux() {
    return x
    var x = 5
  }

The function ``foo`` throws an error because at the *time the function is defined*, the variable ``bar`` doesn't exist. And the function ``qux`` throws an error because at the time the expression ``return x`` is defined, the variable ``x`` doesn't exist.

To fix this, you would have to write it like this::

  function bar() {
    return 1
  }

  function foo() {
    return bar()
  }

  function qux() {
    var x
    return x
    x = 5
  }

This has multiple benefits. Firstly, it means that a function can only depend upon things that exist prior to the function. This makes the dependency graph visible in the source code, rather than allowing the programmer to spread dependencies all over the place.

This system is also much faster, because every variable has only a single location, and this location can be determined at compile-time. This is not true with dynamic scope, where variables have to be looked up at runtime.

This system also gives you most of the benefits of multiple namespaces, but with much lower cost and complexity.

To explain further, the fundamental problem that namespaces are trying to solve is "name resolution". Dynamic scope and lexical scope are really just two different algorithms for name resolution. Hyper-static scope is a particular kind of lexical scope, and thus it too is an algorithm for name resolution.

You, as a programmer, want to be able to use the same variable name for two different things. Most languages solve this in one of two ways:

1) Everything is evaluated in a single namespace. This system is used by C, JavaScript, Emacs Lisp, Arc, Scheme, and many others. In these languages, name conflicts are common. That is, if two different programs use the same variable name, one of them will clobber the other, and thus the two programs *cannot* be used together. This has serious implications for libraries, which are supposed to be building blocks that you can freely mix and match together.

   These languages solve the problem by adding a prefix to all the variables that might cause collisions. For instance, if you're writing a library called "foobarqux", which defines the global variables ``yes``, ``no``, and ``maybe``, you might instead call them ``foobarqux_yes``, ``foobarqux_no``, and ``foobarqux_maybe``

   Effectively, by appending a unique identifier to all exposed variables, you prevent name collisions from occuring. This is not without its drawbacks, however. It is very verbose, making code harder to read and write. It also does not solve the problem of two different libraries that use the same prefix. For instance, there might be two different "foobarqux" libraries, which both use the "foobarqux" prefix.

2) Multiple namespaces. These languages have some sort of mechanism that allows code to be run in a new namespace, which is a kind of sandbox, isolating it from all other code. These languages also provide some way to import another namespace into the current namespace.

   Some examples of this system are Python, Node.js, and Racket. Python and Node.js solve this problem with first-class objects and a module loading system that lets you import these objects into a particular variable. For instance, in Node.js::

    var lib1 = require("lib1")
      , lib2 = require("lib2")

    lib1.foo()
    lib2.foo()

    lib1.bar()

   This solves the problem of two libraries using the same prefix, because the prefix is assigned when the library is imported, rather than when it's defined. But it only helps a little with the problem of verbosity: ``lib1.foo`` is the same number of characters as ``lib1_foo``. The only benefit is that you can rename the library to something shorter, like ``a``, in which case you can say ``a.foo``.

   Racket has multiple namespaces, but unlike Python and Node.js, it doesn't use any prefixes at all, and namespaces are not available at runtime. That is, namespaces in Racket are not first-class. The above would be written like this in Racket::

    (require (rename-in lib1 [foo foo1]))
    (require (rename-in lib2 [foo foo2]))

    (foo1)
    (foo2)

    (bar)

   Notice that there is no ``lib1`` or ``lib2`` prefix. You simply use the variables normally, like as if they were in a single namespace. To resolve name conflicts, Racket lets you rename variables. In this case, we're renaming ``lib1``'s ``foo`` to ``foo1`` and ``lib2``'s ``foo`` to ``foo2``.

   The problem that I have with Racket is that it's *very very very* static, complicated, and in my opinion, bloated. I want a system that is as concise and easy to use as Racket's system, but is also easy to implement.

Hyper-static scope gives you most of Racket's namespace system, but for much lower cost. To explain how it works, I like to use the concept of "boxes", even if the implementation doesn't use boxes.

A box is a (possibly mutable) data structure that can only hold a *single* item. At first, this may sound completely useless, but I've found boxes to be very useful, both as concepts, and as actual data structures.

The basic idea is that at compile-time, all variables are replaced with boxes. To explain this, I'll use some Nulan code::

  var foo = 5

  def bar -> foo

  var foo = 10

  bar()

Here we have created a global variable ``foo``, a function ``bar`` that returns ``foo``, another global variable ``foo``, and then we call the function ``bar``. According to hyper-static scope, variables are always resolved according to where they are defined, thus the call to ``bar`` returns ``5``.

Using the idea of boxes, when the compiler encounters ``var foo = 5``, it creates a new box and binds it to the variable ``foo``. Inside the function ``bar``, it replaces the variable ``foo`` with the box.

Then, when the compiler encounters ``var foo = 10``, it creates a new box and binds it to the variable ``foo``, but inside ``bar``, the variable ``foo`` was already replaced with a box, so this has no effect on any previous uses of the variable ``foo``.

Thus, the second ``var`` expression shadows the previous variable: previous uses of ``foo`` will continue to use the old version of ``foo``, but new uses of ``foo`` will use the new version.

This completely solves the namespace problem. Consider two libraries that both define the same name::

  # library 1
  def foo -> 5
  def bar -> foo()

  # library 2
  def foo -> 10
  def qux -> foo()

If you import both libraries, the functions ``bar`` and ``qux`` will refer to the function ``foo`` defined in the library where they were defined. That is, when one library defines a variable ``foo``, it doesn't clobber any already-existing uses of ``foo``, it simply shadows it.

Going back to the example of conflicting libraries, it could be written like this in Nulan::

  import lib1
  var foo1 = foo

  import lib2
  var foo2 = foo

  foo1()
  foo2()

  bar()

As you can see, we're using a plain-old ``var`` to rename the conflicting variables. In languages which use dynamic scope for global variables, when importing the library ``lib2``, it would overwrite the variable ``foo``. But in Nulan, thanks to hyper-static scope, this works.

The above is common enough that Nulan provides a ``rename`` macro which does the same thing::

  rename foo = foo1
    import lib1

  rename foo = foo2
    import lib2
