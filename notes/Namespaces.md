Nulan does not have multiple namespaces. Instead, the very core of its design gives you something that is very much so *like* multiple namespaces, but is more consistent, elegant, and faster.

First let's define what the *purpose* of a namespace is. To me, the *one and only* purpose of a namespace is to provide some way of dealing with name collisions. That's it. If you want something more, like a way to write "modular code" or whatever, I would call that a "module". Modules are usually built on top of namespaces, but namespaces are lower-level, dealing only with names.

So, what's the problem with using a single namespace? If you have two files that define the same name, one of the files will overwrite the other one, which is not what you want! You want some way to partition them so they can't interfere with eachother. How do different namespaces handle this partitioning?

Probably the simplest namespace system is user convention. If you have a file called "foo.nu" you can prefix all your globals with `foo-`. This is very simple and can be done in any language, but it is a user convention, and it also makes your code more verbose (and thus harder to read).

As a step up from that, there are symbol-renaming namespaces, which basically function the exact same as user convention, but they *automatically* add in the prefix, so when you load a file "foo.nu" it will automatically add the prefix `foo-`. This is *much* more convenient and gives essentially all the same benefits of user convention, but lets you dynamically choose what the prefix should be.

If you make namespaces into a first-class object in your language, it opens up more possibilities. In Python, for instance, the code `import foo` will load the file "foo" and put it into a namespace object called `foo`. Now you can say `foo.bar` to refer to the global name `bar` in the namespace `foo`.

This is just as verbose as the user-convention system, which is a huge minus, but it includes some of the benefits of symbol-renaming, such as the ability to choose what the prefix should be. It also has some benefits of introspection: you can dynamically check at runtime whether a name exists in a namespace, how many names are in a namespace, etc.

Languages like Racket also have first-class namespaces, but unlike Python, they're just as concise as symbol-renaming. That is, if you have a file "foo" that defines a global name `bar`, if you import "foo" you can just use `bar` directly, no prefixes needed! Racket's namespace system is actually quite powerful: it has introspection (like Python), the ability to exclude names from a namespace, and the ability to rename names.

In Racket's namespace system, a module is usually a file, and each module is run in a separate namespace. What happens if, for instance, a file "foo.rkt" defines a name `yes`, and then something else also defines the name `yes`? In other words...

    # foo.rkt
    (define yes 1)

    # bar.rkt
    (require "foo.rkt")
    (define yes 2)

Does redefining `yes` change anything in "foo.rkt"? Nope. Instead, the name is *shadowed*: within "bar.rkt", `yes` will be `2`, but within "foo.rkt", `yes` will still be `1`. Hm, very interesting... this reminds me of something else... lexical scope!

    $let Yes 1
      $let Yes 2
        ...

Notice that we have a local name `Yes` which is bound to `1`. We then have an inner name `Yes` which is bound to `2`. Does the inner `Yes` overwrite the outer `Yes`? No, it *shadows* it, which means that inside of the inner `$let`, `Yes` will be `2`, but once we move outside of it, `Yes` will be restored to the old value of `1`.

This shadowing is important for preventing name collisions. But Racket achieves this shadowing by running each module in a separate namespace and then using some tricks to get it all to line up right. Can we do better?

It turns out, we can! Looking at the above code, what we're trying to do is take things of the form...

    $def Yes 1
    ...
    $def Yes 2
    ...

...and somehow turn them into this:

    $let Yes 1
      ...
      $let Yes 2
        ...

First, let's talk about how a language like Kernel implements first-class environments. In a language like Kernel which has mutable environments, it's implemented by having the environment be a special data structure which contains a link to the "parent" environment, so the above would look something like this:

    $let Env1: make-env %f
      $set! Env1 Yes 1
      ...
      $let Env2: make-env Env1
        $set! Env2 Yes 2
        ...

In the above, `make-env` is a function that accepts one argument: the parent environment. It returns a new (mutable) environment that inherits from its argument.

We then use `$set!` to mutate the environments returned by `make-env`. When looking up a symbol, it will first check in the current environment, and if it's not found, it will then recursively check the parent until it finds an environment that doesn't have a parent, in which case it will throw an error.

As you can see, in the above implementation, environments can only have a single parent. This is how Kernel (and many other languages with first-class environments) implement environments. It is very simple and works well for most cases.

Because of mutation, it's also possible to write it in a linear style:

    $def! Env: make-env %f
    $set! Env Yes 1
    ...
    $set! Env Yes 2
    ...

This means that the mutable first-class environments in Kernel can be used to solve the problem of namespaces. This is roughly the same as Racket's system, except it's more consistent because it uses the same data structure for both global and local scope.

There are certain issues with this system, though, like how to control things like mutability. Kernel uses the policy that "if you have *direct* access to an environment, you can mutate it, otherwise you can't". This makes certain common idioms *much* more verbose, but it does preserve encapsulation.


Nulan, however, does things very different. In Nulan, environments are immutable, so the idea of "mutating an environment" is impossible. Instead, you create a new environment which is just like the old one but with the changes you want:

    $let Env1: make-env
      $let Env2: $set Env1 X 1
        ...
        $let Env3: $set Env2 X 2
          ...

We first create an empty environment with `make-env` and then use `$set` to add new values to it. Notice that `$set` is functional: it takes an environment as its first argument, a symbol as its second argument, and a value as its third argument... but rather than mutating the environment, it instead returns a *new* immutable environment which is just like the old one, but with the changes applied.

Also notice that this no longer has any notion of "parent" or "child" or anything like that. This means that you can, for instance, merge multiple environments together:

    $let Env4: merge Env1 Env2 Env3
      ...

The above effectively means that `Env4` inherits from *three* parents: `Env1`, `Env2`, and `Env3`. This is just a natural property of immutability, which means generic utilities (like `merge`) can work on environments just as well as other data structures.

Now, that's all well and good, but how does it help us with namespaces? To fix the namespace issue, I'm going to have to introduce a little bit of mutability. All data in Nulan is immutable, with *one* exception: variables.

A variable is essentially a mutable single-celled box which can hold any value. The data that it points to is immutable, but the variable itself can be changed to point to a different immutable object. This accomplishes the same thing as mutable data:

  * With static variables and mutable data, anytime you lookup a variable, it might have changed since the last time you looked.

  * With mutable variables and static data, anytime you lookup a variable, it might have changed since the last time you looked.

Let's rewrite our example to use a single mutable variable:

    $var! Env: make-env
    $set-var! Env: $set Env X 1
    ...
    $set-var! Env: $set Env X 2
    ...

What we've done here is, we have created a new variable `Env` which contains an empty environment. We then use `$set` to return a new environment which is just like the old one but `X` is bound to `1`. We then take that new environment and set the variable to it.

And then later on, we do the same thing, except this time `X` is bound to `2`. The interesting thing about this is... because environments are immutable, this has *exactly* the same shadowing effect as multiple nested `$let`s!

Our goal was to take the linear form of multiple files and somehow express it as nested `$let`s, but we can accomplish the same goal the other way around: if we can express nested `$let`s in a linear style, we can use that for namespaces!

And in fact, that's exactly what Nulan does. There's an implicit (hidden) environment, just like there's an implicit (hidden) continuation. When you use a form that does symbol binding like `$set!` or `$var!` or `$def!`, it will mutate that hidden environment variable.

This means that Nulan runs everything in a single namespace, but it *behaves* like as if each file is running in a separate namespace, because of environment immutability!

Okay, but since environments are immutable, surely that must add restrictions, right? Well, no, because mutation can always be done through variables. The only difference is that in languages like Kernel, environment mutation is the norm, but in Nulan, you have to explicitly create a variable to store the mutable data in.

To demonstrate the power of this namespace system, let's try to accomplish some fairly basic namespace tasks. First off, what if you want to exclude a name from a file? For instance, perhaps the file "foo" creates the name `nou` which you're already using. That's easy to deal with:

    $lets: Top:  get-current-env
           N:    $quote nou
           Old:  (Top) N
      eval Top: $quote $use! foo
      set! Top N Old

But this is clunky and common enough that there's an `$exclude!` vau which does just that:

    $exclude! nou
      $use! foo

And what about selectively importing only certain names from a file? Well, environments are just variables to immutable data, so all we need to do to make a new environment is to wrap an existing data structure in a `var`. We can then evaluate any expression we like in that new environment:

    $lets: Top:  get-current-env
           N:    $quote nou
           Env:  var: Top
      eval Env: $quote: $use! foo
      set! Top N: (Env) N

Once again, this is clunky enough that there's an `$include!` vau:

    $include! nou
      $use! foo

And the above definitions of `$exclude!` and `$include!` don't use any trickery, they do everything using the *exact same* immutable data + mutable variables system that everything else uses, so it's 100% consistent.

This system avoids many of the verbosities of Kernel's system while being easier to reason about, *much* faster, and still retaining tremendous amounts of flexibility. It also integrates in a consistent and seamless way with the rest of the language, which is a huge plus.
