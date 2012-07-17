Nulan does not have namespaces. Instead, the very core of its design gives you something that is very much so *like* namespaces, but is more consistent, elegant, and faster.

First let's define what the *purpose* of a namespace is. To me, the *one and only* purpose of a namespace is to provide some way of dealing with name collisions. That's it. If you want something more, like a way to write "modular code" or whatever, I would call that a "module". Modules are usually built on top of namespaces, but namespaces are lower-level, dealing only with names.

So, what's the problem with using a single namespace? If you have two files that define the same name, one of the files will overwrite the other one, which is not what you want! You want some way to partition them so they can't interfere with eachother. How do different namespaces handle this partitioning?

Probably the simplest namespace system is user convention. If you have a file called "foo.nu" you can prefix all your globals with "foo-". This is very simple and can be done in any language, but it is a user convention, and it also makes your code more verbose (and thus harder to read).

As a step up from that, there are symbol-renaming namespaces, which basically function the exact same as user convention, but they *automatically* add in the prefix, so when you load a file "foo.nu" it will automatically add the prefix "foo-". This is *much* more convenient and gives essentially all the same benefits of user convention, but lets you dynamically choose what the prefix should be.

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

One major difference between Nulan and Racket is that although Racket has first-class namespaces (global scope) it doesn't have first-class environments (local scope). Nulan, however, uses the *exact same* environments for both global and local scope.

First, let's talk about how a language like Kernel implements first-class environments. In a language like Kernel which has mutable environments, it's implemented by having the environment be a special data structure which contains a link to the "parent" environment.

So the above would look something like this:

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
      #eval Top [$set! N Old]
      set! Top N Old

But this is clunky and common enough that there's an `$exclude!` vau which does just that:

    $exclude! nou
      $use! foo

And what about selectively importing only certain names from a file? Well, environments are just variables to immutable data, so all we need to do to make a new environment is to wrap an existing data structure in a `var`. We can then evaluate any expression we like in that new environment:

    $lets: Top:  get-current-env
           N:    $quote nou
           Env:  var: Top
      eval Env: $quote: $use! foo
      set! Top N: Env N
      #eval Top [$set! N (Env N)]

Once again, this is clunky enough that there's an `$include!` vau:

    $include! nou
      $use! foo

And the above definitions of `$exclude!` and `$include!` don't use any trickery, they do everything using the *exact same* immutable data + mutable variables system that everything else uses, so it's 100% consistent.

This system avoids many of the verbosities of Kernel's system while being easier to reason about, *much* faster, and still retaining tremendous amounts of flexibility. It also integrates in a consistent and seamless way with the rest of the language, which is a huge plus.



First off, I must explain that in Nulan, *all* data structures are immutable. So, if you have a sequence or a dictionary and you wish to "add" new items to it, you must create a new sequence/dictionary that is just like the old one but with the changes added. I won't go into all the details, but if you want to know more, you can _go here_.

Anyways, all environments in Nulan are also immutable, because they're implemented with the same data structure as everything else. One important property of immutable data structures is that *things are always shadowed, never mutated*. To understand this, let's talk about lexical scope:

This principle of shadowing is very important to namespaces, as we shall soon see. How is this shadowing normally implemented?

Okay, big deal, right? What does this have to do with namespaces? To understand that,




This is trivial to implement with immutable environments: the environment parameter of a vau is

So, consider these three files:

    # foo.nu
    $def! foo
      ...

    $def! corge
      ...


    # bar.nu
    $def! bar
      ...

    $def corge
      ...


    # qux.nu
    $use! foo bar

When you load up "qux.nu", it will first load "foo.nu", which will define



This is very fast because data structures in Nulan are implemented with binary trees and they're http://en.wikipedia.org/wiki/Persistent_data_structure persistent. In this case, "persistent" doesn't mean, "will be saved to the harddrive". Instead, it means "old versions of the data structure are still available when creating new versions". This could be implemented by simply copying the data every time you make a change, but that's ridiculously inefficient. Instead, Nulan takes advantage of the tree structure to *share data between versions*.

Now, this has many very interesting consequences. One of those consequences is that because environments are implemented with the same data structures as everything else, environments are also immutable and persistent.

This means that forward references aren't possible. Consider a function `foo` which calls the function `bar`, which in turn calls `foo` again:

    $def! foo
      ... (bar ...) ...

    $def! bar
      ... (foo ...) ...

What happens is that at the time that `foo` is defined, its lexical scope doesn't include a reference to `bar` in it, so when trying to call `foo`, it will throw an "undefined reference" error.

Essentially, to create a new reference in an environment, you "cons" the new reference onto the old environment. This means functions can only refer to things defined earlier in the environment, which in turn prevents forward references.

Why would you want to program in a language like this? One of the issues I have in languages like Arc is that (because forward references are possible) it's sometimes very difficult to understand the function dependencies and the program's flow. Rather than understanding a program as a series of simple steps, instead you have to jump around all over the place to figure out what's going on.

Another benefit is that it is consistent with how all the other data structures work in Nulan, and I value consistency, provided it makes the programs simpler to understand.

Another benefit is that because the lexical environment of a function *never ever ever changes*, you can potentially write a compiler/interpreter that would take advantage of this fact to generate very fast code. This is either impossible or at least very difficult in a language like Kernel, because Kernel has mutable environments.

So, to summarize, immutable environments are consistent with the other data structures, they help make a program more linear (and thus easier to understand), and they potentially allow for some awesome optimizations to make things go fast (which is important because languages using vau are normally seen as much slower than languages with macros, but that doesn't necessarily have to be so).

But you don't care about all that, right? You see the lack of forward references as a problem. A restriction. How dare Nulan prevent you from writing your code however you want to?! Well, it turns out, I told a small lie. It *is* possible to define a forward reference, you just have to be explicit about it:

    $var bar

    $def! foo
      ... (bar ...) ...

    $def! bar
      ... (foo ...) ...

You see what happened? I just added the line `$var bar`. Now, Nulan has a concept of "variable". A variable is basically a one-cell mutable box. It is the only mutable data structure in Nulan.

So, rather than having static variables that refer to mutable data, instead Nulan has mutable variables that refer to static data. The end result is the same, but you get all the benefits of concurrency and simplicity of immutable data.

So, in the above code, we first define a variable `bar`. The variable can be set to different things, but the things it points to are always immutable (well, unless you have a variable that points to a variable...)

Now, the function `foo` has a reference to the variable `bar`, which is currently not assigned to anything. And when we define `bar`, what Nulan does is, it first checks to see if `bar` is a variable. If it is, it will update the reference. Otherwise, it will do the usual "cons onto environment" thing.

This means that I've effectively specified that the variable `bar` is now mutable, whereas before it was immutable. Using this technique, forward references are now possible. In and of itself, this might not seem like a big win... I mean, sure, immutable environments *might* make it easier to reason about programs, and it *might* give some speed boosts, but gosh having to explicitly mark references as immutable is a damn pain, right?

There is one other benefit that I haven't mentioned yet... and that is namespaces. I won't bore you with the details, but I've thought about namespaces a fair amount both before and during the development of Nulan. I thought about systems like symbol renaming, but ultimately rejected them in favor of using first-class environments.

Originally Nulan had mutable environments like Kernel, which raised all sorts of questions with regard to namespaces. Consider these three files:

    # foo.nu
    $def! foo
      ...


    # bar.nu
    $def! bar
      ...


    # qux.nu
    $use! foo bar

Here, the file "qux.nu" imports the files "foo.nu" and "bar.nu". "qux.nu" should now be able to use the references `foo` and `bar` freely, like as if everything was in a single namespace.

But what if you have a file which doesn't initialize its top-level variables right away? For instance, it might have an `init` function which will do the setup:

    # foo.nu
    $def! init; ->
      $def! foo
        ...

Now, when you import "foo.nu", the reference `foo` doesn't exist. Instead, you have to call `init` which will create a new reference `foo`. If the import mechanism merely makes a *copy* of the file it's importing, then it won't notice the new reference `foo`.

But if it doesn't make a copy, that implies some sort of inheritance scheme. That is, "qux.nu" will now inherit from "bar.nu" and "foo.nu". This then means that environments need the ability to inherit from multiple parents...

It would have been possible to code something like that up, but it was a very clunky idea. I didn't like it. It felt way too hard to reason about and wasn't consistent at all with the rest of Nulan.

Let's talk for a moment about what the *purpose* of a namespace system is. My idea is that the *one and only* purpose of a namespace system is to prevent name collisions. It isn't to make your code more "modular", it isn't to help with organization. It's solely to allow different namespaces to use the same name without colliding. That's it.

If you want something more than just namespaces, something that helps with modularity or whatever, I would call that a "module". Modules almost always *use* a namespace system underneath, but the namespace system is lower-level. That is, modules are built on top of namespaces. Let's focus on namespaces right now.

Let's suppose that "foo.nu" and "bar.nu" both define something with the same name:

    # foo.nu
    $def! corge
      ...

    # bar.nu
    $def! corge
      ...

Now, in most languages that have a single namespace, what would happen is that you would import "foo.nu", which would define `corge`. Then you would import "bar.nu" and it would *overwrite* `corge`. Now any references to `corge` will use the "bar.nu" version, *including* any references to `corge` in "foo.nu"!

This means that because everything is in a single mutable namespace, one file can clobber what another file does! This is the whole reason why we design multiple namespace systems, to prevent one namespace from clobbering something in another namespace.

So, let's suppose that "foo.nu" is run in its own separate little isolated namespace, and "bar.nu" is run in its own separate little isolated namespace. They both define a reference `corge`, but because they're in separate namespaces, they don't clobber eachother.

This is great so far, but what happens when "qux.nu" imports "foo.nu" and "bar.nu"? What I would expect to happen is that whichever namespace is imported last would win. That is, inside of "qux.nu", references to `corge` would use the "bar.nu" version.

But references to `corge` inside "foo.nu" would still use the "foo.nu" version. So, you could say that "bar.nu" *shadowed* "foo.nu". Where is another area where shadowing is common...? Ah, yes, lexical scopes!

    $let X 1
      $let X 2
        prn! X
      prn! X

The above creates a local reference `X` which is bound to `1`. It then creates *another* local reference `X` which points to `2`. It then prints out the two `X`s. Now, the important thing to notice here is that the inner `X` doesn't overwrite the outer `X`: it *shadows* it. Within the inner `$let`, `X` will be `2`, but inside the outer `$let`, `X` will be `1`, thus the above will first print `2`, and it will then print `1`.

Now, my plan was to use the *exact same environments* for lexical scope and global scope, just like Kernel. That means that the above will use immutable persistent environments, so creating a new environment binding just means consing it onto the outer environment. You can visualize it like this:

    $let Env []
      $let Env (set Env [X 1])
        $let Env (set Env [X 2])
          prn! (get Env X)
        prn! (get Env X)

In other words, we have an environment, and to make a "change" to that environment, we instead cons up a new environment with the changes added. Because of this, earlier bindings aren't affected at all, they're only shadowed. Lexical scope occurs *naturally* in a language with immutable environments!

And the global scope works in *exactly the same way*. Now, going back to what I talked about, you could say the whole point of a namespace system is that if two namespaces define a reference with the same name, it'll be *shadowed* rather than *overwritten*. But that shadowing is the entire point of immutable environments!

Going back to the example before...

    # foo.nu
    $def! corge
      ...

    # bar.nu
    $def! corge
      ...

    # qux.nu
    $use! foo bar

When you load up "qux.nu", it'll first load up "foo.nu" which will define a reference `corge`. Now it'll load up "bar.nu" which also defines a reference `corge`, but *because environments are immutable* it doesn't mutate the existing reference to `corge`, but instead creates a shadowed version of it.

This does *exactly the same thing* that a multiple namespace system does, except everything is running in a single namespace! The reason it works is simply because immutable persistent data structures always work by *shadowing old values* rather than *mutating them*. So we get the same results as a multiple-namespace system, but we get it for free *simply by using immutable environments*.

But one of the nice things about multiple namespaces is that they don't *only shadow* values, they also let you mutate them too! This might be useful to, for instance, fix a broken library that you can't fix directly. You can instead import the library and then mutate things to fix it.

It's also useful for things like types. If a library is designed to work on certain types, if you define a new type and want to use that type with the library, you need some way of making the library aware of your type. That requires some form of mutation.

But Nulan already has a general-purpose data structure for mutation: variables. If a file defines a variable, then later uses of it will be *mutated* rather than *shadowed*. So although Nulan uses immutable references by default, you can *selectively* specify that certain references are to be mutable. This gives remarkably fine-grained control over how to handle conflicts.

That's all well and good, but what if the author of a file just didn't think that anybody would want to mutate some reference, but it turns out somebody did. In other words, what if a file *doesn't* mark something as a variable, but you still want to mutate it? How do you deal with that situation?

It's actually very easy. If you define something as a variable *before* importing a file, the file will use the mutable variable:

    # qux.nu
    $var! corge
    $use! foo bar

Now `corge` is defined as a variable, so when "bar.nu" is imported, it'll overwrite the `corge` defined in "foo.nu". What if you want to exclude a name from a file? For instance, perhaps the file creates the name `nou` which you're already using. That's easy to deal with too:

    $lets: Top:  get-current-env
           N:    $quote nou
           Old:  Top N
      $use! foo
      eval Top [$set! N Old]

But this is clunky and common enough that there's an `$exclude!` vau which does just that:

    $exclude! nou
      $use! foo

And what about selectively importing only certain names from a file? It's possible to create a new reference to an environment that inherts from the current environment and then load the file in it:

    $lets: Top:  get-current-env
           N:    $quote nou
           Env:  make-env Top
      eval Env: $quote: $use! foo
      eval Top [$set! N (Env N)]

Once again, this is clunky enough that there's an `$include!` vau:

    $include! nou
      $use! foo

This means that everything can run in a single namespace, with *all* the benefits of multiple namespaces, and with plenty of flexibility on how to handle name conflicts.

And the above definitions of `$exclude!` and `$include!` don't use any trickery, it does everything using the *exact same* immutable data + mutable variables system that everything else uses, so it's 100% consistent.



What if you want to be selective, like using a reference only in one file? You can do this:

    # qux.nu
    $var! corge
    $use! foo
    $del! corge
    $use! bar

This will create a variable `corge`, load "foo.nu" with the variable `corge`, then it will delete `corge` so that `bar` will create a new shadowed variable rather than mutating `corge`.
