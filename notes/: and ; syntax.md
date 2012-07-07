The `:` and `;` syntaxes have undergone an awful lot of change, evolution, redesign, and experimentation. This page attempts to describe some of these past experiments, why they were tried, and why they were rejected.

 1. The original idea came from Arc, which uses `:` to mean function composition. That is, in Arc, `(foo:bar:qux 1)` is exactly the same as `(foo (bar (qux 1)))`. This is extremely convenient and I use it a lot in my Arc code.

    My belief is that a Lisp should be very frugal in parens. This keeps it lightweight, cleaner, easier to read, and overall a lot more fun to write. It's one of the major reasons I prefer Arc over, say, Scheme:

        (let a 5 a)                        ; Arc
        (let ((a 5)) a)                    ; Scheme

        (with (a 5 b 10) a)                ; Arc
        (let ((a 5) (b 10)) a)             ; Scheme

        (if a 1 b 2 c 3 d)                 ; Arc
        (cond (a 1) (b 2) (c 3) (else d))  ; Scheme

    There are some issues with the `:` syntax in Arc, though. Firstly, it is known as "ssyntax", which I'm guessing is short for "symbol syntax". This is an appropriate name because the `:` syntax can only occur within symbols. So, you can't say `(foo : bar : qux 1)` for instance.

    The other issue is that, because it's "ssyntax", it's handled at compile-time rather than at read-time. This means that, for instance, macros get the unexpanded symbols. On the one hand, this gives macros more power, since they can give their own special meaning to `:`. On the other hand, it's confusing for the programmer because they probably weren't expecting some random macro they use to suddenly hijack the language's syntax!

    Arc also hardcodes the *symbol* `compose` into the ssyntax, so that `foo:bar` expands to `(compose foo bar)` rather than `(#<fn:compose> foo bar)`. That is, it inserts the symbol rather than the actual function itself. This means that you can change the meaning of the `:` syntax locally:

        (let compose (fn (x y)
                       (fn (z) 5))
          foo:bar)

    The above doesn't compose `foo` and `bar` like you might expect, instead it returns a function that, when called with a single argument, always returns `5`. Just like allowing macros to change the meaning of the `:` syntax, this appears to give the programmer more power. It also makes it quite easy to screw things up and break things. This is consistent with Arc's philosophy that the programmer is smart, so just let them deal with it.

    This leads me to the first implementation of the `:` syntax in Nulan...

 2. Like Arc, Nulan generally tries to give the programmer maximum power, on the basis that good programmers need good tools to do good work, and bad programmers are going to screw up no matter how much you baby them, so you might as well just assume that the programmer is smart.

    Unlike Arc, I care about making dangerous things difficult to do *by accident*. They should be possible, of course, just not easy to do accidentally. In this case, "dangerous" means "anything the programmer didn't expect".

    Randomly deleting a file for no reason would count as "dangerous". Having the `:` syntax expand to the symbol `compose` also counts, because the programmer probably wasn't expecting that `(let compose ...)` would redefine the meaning of the `:` syntax!

    So I decided to solve all the above problems with Arc's syntax. Rather than being handled at compile-time, the `:` syntax would be expanded at read-time, which means it behaves consistently with all the other syntax in the language.

    And because it's handled by the reader, it allows for things like this: `(foo 1 : bar 2 : qux 3)` which is read as `(foo 1 (bar 2 (qux 3)))`. This is strictly more powerful than Arc's syntax in the common case, and is *very* similar to the Unix shell pipe `|`, but backwards.

    As for locally redefining syntax, I believed that it should be possible. Just not easy to do by *accident*. So rather than having the `:` expand to the symbol `compose`, it instead expanded to the symbol `&colon`, so that `(foo : bar)` would expand to `(foo (&colon bar))`.

    I named it `&colon` because I didn't want any name collisions. In Nulan, `&` was originally used for "special variables", things that had a special meaning. This meant that you were free to redefine the variable `&colon`, but you couldn't do so accidentally, because the `&` was a visual signal that something funky was going on.

    For a while, I thought this was a good idea, but after giving it much thought, I instead decided that allowing users of the language to change the meaning of syntax is a stupid and wrong idea.

    Now, don't get me wrong, allowing the user to change the syntax of the language is a fantastic idea, something which Nulan embraces. The problem with `:` expanding into `&colon` is that it lets you change the *meaning* of the syntax, but not the syntax *itself*.

    Chances are, if you want to change the meaning of the syntax, you probably also want to change other things: add new syntax, remove syntax, or modify syntax. You can't do that with just `&colon` because `&colon` only defines the *meaning* of the syntax.

    So I then decided that the best way to handle it is to make it easy for users of the language to define custom readers, which can then be plugged into Nulan. So, for instance, somebody might write a library (in Nulan) which defines a custom reader. You can then import that library, plug in the reader, and bam, everything afterwards is read according to the library's reader, rather than Nulan's reader.

    This lets users change not only the meaning of syntax, but also the entire syntax itself. As an example, there could be a library which provides a reader that lets you write Nulan code in a style similar to C, with curly brackets delimiting blocks and everything. Or how about Haskell syntax? Standard ML? Or maybe CoffeeScript, Arc, Ruby, Python, or some crazy new language we don't even know about yet. This is *massively* more powerful than just changing the meaning of `&colon`.

 3. At this point, I had a very powerful `:` syntax which lets me get rid of many parens. But I discovered a use-case that the `:` syntax wasn't powerful enough to accomodate:

        $assign foo
          $vau Env [X Y]
            ...

    I really wanted to be able to write that like this:

        $assign foo: $vau Env [X Y]
          ...

    But alas, the `:` syntax ended at the end of the line, which means it would be read as `($assign foo ($vau Env [X Y]) ...)` which isn't what I wanted. So I devised a new syntax, called `\`. Using it, the above example could be written like this:

        $assign foo \ $vau Env [X Y]
          ...

    With some tweaking, the rules for `\` and `:` were both starting to get a little complicated. After lots of thought, I suddenly came to a massive realization: `:` and `\` are opposites of eachother. Consider this code snippet:

        $assign foo \ $vau Env [X Y]
          $let: bar 1
                qux 2
            corge \ yes
                    nou
            ...

    Which was then read as:

        $assign foo ($vau Env [X Y]
          $let (bar 1
                qux 2)
            corge (yes)
                  nou
            ...)

    The rule for `:` was: take everything on the same line and to the lower-right of `:` and place it into a list.

    The rule for `\` was: take everything on the same line and to the lower-left of `\` and place it into a list.

    There was an inherent symmetry there, which I found *extremely* compelling and super awesome. I later realized that using `;` rather than `\` looked a lot nicer, and had the extra advantage that the comma part of `;` points to the left, which is a subtle reminder that it collects everything to the *left* into a list.

    Unfortunately, the rules for `:` and `;` relied upon *indentation*, which means that when I tried to use them inside `()` and `[]`, they had different meanings! This inconsistency really bugged me, so I tried out other schemes.

 4. One day, I stumbled upon a realization. I could define `:` to mean "collect everything between the : and the end of the list into a list". And `;` would behave like a line break, with the added bonus of terminating the closest `:`. Then the above example could be written like so:

        $assign foo: $vau Env [X Y]
          $let: bar 1
                qux 2;
            corge: yes;
                   nou
            ...

    This approach had some issues... the most major one was that `;` only closed the nearest `:`. This is most noticable with certain constructs, like `$lets`:

        $lets: foo: bar;;
               qux: corge;
               nou: maybe: something;;
          ...

    Notice how `;` had to be doubled, which I found to be extremely ugly, so I dismissed this approach.

 5. But it sparked an idea, that `:` and `;` should be used solely to manipulate the indentation, and thus shouldn't be allowed inside `()` and `[]`. After some playing around, I settled on the following definition:

    `:` will take everything to the right of it, shift it down by one line, and add one indent level

    `;` will take everything to the right of it, shift it down by one line, keeping it at the same indent level

    Basically, that means that this:

        foo; bar
          qux: corge
               nou; yes
          maybe

    Would be read as:

        foo
        bar
          qux
            corge
            nou
            yes
          maybe

    In this case, `bar` was shifted down one line and put at the same indent level as `foo`, `corge` and `nou; yes` was shifted down one line and indented one further than `qux`, and `yes` was shifted down one line and put at the same indent as `nou`.

    As a special case, if `:` appeared at the end of the line, it would take everything indented further than the current line and shift it one more indent:

        foo: bar:
          qux corge
            yes maybe

    Would be read as:

        foo
          bar
            qux corge
              yes maybe

    Reasonably simple to explain, yet very flexible, capable of expressing a wide variety of different language constructs (like `$let`, `$and`, and `$assign`, just to name a few). Unfortunately, this approach had two major downsides: firstly, it was very difficult to actually implement with my *very* simple parsing approach.

    Secondly, it relied *entirely* upon indentation, so there was no satisfactory way I could use `:` or `;` inside `()` and `[]`. This actually isn't that big of a deal, because `()` and `[]` occur fairly rarely in Nulan code. This is the approach that Nulan would have ended up using, but...

 6. I decided that if I was going to have `:` and `;` syntax, it had to work exactly the same inside `()` and `[]` as it did with indent. That meant it couldn't rely upon indentation, it had to operate entirely on the list structure. But unlike approach #4, I tried something else...

    This whole time, I had been thinking about how `:` and `;` will interact with indentation, but I had a sudden realization: what if I first added in the automatic parens, and *then* processed the `:` and `;` syntaxes? Let's go back to a previous example:

        $lets: foo: bar;
               qux: corge
               nou: maybe: something
          ...

    First, let's automatically add in parens based on indentation:

        ($lets: foo: bar;
                (qux: corge)
                (nou: maybe: something)
          ...)

    Now `:` has a very simple meaning: it wraps everything between `:` and `)` in a list. And `;` also has a very simple meaning: it will terminate *all* the preceding `:` in the list. Following these rules, we get this:

        ($lets (foo (bar))
               (qux (corge))
               (nou (maybe (something)))
          ...)

    As you can see, the double `;`s aren't needed anymore, because the automatic paren insertion already added in the proper ending `)`! Perfect! But does it work in other common situations? Let's find out!

        $assign foo: $vau Env [X Y]
          $let bar: uniq;
            $if: not: yes;
              corge
              maybe

    First, we add in automatic parens...

        ($assign foo: $vau Env [X Y]
          ($let bar: uniq;
            ($if: not: yes;
              corge
              maybe)))

    Now we process `:` and `;` with the above simple rules:

        ($assign foo ($vau Env [X Y]
          ($let bar (uniq)
            ($if (not (yes))
              corge
              maybe))))

    Voila. This is the syntax that Nulan currently uses. It is extremely simple to explain and reasonably simple to implement. It works exactly the same with `()`, `[]`, and indent. It can be applied to a wide variety of common constructs. Overall, it's very awesome.

    Another very nifty feature is that because it's not based on indentation at all, it can be applied to existing Lisps, like Arc. That is, you could create a Lisp that uses Nulan's `:` and `;` syntax but doesn't have automatic parens, or you could have automatic parens but without Nulan's syntax, or you could have both!

    This means that, the `:` and `;` syntax, and automatic paren insertion, are two separate and orthogonal features which can be mixed-and-matched.
