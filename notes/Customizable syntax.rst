The syntax system in Nulan is (as far as I know) unique. Just like macros make it easy to change the compiler, Nulan makes it easy to change the syntax.

To demonstrate how the system works, let's parse this Nulan program::

  # Needed because I wasn't able to get element.scroll-into-wiew-if-needed working
  def scroll-into-view -> n p
    w/var n = n.get-bounding-client-rect;
          r = p.get-bounding-client-rect;
      | if (n.top < r.top || n.bottom > r.bottom)
          p.scroll-top <= n.top - r.height / 2
      | if (n.left < r.left || n.right > r.right)
          p.scroll-left <= n.left - r.width / 2

There are three phases to Nulan's syntax parsing:

1) Tokenization. This phase splits a string into tokens. The end result is a flat 1-dimensional list of numbers, symbols, and strings::

     def scroll-into-view -> n p w/var n = n . get-bounding-client-rect ; r = p . get-bounding-client-rect ; | if ( n . top < r . top || n . bottom > r . bottom ) p . scroll-top <= n . top - r . height / 2 | if ( n . left < r . left || n . right > r . right ) p . scroll-left <= n . left - r . width / 2

   The list has no structure to it, but Nulan keeps track of the line and column where each token was found. This will be very important later on.

2) Nulan uses significant whitespace, and has very simple rules for how to handle it:

   1) Everything on the same line is put into a list::

        {def scroll-into-view -> n p}

   2) For all the lines that have greater indentation than the current line, put them into the list too::

        {def scroll-into-view -> n p
          {w/var n = n . get-bounding-client-rect ;}}

   3) If the list only contains a single item, unwrap it::

        # This...
        foo bar
          qux
          corge

        # ...is parsed into this
        {foo bar
          {qux}
          {corge}}

        # ...which is then unwrapped to this
        {foo bar
          qux
          corge}

   4) Repeat the above three processes recursively::

        {def scroll-into-view -> n p
          {w/var n = n . get-bounding-client-rect ;
            {r = p . get-bounding-client-rect ;}
            {| if ( n . top < r . top || n . bottom > r . bottom )
              {p . scroll-top <= n . top - r . height / 2}}
            {| if ( n . left < r . left || n . right > r . right )
              {p . scroll-left <= n . left - r . width / 2}}}}

3) Now we have a structured program, with lists nested within lists. But we're not done yet. There's a bunch of symbols like ``=``, ``.``, and ``<`` that have special meaning, but they haven't been parsed yet.

   Nulan has an object called ``syntax-rules`` which contains information on how to parse the remaining syntax. To create new syntax, you can use the ``$syntax-rule`` macro::

     $syntax-rule "^" [
       ...
     ]

   The above creates a new rule for the ``^`` symbol. What can this rule do?

   * Syntax rules with higher ``priority`` are run first (the default is ``0``)::

       $syntax-rule "^" [
         priority 50
       ]

   * If ``whitespace`` is true, then the symbol will be treated as whitespace::

       $syntax-rule "^" [
         whitespace %t
       ]

     Currently, the only thing this changes is whether ``^[foo]`` will parse as ``{. ^ foo}`` or ``{^ {dict foo}}``

   * If ``delimiter`` is true, the parser will not treat the syntax as being a part of symbols::

       $syntax-rule "^" [
         delimiter %t
       ]

     This means that ``foo^bar`` will be parsed as ``{foo ^ bar}`` rather than the single symbol ``foo^bar``

   * If ``separator`` is true, the parser will take everything that's indented to the right of the the symbol and will put it into a list::

       $syntax-rule "^" [
         separator %t
       ]

     What this means is that this...

     ::

       foo ^ bar qux
               corge
         yes

     ...will be parsed as this::

       {foo ^ {bar qux
                corge}
         yes}

   * If ``vertical`` is true, the parser will scan vertically for the same symbol and will mush it into a single list::

       $syntax-rule "^" [
         vertical %t
       ]

     What this means is that this...

     ::

       foo ^ 1 2 3
           ^ 4 5 6
           ^ 7 8 9
         bar qux

     ...will be parsed into this::

       {foo ^ {1 2 3
               4 5 6
               7 8 9}
         {bar qux}}

     You will usually want to use ``separator`` at the same time, in which case it would be parsed like this::

       {foo ^ {{1 2 3}
               {4 5 6}
               {7 8 9}}
         {bar qux}}

     This is used for the ``|`` syntax.

   * If ``endAt`` exists, it should be a string. The parser will search for a symbol that matches the string and will put everything between it and the original symbol into a list::

       $syntax-rule "^" [
         endAt "/"
       ]

     What the above means is that the following program...

     ::

       foo bar ^ qux corge / nou yes

     ...will be parsed as this::

       {foo bar ^ {qux corge} nou yes}

     In other words, it took everything between ``^`` and ``/`` and put it into a list. This is used for the ``()``, ``{}``, and ``[]`` braces.

   * If ``order`` is ``"right"``, the syntax will be right-associative, otherwise it's left-associative::

       $syntax-rule "^" [
         order "right"
       ]

     Left-associative (the default) means that ``foo ^ bar ^ qux`` is parsed as ``{{foo ^ bar} ^ qux}`` and right-associative means that it's parsed as ``{foo ^ {bar ^ qux}}``

   * The ``tokenize`` property is a function::

       $syntax-rule "^" [
         tokenize -> s o push
           ...
       ]

     When the tokenizer encounters "^" it will call the ``tokenize`` function with three arguments:

     1) The first argument is the string "^"

     2) The second argument is an iterator that contains all the characters remaining after the "^" character. Because this is handled by the tokenizer, it's just raw characters, there's no structure yet. It has the following methods:

        * ``has`` returns true if the iterator has any items remaining, otherwise false
        * ``peek`` returns the next character in the iterator, but doesn't consume anything
        * ``read`` returns the next character in the iterator, and consumes it

     3) The third argument is a function that you can call to return a result. Here's an example of a rule that when given the string "^foo" will return "bar"::

          $syntax-rule "^" [
            tokenize -> s o push
              if o.peek; == "f"
                | o.read;
                | if o.peek; == "o"
                    | o.read;
                    | if o.peek; == "o"
                        | o.read;
                        | push "bar"
          ]

        The tokenize function is used for parsing whitespace, comments, and strings.

   * The ``parse`` property is a function that accepts three arguments: a list of everything to the left of the symbol, the symbol, and a list of everything to the right of the symbol::

       $syntax-rule "^" [
         parse -> l s r
           ...
       ]

     This is the unique part of Nulan's parser. It's what makes it so easy to define new syntax, while still being very powerful. Consider this program::

       foo bar ^ qux corge

     When Nulan encounters ``^``, it will pass the arguments ``{foo bar}``, ``^``, and ``{qux corge}`` to the ``parse`` function. Whatever the function returns is used as the final result.

     A typical infix operator is easy to define, it simply takes the last element of the left list and the first element of the right list and mushes them together::

       $syntax-rule "^" [
         parse -> {@l x} s {y @r}
           ',@l (s x y) ,@r
       ]

     And now the above program will be parsed as ``{foo {^ bar qux} corge}``. This is common enough that Nulan provides a macro called ``$syntax-infix``::

       $syntax-infix "^"

     Using the same system, unary is also easy::

       $syntax-rule "^" [
         parse -> l s {y @r}
           ',@l (s y) ,@r
       ]

     And now the program is parsed as ``{foo bar {^ qux} corge}``. Just like with infix, you can use ``$syntax-unary`` to do the same thing::

       $syntax-unary "^"

     But you aren't limited to using only a single symbol. For instance, consider the ``->`` syntax::

       foo bar -> a b c
         qux corge

     Here's how you would write a rule for ``->``::

       $syntax-rule "->" [
         order "right"
         parse -> l s {@args body}
           ',@l (s args body)
       ]

     And now the program will parse as ``{foo bar {-> {a b c} {qux corge}}}``

     Or consider the ``<=`` syntax::

       foo bar <= qux corge

     You can write a rule for it like this::

       $syntax-rule "<=" [
         order "right"
         parse -> l s r
           's ,(unwrap l) ,(unwrap r)
       ]

     And now it will be parsed as ``{<= {foo bar} {qux corge}}``

     The reason for ``unwrap`` is so that ``foo <= bar`` is parsed as ``{<= foo bar}`` rather than ``{<= {foo} {bar}}``

   Here is a list of all the built-in syntax::

     $syntax-rule "(" [
       priority 110
       delimiter %t
       endAt ")"
       parse -> l s {x @r}
         ',@l ,(unwrap x) ,@r
     ]

     $syntax-rule "{" [
       priority 110
       delimiter %t
       endAt "}"
       parse -> l s {x @r}
         ',@l (list ,@x) ,@r
     ]

     $syntax-rule "[" [
       priority 110
       delimiter %t
       endAt "]"
       parse -> {@l x} s {y @r}
         if s.whitespace
           ',@l x (dict ,@y) ,@r
           ',@l (. x ,(unwrap y)) ,@r
     ]

     $syntax-rule ";" [
       priority 100
       delimiter %t
       parse -> {@l x} s r
         ',@l (x) ,@r
     ]

     $syntax-rule ":" [
       priority 100
       delimiter %t
       separator %t
       parse -> l s {x @r}
         ',@l x ,@r
     ]

     $syntax-rule "." [
       priority 100
       delimiter %t
       parse -> {@l x} s {y @r}
         if (num? x) && (num? y)
           ',@l ,(num: x + "." + y) ,@r
           if (sym? y)
             ',@l (s x y.value) ,@r
             ',@l (s x y) ,@r
     ]

     $syntax-unary "," 90 [ delimiter %t ]
     $syntax-unary "@" 90 [ delimiter %t ]
     $syntax-unary "~" 90

     $syntax-infix "*" 80
     $syntax-infix "/" 80

     $syntax-infix "+" 70
     $syntax-infix "-" 70

     $syntax-infix "<"  60
     $syntax-infix ">"  60
     $syntax-infix "=<" 60
     $syntax-infix ">=" 60

     $syntax-infix "==" 50
     $syntax-infix "~=" 50
     $syntax-infix "|=" 50

     $syntax-infix "&&" 40

     $syntax-infix "||" 30

     $syntax-rule "'" [
       priority 10
       whitespace %t
       delimiter %t
       separator %t
       parse -> l s {x @r}
         ',@l (s ,(unwrap x)) ,@r
     ]

     $syntax-rule "->" [
       priority 10
       order "right"
       parse -> l s {@args body}
         ',@l (s args body)
     ]

     $syntax-rule "=" [
       priority 10
       separator %t
       parse -> {@l x} s {y @r}
         ',@l (s x ,(unwrap y)) ,@r
     ]

     $syntax-rule "<=" [
       priority 0
       order "right"
       parse -> l s r
         's ,(unwrap l) ,(unwrap r)
     ]

   Okay! Going back to our program from before::

     {def scroll-into-view -> n p
       {w/var n = n . get-bounding-client-rect ;
         {r = p . get-bounding-client-rect ;}
         {| if ( n . top < r . top || n . bottom > r . bottom )
           {p . scroll-top <= n . top - r . height / 2}}
         {| if ( n . left < r . left || n . right > r . right )
           {p . scroll-left <= n . left - r . width / 2}}}}

   Let's use the built-in syntax to parse this. Firstly, let's run the ``parse`` function for ``->``::

     {def scroll-into-view
       {-> {n p}
         {w/var n = n . get-bounding-client-rect ;
           {r = p . get-bounding-client-rect ;}
           {| if ( n . top < r . top || n . bottom > r . bottom )
             {p . scroll-top <= n . top - r . height / 2}}
           {| if ( n . left < r . left || n . right > r . right )
             {p . scroll-left <= n . left - r . width / 2}}}}}

   Now the function for ``=``...

   ::

     {def scroll-into-view
       {-> {n p}
         {w/var {= n {n . get-bounding-client-rect ;}}
           {r = p . get-bounding-client-rect ;}
           {| if ( n . top < r . top || n . bottom > r . bottom )
             {p . scroll-top <= n . top - r . height / 2}}
           {| if ( n . left < r . left || n . right > r . right )
             {p . scroll-left <= n . left - r . width / 2}}}}}

   Now the function for ``.``...

   ::

     {def scroll-into-view
       {-> {n p}
         {w/var {= n {{. n get-bounding-client-rect} ;}}
           {r = p . get-bounding-client-rect ;}
           {| if ( n . top < r . top || n . bottom > r . bottom )
             {p . scroll-top <= n . top - r . height / 2}}
           {| if ( n . left < r . left || n . right > r . right )
             {p . scroll-left <= n . left - r . width / 2}}}}}

   Now the function for ``;``...

   ::

     {def scroll-into-view
       {-> {n p}
         {w/var {= n {{. n get-bounding-client-rect}}}
           {r = p . get-bounding-client-rect ;}
           {| if ( n . top < r . top || n . bottom > r . bottom )
             {p . scroll-top <= n . top - r . height / 2}}
           {| if ( n . left < r . left || n . right > r . right )
             {p . scroll-left <= n . left - r . width / 2}}}}}

   And such forth and so on. After all the syntax rule functions have been run, the end result is this::

     {def scroll-into-view
       {-> {n p}
         {w/var {= n {{. n get-bounding-client-rect}}}
                {= r {{. p get-bounding-client-rect}}}
           {| {if {|| {< {. n top} {. r top}} {> {. n bottom} {. r bottom}}}
                {<= {. p scroll-top} {- {. n top} {/ {. r height} 2}}}}
              {if {|| {< {. n left} {. r left}} {> {. n right} {. r right}}}
                {<= {. p scroll-left} {- {. n left} {/ {. r width} 2}}}}}}}}

   And now the program is fully parsed and ready to be compiled and executed.
