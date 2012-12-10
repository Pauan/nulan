The syntax system in Nulan is (as far as I know) unique. Just like macros make it easy to change the compiler, Nulan makes it easy to change the syntax.

To demonstrate how the system works, let's parse this Nulan program::

  # Needed because I wasn't able to get element.scroll-into-wiew-if-needed working
  def scroll-into-view -> n p
    let n = n.get-bounding-client-rect;
        r = p.get-bounding-client-rect;
      | if (n.top < r.top || n.bottom > r.bottom)
          p.scroll-top <= n.top - r.height / 2
      | if (n.left < r.left || n.right > r.right)
          p.scroll-left <= n.left - r.width / 2

There are five phases to Nulan's syntax parsing:

1) Tokenization. This phase splits a string into tokens. The end result is a flat 1-dimensional list of numbers, symbols, and strings::

     def scroll-into-view -> n p let n = n . get-bounding-client-rect ; r = p . get-bounding-client-rect ; | if ( n . top < r . top || n . bottom > r . bottom ) p . scroll-top <= n . top - r . height / 2 | if ( n . left < r . left || n . right > r . right ) p . scroll-left <= n . left - r . width / 2

   The list has no structure to it, but Nulan keeps track of the line and column where each token was found. This will be very important later on.

2) Because braces ignore indentation, they need to be handled specially. So when Nulan encounters a ``(``, ``{``, or ``[`` token, it will parse until it finds an ending ``)``, ``}``, or ``]`` token::

     def scroll-into-view -> n p let n = n . get-bounding-client-rect ; r = p . get-bounding-client-rect ; | if
       {n . top < r . top || n . bottom > r . bottom}
       p . scroll-top <= n . top - r . height / 2 | if
       {n . left < r . left || n . right > r . right}
       p . scroll-left <= n . left - r . width / 2

3) The ``|`` token is also handled specially. If it occurs at the start of the line, it will take all the lines that start with ``|`` at the same indentation and put them into a list::

     def scroll-into-view -> n p let n = n . get-bounding-client-rect ; r = p . get-bounding-client-rect ;
       {|
         {if
           {n . top < r . top || n . bottom > r . bottom}
           p . scroll-top <= n . top - r . height / 2}
         {if
           {n . left < r . left || n . right > r . right}
           p . scroll-left <= n . left - r . width / 2}}

4) Nulan uses significant whitespace, and has very simple rules for how to handle it:

     1) Everything on the same line is put into a list::

          {def scroll-into-view -> n p}

     2) For all the lines that have greater indentation than the current line, put them into the list too::

          {def scroll-into-view -> n p
            {let n = n . get-bounding-client-rect ;}}

     3) Repeat the above two processes recursively::

          {def scroll-into-view -> n p
            {let n = n . get-bounding-client-rect ;
              {r = p . get-bounding-client-rect ;}
              {| {if {n . top < r . top || n . bottom > r . bottom}
                   {p . scroll-top <= n . top - r . height / 2}}
                 {if {n . left < r . left || n . right > r . right}
                   {p . scroll-left <= n . left - r . width / 2}}}}}

     4) Certain syntax like ``=`` and ``'`` will take everything that's indented further than the token and will put it into a list::

          {def scroll-into-view -> n p
            {let n = {n . get-bounding-client-rect ;}
              {r = {p . get-bounding-client-rect ;}}
              {| {if {n . top < r . top || n . bottom > r . bottom}
                   {p . scroll-top <= n . top - r . height / 2}}
                 {if {n . left < r . left || n . right > r . right}
                   {p . scroll-left <= n . left - r . width / 2}}}}}

5) Now we have a structured program, with lists nested within lists. But we're not done yet. There's a bunch of symbols like ``=``, ``.``, and ``<`` that have special meaning, but they haven't been parsed yet.

   Nulan has a macro called ``syntax-rule`` that lets you add new syntax. Here are some examples::

     syntax-rule <>
       priority 70
       action -> {@l x} s {y @r}
         `,@l (s x y) ,@r

     syntax-rule \
       priority  10
       delimiter %t
       separator %t
       action -> l s {x @r}
         `,@l (s x) ,@r

     syntax-rule ^
       delimiter %t
       priority 10
       order "right"
       action -> l s {@args body}
         ',@l (s args body)

   How this works is, each rule can have the following properties:

     * If ``separator`` is true, the parser will take everything that's indented further than the symbol and will put it into a list
     * If ``delimiter`` is true, the parser will treat the syntax as not being a part of symbols
     * The rules with higher ``priority`` are run first. The default is ``0`` priority
     * If a rule has ``order`` set to ``"right"`` then it will be right-associative, otherwise it's left-associative
     * The ``action`` function receives three arguments: a list of everything to the left of the symbol, the symbol, and a list of everything to the right of the symbol.

   So, looking at the above, the rule for ``<>`` is pretty simple: take the last argument of the left list and the first argument of the right list and mush them together. As an example, this::

     {1 2 3 <> 4 5 6}

   Will pass the arguments ``{1 2 3}``, ``<>``, and ``{4 5 6}`` to the action function. The action function then returns this::

     {1 2 {<> 3 4} 5 6}

   Most infix operators work this way, and this is so common that there's a macro called ``syntax-infix`` which does this for you, which means that the ``<>`` syntax could be written like this instead::

     syntax-infix <> 70

   The ``\`` syntax is a bit trickier. It specifies that it's a delimiter, which means that it'll never be processed as part of a symbol. That means that ``foo\bar`` will be parsed as the three symbols ``foo`,  ``\``, and ``bar`` rather than the single symbol ``foo\bar``

   It also says that it's a separator. What this means is that, in the following Nulan program::

     foo bar\ corge
                qux
       nou

   It will be parsed like this::

     {foo bar \ {corge qux}
       nou}

   That is, it took everything indented further than ``\`` and put it into a list. The action function then receives the arguments ``{foo bar}``, ``\``, and ``{{corge qux} nou}``. It then returns this::

     {foo bar {\ corge qux} nou}

   Lastly, the ``^`` syntax. With this list::

     {1 2 3 ^ a b c {+ a b c}}

   It will pass the arguments ``{1 2 3}``, ``^``, and ``{a b c {+ a b c}}`` to the action function. It then returns this::

     {1 2 3 {^ {a b c} {+ a b c}}}

   And because it has ``order`` set to ``"right"``, that means that this::

     {^ a ^ b {+ a b}}

   Will parse as this::

     {^ {a} {^ {b} {+ a b}}}

   Rather than this::

     {^ {a {^ {b}}} {+ a b}}

   One last thing. If the parser returns a list that only has a single item, then it unwraps the list, which means that these::

     1 + 2

     (1 + 2)

     (((1 + 2)))

     (((((1 + 2)))))

   Are all parsed into this::

     {+ 1 2}

That describes basically the entire parser.
