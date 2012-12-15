One of the simplest patterns is a symbol, which simply binds something to a variable::

  var a = 5

The above binds the variable ``a`` to the value ``5``

----

Numbers and strings match themself::

  var "foo" = "foo"

The above doesn't bind any variables, but checks that the right hand side is equal to ``"foo"``

If you had instead said this...

::

  var "foo" = "bar"

...then Nulan would throw an error.

----

Nulan also supports the ``'`` pattern, but only at compile-time::

  var ('+) = ('+)

The above simply checks that the right hand side is equal to the box ``+``

----

Nulan uses the symbol ``_`` for a "wildcard" that matches anything::

  var _ = 5

The above doesn't bind any variables.

----

You can also specify the default for a pattern::

 var (a = 10) = ...

If ``...`` is null, the above will bind ``a`` to ``10``, otherwise it will bind ``a`` to ``...``

It is equivalent to this::

  | var a = ...
  | if: null? a
      a <= 10

----

Nulan also supports pattern matching on lists::

  var {a b c} = {1 2 3}

The above binds ``a`` to ``1``, ``b`` to ``2``, and ``c`` to ``3``

Lists also support the ``@`` pattern, which takes zero or more elements of the list and puts them into a list::

  var {a @b c} = {1 2 3 4 5 6}

The above binds ``a`` to ``1``, ``b`` to ``{2 3 4 5}``, and ``c`` to ``6``

This is very similar to the ``*`` operator in Python and Ruby.

----

Nulan also supports pattern matching on dictionaries::

  var [ foo a bar b ] = [ foo 1 bar 2 ]

The above binds ``a`` to ``1`` and ``b`` to ``2``

Dictionaries don't support the ``@`` pattern.

----

All of the above patterns can be nested as deeply as you like::

  var {a {b _ [ foo (c = 5) ] ('+) 50} @d} = ...

And the patterns can be used in function/macro arguments too::

  -> {a b} _ c @d
    ...
