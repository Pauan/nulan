One of the simplest patterns is a symbol, which simply binds the data to a variable::

  -> a b c
    ...

The above is a function that accepts three arguments, bound to ``a``, ``b``, and ``c``

Functions that accept only a fixed number of arguments are boring, so let's write a function that can accept any number of arguments::

  -> @a
    ...

The above uses a ``splice`` pattern, which simply says "take all of the arguments and bind them to the variable ``a``"

You can, of course, combine these two together::

  -> a b @c d
    ...

The above is a function that accepts 3 or more arguments. The first two arguments are put into ``a`` and ``b``, the last argument is put into ``d``, and everything between the second and last arguments are put into ``c``

----

Nulan also supports pattern matching on lists::

  -> {a b c}
    ...

  -> {a @b c}
    ...

The first function accepts a *single argument*, which is expected to be a list of three elements. It binds the elements in order to ``a``, ``b``, and ``c``

The second function demonstrates that ``splice`` works on lists as well.

----

Nulan also supports pattern matching on dictionaries::

  -> [ foo a bar b ]
    ...

The above is a function that accepts a *single argument*, which is expected to be a dictionary. It then binds the ``foo`` key to ``a`` and the ``bar`` key to ``b``

----

There is also a special ``_`` pattern that matches anything::

  -> _ a b _
    ...

The above is a function that accepts 4 arguments, and it doesn't care what the first and last arguments are.

----

Literals match themself::

  -> 1 "2" 3
    ...

The above is a function that accepts 3 arguments, and the first argument **must** be ``1``, the second argument **must** be ``"2"``, and the third argument **must** be ``3``

----

Nulan also supports setting the default of a pattern::

  -> a = 5
    ...

The above is similar to "optional arguments" in other languages and is equivalent to this::

  -> a
    | if a = ()
        a <= 5
    | ...

----

And of course all of these can be combined as much as you like::

  -> {[ foo {a "20" c = 5} ] @d} _ @{e f g}
    ...

The above is a function that accepts 5 arguments. The first argument is a list that has 1 or more elements, with the first element being a dictionary that has a ``foo`` key that is bound to a list of 2 or 3 elements, where the second element is ``"20"`` and the third element defaults to ``5`` if it doesn't exist.
