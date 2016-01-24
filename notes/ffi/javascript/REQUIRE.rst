The ``REQUIRE`` type specifies that a function accepts any type, as
long as that type uses ``PROVIDE`` to provide the appropriate constants.

Inside of the function's body, you can use the protocol's constants. The
protocol constants vary in behavior based upon the type of the parameters
which are passed to the function.

If the protocol has zero constants, then the Nulan function is represented as
a JavaScript function:

* Nulan::

    (PROTOCOL ($bar a))

    (PROVIDE ($bar Integer))

    (FUNCTION foo :: (REQUIRE ($bar a)
                       (-> a a))
    | (foo a)
        a)

    (foo 5)

* JavaScript::

    const foo = (a) => {
      return a;
    };

    foo(5);

If the protocol has exactly one constant, then the Nulan function is
represented as a JavaScript function, except it has an extra parameter at the
beginning, which is the value of the protocol's constant:

* Nulan::

    (PROTOCOL ($bar a)
    | bar :: a)

    (PROVIDE ($bar Integer)
    | bar <= 10)

    (FUNCTION foo :: (REQUIRE ($bar a)
                       (-> a a))
    | (foo a)
        a)

    (foo 5)

* JavaScript::

    const _bar = 10;

    const foo = (bar, a) => {
      return a;
    };

    foo(_bar, 5);

If the protocol has multiple constants, then the extra parameter is a
JavaScript object, which has alphabetical properties corresponding to each
constant in the protocol, starting with the properties ``a``, ``b``, ``c``,
etc.

* Nulan::

    (PROTOCOL ($bar a)
    | bar :: a
    | qux :: a)

    (PROVIDE ($bar Integer)
    | bar <= 10
    | qux <= 15)

    (FUNCTION foo :: (REQUIRE ($bar a)
                       (-> a a))
    | (foo a)
        a)

    (foo 5)

* JavaScript::

    const _bar = {
      a: 10,
      b: 15
    };

    const foo = (bar, a) => {
      return a;
    };

    foo(_bar, 5);

If there are multiple required protocols, then each protocol is added as an
extra parameter, in the same order that they appear in ``REQUIRE``:

* Nulan::

    (PROTOCOL ($bar a)
    | bar :: a)

    (PROTOCOL ($qux a)
    | qux :: a)

    (PROVIDE ($bar Integer)
    | bar <= 10)

    (PROVIDE ($qux Integer)
    | qux <= 15)

    (FUNCTION foo :: (REQUIRE ($bar a) ($qux a)
                       (-> a a))
    | (foo a)
        a)

    (foo 5)

* JavaScript::

    const _bar = 10;
    const _qux = 15;

    const foo = (bar, qux, a) => {
      return a;
    };

    foo(_bar, _qux, 5);
