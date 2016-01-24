The ``FUNCTION`` statement creates a new function in the current module.

It is represented with the JavaScript ``const`` statement and a JavaScript
function:

* Nulan::

    (FUNCTION foo :: (-> a a)
    | (foo a)
        a)

* JavaScript::

    const foo = (a) => {
      return a;
    };

Pattern matching is done in the same way as ``MATCHES``::

* Nulan::

    (FUNCTION foo :: (-> Integer Integer)
    | (foo 0)
        0
    | (foo 1)
        1
    | (foo a)
        a)

* JavaScript::

    const foo = (a) => {
      switch (a) {
      case 0:
        return 0;
      case 1:
        return 1;
      default:
        return a;
      }
    };

If the function calls itself, it has the same behavior as ``WITH-LOOP``:

* Nulan::

    (FUNCTION foo :: (-> Integer Integer)
    | (foo 0)
        0
    | (foo 1)
        1
    | (foo a)
        (foo (- a 1)))

* JavaScript::

    const foo = (a) => {
      for (;;) {
        switch (a) {
        case 0:
          return 0;
        case 1:
          return 1;
        default:
          a = a - 1;
        }
      }
    };
