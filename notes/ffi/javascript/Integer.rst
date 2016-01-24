The Nulan `Integer` type is represented as a rounded JavaScript number:

* Nulan::

    5

* JavaScript::

    5

Pattern matching is done using a JavaScript switch:

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
