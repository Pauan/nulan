The Nulan `Number` type is represented as a JavaScript number:

* Nulan::

    5.10

* JavaScript::

    5.10

Pattern matching is done using a JavaScript switch:

* Nulan::

    (FUNCTION foo :: (-> Number Number)
    | (foo 0.5)
        0.5
    | (foo 1.5)
        1.5
    | (foo a)
        a)

* JavaScript::

    const foo = (a) => {
      switch (a) {
      case 0.5:
        return 0.5;
      case 1.5:
        return 1.5;
      default:
        return a;
      }
    };
