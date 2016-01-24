The Nulan `Text` type is represented as a JavaScript string:

* Nulan::

    "I am a string!"

* JavaScript::

    "I am a string!"

Pattern matching is done using a JavaScript switch:

* Nulan::

    (FUNCTION foo :: (-> Text Text)
    | (foo "0")
        "0"
    | (foo "1")
        "1"
    | (foo a)
        a)

* JavaScript::

    const foo = (a) => {
      switch (a) {
      case "0":
        return "0";
      case "1":
        return "1";
      default:
        return a;
      }
    };
