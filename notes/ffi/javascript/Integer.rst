The Nulan ``Integer`` type is represented as a `signed 32-bit binary integer <https://en.wikipedia.org/wiki/Integer_%28computer_science%29#Common_integral_data_types>`_.

The maximum value is ``2147483647`` and the minimum value is ``-2147483648``

If you add ``1`` to the maximum value, it will return the minimum value.

If you subtract ``1`` from the minimum value, it will return the maximum value.

You should not write code which relies upon the size or wrapping behavior of ``Integer``

The size or wrapping behavior of Nulan's ``Integer`` type could change at any time. If the behavior changes, it will cause a minor bump to Nulan's version.

----

Pattern matching is done using a JavaScript ``switch``:

* Nulan::

    (FUNCTION foo :: (-> Integer Integer)
      (foo 0)
      = 0

      (foo 1)
      = 1

      (foo a)
      = a)

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
