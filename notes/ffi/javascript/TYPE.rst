The ``TYPE`` statement creates new Nulan types. These types only exist at
compile-time, and do not affect the FFI in any way.

In addition, the ``TYPE`` statement can create zero or more data wrappers.
These data wrappers exist at run-time, and are affected by the FFI.

----

Data wrappers are represented at run-time as JavaScript objects::

* Nulan::

    (TYPE Foo
    | (*foo Integer Integer))

    (*foo 1 2)

* JavaScript::

    { a: 1, b: 2 }

The wrapper contains properties in alphabetical order, corresponding to each
parameter of the wrapper: the property ``a`` is the first parameter, the
property ``b`` is the second parameter, the property ``c`` is the third
parameter, etc.

After the property ``z``, it uses the property ``aa``, then ``ab``, then
``ac``, etc.

After the property ``zz`` it uses the property ``aaa``, then ``aab``, then
``aac``, etc.

After ``zzz`` it continues in a similar pattern to infinity.

----

When there are multiple data wrappers for a type, it uses the ``$`` property
to distinguish between them:

* Nulan::

    (TYPE Foo
    | (*foo Integer Integer)
    | (*bar Integer Integer Integer))

    (*foo 1 2)
    (*bar 3 4 5)

* JavaScript::

    { $: 0, a: 1, b: 2 }
    { $: 1, a: 3, b: 4, c: 5 }

The ``$`` property only exists when there are two or more data wrappers.

If the ``$`` property exists, it is always an integer starting from ``0``
(which represents the first data wrapper), then ``1`` (which represents the
second data wrapper), then ``2`` (which represents the third data wrapper),
etc.

----

If all of the data wrappers contain zero parameters, then the data wrappers
are represented as integers starting from ``0``:

* Nulan::

    (TYPE Foo
    | *foo
    | *bar
    | *qux)

    *foo
    *bar
    *qux

* JavaScript::

    0
    1
    2

----

If a type contains a single data wrapper, and the data wrapper contains a
single parameter, then it does not create a wrapper at all. Instead, the
parameter is returned unmodified:

* Nulan::

    (TYPE Foo
    | (*foo Integer))

    (*foo 5)

* JavaScript::

    5

This is very convenient for creating Nulan wrappers for JavaScript types,
while still using JavaScript types at run-time::

  (TYPE URL
  | (*url String))

  (UNSAFE-FFI-LOAD { target <= javascript
                   | file <= "url.js" }
  | concat :: (-> URL URL URL))

The ``concat`` function only works with the type ``URL``, and the static type
system guarantees that it will be used correctly: it is not possible to call
``concat`` with a ``String``

But at run-time, it is actually using JavaScript strings, which gives good
performance.

This is especially useful for importing existing JavaScript libraries into
Nulan.

----

When pattern matching on data wrappers, it uses JavaScript's ``switch`` to
check the ``$`` property, and then unwraps the alphabetical properties of
the data wrapper:

* Nulan::

    (TYPE Foo
    | (*foo Integer)
    | (*bar Integer Integer))

    (MATCH (*foo 5)
    | (*foo a)
        a
    | (*bar a b)
        (+ a b))

* JavaScript::

    (() => {
      const a = { $: 0, a: 5 };

      switch (a.$) {
      case 0:
        const b = a.a;
        return b;
      case 1:
        const c = a.a;
        const d = a.b;
        return c + d;
      }
    })();

----

If there is only one data wrapper, then it does not check the ``$`` property:

* Nulan::

    (TYPE Foo
    | (*foo Integer Integer))

    (MATCH (*foo 5 10)
    | (*foo a b)
        (+ a b))

* JavaScript::

    (() => {
      const a = { a: 5, b: 10 };

      const b = a.a;
      const c = a.b;
      return b + c;
    })();

----

If there is only one data wrapper, and the data wrapper has only one
parameter, then it does not unwrap the data wrapper:

* Nulan::

    (TYPE Foo
    | (*foo Integer))

    (MATCH (*foo 5)
    | (*foo a)
        a)

* JavaScript::

    (() => {
      const a = 5;

      const b = a;
      return b;
    })();

----

If all of the data wrappers have zero parameters, then it uses a ``switch``
to check the data wrappers:

* Nulan::

    (TYPE Foo
    | *foo
    | *bar
    | *qux)

    (MATCH *foo
    | *foo
        5
    | *bar
        10
    | *qux
        15)

* JavaScript::

    (() => {
      const a = 0;

      switch (a) {
      case 0:
        return 5;
      case 1:
        return 10;
      case 2:
        return 15;
      }
    })();
