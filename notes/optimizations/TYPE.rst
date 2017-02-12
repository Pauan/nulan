Data unwrapping
===============

When does the optimization apply?
---------------------------------

1) The type contains a single data function, and the data function contains a
   single parameter::

     (TYPE (Foo a)
     | (*foo a))


What is optimized?
------------------

When the data function is called, it does not return a new data wrapper.

Instead, the parameter of the data function is returned unmodified.

In addition, when pattern matching on the data function, it does not do any
run-time checks.

Note: this does not affect the type or the behavior. It only affects the
way that the value is represented at run-time.


What are the benefits?
----------------------

* Faster.

* Makes using the FFI easier: it is possible to treat native types as if they
  were a Nulan type, while still using the native type at run-time::

    (TYPE Foo
    | (*foo Integer))

    (UNSAFE-FFI-LOAD { target <= javascript
                     | file <= "foo.js" }
    | foo :: (-> Foo Foo))

  The run-time type is ``Integer``, but Nulan treats it as if its type is
  ``Foo``
