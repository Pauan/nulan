The function body must be transformed into a `WITH-LOOP`.

Before::

  (FUNCTION foo :: (-> Integer Integer)
  | (foo a)
      (MATCHES [a]
      | [0]
          0
      | [1]
          1
      | [a]
          (foo a)))

After::

  (FUNCTION foo :: (-> Integer Integer)
  | (foo a)
      (WITH-LOOP foo | a <= a
        (MATCHES [a]
        | [0]
            0
        | [1]
            1
        | [a]
            (foo a))))

----

As an example, this::

  (MUTUALLY-RECURSIVE
    (FUNCTION even? :: (-> Integer Boolean)
    | (even? 0)
        true
    | (even? a)
        (odd? (- a 1)))

    (FUNCTION odd? :: (-> Integer Boolean)
    | (odd? 0)
        false
    | (odd? a)
        (even? (- a 1))))

Is transformed into this::

  (TYPE State
  | (*even Integer)
  | (*odd Integer))

  (FUNCTION loop :: (-> State Boolean)
  | (loop (*even 0))
      true
  | (loop (*even a))
      (loop (*odd (- a 1)))
  | (loop (*odd 0))
      false
  | (loop (*odd a))
      (loop (*even (- a 1))))

  (FUNCTION even? :: (-> Integer Boolean)
  | (even? a)
      (loop (*even a)))

  (FUNCTION odd? :: (-> Integer Boolean)
  | (odd? a)
      (loop (*odd a)))

Note: the variables `State`, `*even`, `*odd`, and `loop` are unique, so that
they do not conflict with existing variables.

After the transformation is applied, optimizations are applied as normal.

e.g. the `loop` function calls itself tail-recursively, so it should be
optimized as such.

----



const d = (state) => {
  for (;;) {
    if (state.$ === 0) {
      const a = state.a;

      if (a === 0) {
        return true;
      } else {
        state = { $: 1, a: a - 1 };
      }

    } else {
      const a = state.a;

      if (a === 0) {
        return false;
      } else {
        state = { $: 0, a: a - 1 };
      }
    }
  }
};

const even = (a) =>
  d({ $: 0, a });

const odd = (a) =>
  d({ $: 1, a });
