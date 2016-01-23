Self tail call optimization
===========================

When does the optimization apply?
---------------------------------

1) Calls itself within the body::

     (WITH-LOOP loop
     | a <= 1
       (loop a))

2) Calls itself within the body of a `MATCHES`::

     (WITH-LOOP loop
     | a <= 1
       (MATCHES [a]
       | [a]
           (loop a)))

   Note: if there are multiple branches, the optimization applies if it
   calls itself in **any** of the branches::

     (WITH-LOOP loop
     | a <= 1
       (MATCHES [a]
       | [0]
           0
       | [1]
           1
       | [a]
           (loop a)))

3) Calls itself within the body of a nested `WITH-LOOP`::

    (WITH-LOOP loop
    | a <= 1
      (WITH-LOOP loop2
      | b <= 2
        (loop a)))

4) Any of the above situations, recursively::

    (WITH-LOOP loop
    | a <= 1
      (MATCHES [a]
      | [0]
          0
      | [1]
          1
      | [a]
          (WITH-LOOP loop2
          | b <= 2
            (MATCHES [b]
            | [c]
               (loop a)))))

Note: because of lambda inlining, this optimization also applies when it
calls itself within the body of a `->`, and the `->` is immediately called::

   (WITH-LOOP loop
   | a <= 1
     ((-> b
        (loop a))
      2))


What is optimized?
------------------

When calling itself repeatedly, it must use a constant amount of memory for
all of the function calls which call itself.


What are the benefits?
----------------------

* Can loop forever without running out of memory.

* Some implementations can completely avoid the cost of the function call.


Implementation details
----------------------

If the implementation is an interpreter or virtual machine, this optimization
is normally implemented by reusing the existing stack frame, rather than
creating a new stack frame.

If the implementation is a compiler, this optimization is normally implemented
by using gotos, or a `for` / `while` loop and labels.

Here is an example of the output of the Nulan -> JavaScript compiler:

* Before::

    (WITH-LOOP loop
    | a <= 0
      (MATCH a
      | 10
          a
      | a
          (WITH-LOOP loop2
          | a <= a
            (MATCH a
            | 5
                ((-> b
                   (loop2 b))
                 (+ a 1))
            | 20
                a
            | a
                (loop (+ a 1))))))

* After::

    const loop = (a) => {
      loop: for (;;) {
        if (a === 10) {
          return a;

        } else {
          let a2 = a;

          loop2: for (;;) {
            if (a2 === 5) {
              const b = a2 + 1;
              a2 = b;
              continue loop2;

            } else if (a2 === 20) {
              return a2;

            } else {
              a = a2 + 1;
              continue loop;
            }
          }
        }
      }
    };

    loop(0);

Because it uses a `for` loop and labeled statements, it is evaluated with
constant memory usage.
