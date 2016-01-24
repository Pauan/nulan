The ``MATCHES`` expression allows for evaluating different code depending on
whether a pattern matches or not. It is represented as nested JavaScript
switches:

* Nulan::

    (MATCHES [ 1 2 3 ]
    | [ a ]
        a
    | [ a b ]
        (+ a b)
    | [ 1 0 0 ]
        2
    | [ a b c ]
        (+ a b c)
    | a
        3)

* JavaScript::

    (() => {
      const a = [1, 2, 3];

      switch (a.length) {
      case 1:
        const b = a[0];
        return b;

      case 2:
        const c = a[0];
        const d = a[1];
        return c + d;

      case 3:
        const e = a[0];
        const f = a[1];
        const g = a[2];

        switch (e) {
        case 1:
          switch (f) {
          case 0:
            switch (g) {
            case 0:
              return 2;
            }
          }
        default:
          return e + f + g;
        }

      default:
        return 3;
      }
    })();
