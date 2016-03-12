import { async_killable, make_thread_pool, run_in_thread_pool,
         kill_thread_pool } from "./task";
import { noop } from "./util";


// TODO test this
export const observe = (a, b) =>
  async_killable((success, error) => {
    // TODO what if an error happens ?
    const pool = make_thread_pool(error);

    const kill = a((value) => {
      run_in_thread_pool(pool, b(value));
    });

    return () => {
      kill_thread_pool(pool);
      kill();
    };
  });


// TODO don't push if the value is the same ?
export const transform = (a, b) =>
  (push) =>
    a((value) => {
      push(b(value));
    });


const empty = {};

// TODO test this
// TODO don't push if the values are the same ?
export const transform_pair = (a, b) =>
  (push) => {
    let value1 = empty;
    let value2 = empty;

    const kill1 = a((value) => {
      value1 = value;

      if (value2 !== empty) {
        push({ a: value1, b: value2 });
      }
    });

    const kill2 = b((value) => {
      value2 = value;

      // TODO is this needed ?
      if (value1 !== empty) {
        push({ a: value1, b: value2 });
      }
    });

    return () => {
      kill1();
      kill2();
    };
  };


// TODO test this
// TODO don't push if the values are the same ?
export const transform2 = (a, b, c) =>
  (push) => {
    let value1 = empty;
    let value2 = empty;

    const kill1 = a((value) => {
      value1 = value;

      if (value2 !== empty) {
        push(c(value1, value2));
      }
    });

    const kill2 = b((value) => {
      value2 = value;

      // TODO is this needed ?
      if (value1 !== empty) {
        push(c(value1, value2));
      }
    });

    return () => {
      kill1();
      kill2();
    };
  };


// TODO test this
export const transform_many = (a, b) =>
  (push) => {
    const length = a["length"];

    const stops = new Array(length);
    const values = new Array(length);

    let pending = length - 1;

    for (let i = 0; i < length; ++i) {
      stops[i] = a[i]((value) => {
        values[i] = value;

        // TODO is there a better way to accomplish this ?
        if (pending === 0) {
          // TODO don't push if the values are the same ?
          push(b(values));

        } else {
          --pending;
        }
      });
    }

    return () => {
      for (let i = 0; i < length; ++i) {
        stops[i]();
      }
    };
  };


export const reply = (a) =>
  (push) => {
    push(a);
    return noop;
  };


// TODO some code duplication with event.js
// TODO test this
// TODO use Date.now instead ?
export const throttle = (observer, delay) => {
  if (delay < 1) {
    crash(new Error("delay must be 1 or greater"));
  }

  return (push) => {
    let timer = null;
    let changed = empty;

    const kill = observer((x) => {
      if (timer === null) {
        timer = setTimeout(() => {
          timer = null;

          const x = changed;

          if (x !== empty) {
            changed = empty;
            push(x);
          }
        }, delay);

        push(x);

      } else {
        changed = x;
      }
    });

    return () => {
      if (timer !== null) {
        clearTimeout(timer);
      }
      kill();
    };
  };
};


// TODO code duplication with throttle
// TODO test this
export const throttle_refresh_rate = (observer) =>
  (push) => {
    let timer = null;
    let changed = empty;

    const kill = observer((x) => {
      if (timer === null) {
        timer = requestAnimationFrame(() => {
          timer = null;

          const x = changed;

          if (x !== empty) {
            changed = empty;
            push(x);
          }
        });

        push(x);

      } else {
        changed = x;
      }
    });

    return () => {
      if (timer !== null) {
        cancelAnimationFrame(timer);
      }
      kill();
    };
  };
