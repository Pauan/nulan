import { async_killable, make_thread_pool, run_in_thread_pool,
         kill_thread_pool } from "./task";
import { crash, error_from } from "./crash";
import { noop } from "./util";


// TODO test this
export const observe = (a, b) =>
  async_killable((success, error) => {
    const pool = make_thread_pool();

    // TODO what if an error happens ?
    const on_error = (e) => {
      kill_thread_pool(pool);
      return error(e);
    };

    const kill = a((value) =>
      run_in_thread_pool(pool, on_error, b(value)));

    return () => {
      kill_thread_pool(pool);
      return kill();
    };
  });


// TODO don't push if the value is the same ?
export const transform = (a, b) =>
  (push) =>
    a((value) =>
      push(b(value)));


const empty = {};

// TODO test this
// TODO don't push if the values are the same ?
export const transform2 = (a, b, c) =>
  (push) => {
    let value1 = empty;
    let value2 = empty;

    const kill1 = a((value) => {
      value1 = value;

      if (value2 !== empty) {
        return push(c(value1, value2));
      }
    });

    const kill2 = b((value) => {
      value2 = value;

      // TODO is this needed ?
      if (value1 !== empty) {
        return push(c(value1, value2));
      }
    });

    return () => {
      kill1();
      return kill2();
    };
  };


export const reply = (a) =>
  (push) => {
    push(a);
    return noop;
  };


export const flatten = (a) =>
  (push) => {
    let old_kill = noop;

    const kill = a((value) => {
      old_kill();
      old_kill = value(push);
    });

    return () => {
      kill();
      return old_kill();
    };
  };


// TODO some code duplication with event.js
// TODO test this
// TODO use Date.now instead ?
export const throttle = (observer, delay) => {
  if (delay < 1) {
    return crash(error_from("delay must be 1 or greater"));
  }

  return (push) => {
    let timer = null;
    let changed = empty;

    const on_change = (x) => {
      if (timer === null) {
        timer = setTimeout(() => {
          timer = null;

          const x = changed;

          if (x !== empty) {
            changed = empty;
            return on_change(x);
          }
        }, delay);

        return push(x);

      } else {
        changed = x;
      }
    };

    const kill = observer(on_change);

    return () => {
      if (timer !== null) {
        clearTimeout(timer);
        timer = null;
      }

      return kill();
    };
  };
};


// TODO code duplication with throttle
// TODO test this
export const throttle_refresh_rate = (observer) =>
  (push) => {
    let timer = null;
    let changed = empty;

    const on_change = (x) => {
      if (timer === null) {
        timer = requestAnimationFrame(() => {
          timer = null;

          const x = changed;

          if (x !== empty) {
            changed = empty;
            return on_change(x);
          }
        });

        return push(x);

      } else {
        changed = x;
      }
    };

    const kill = observer(on_change);

    return () => {
      if (timer !== null) {
        cancelAnimationFrame(timer);
        timer = null;
      }

      return kill();
    };
  };
