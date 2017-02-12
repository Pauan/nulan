import { async_killable, make_thread_pool, run_in_thread_pool,
         kill_thread_pool } from "./task";
import { crash, error_from } from "./crash";


// TODO code duplication with changing.js
export const observe = (event, f) =>
  async_killable((success, error) => {
    const pool = make_thread_pool();

    const on_error = (e) => {
      kill_thread_pool(pool);
      return error(e);
    };

    const kill = event((value) =>
      run_in_thread_pool(pool, on_error, f(value)));

    return () => {
      kill();
      return kill_thread_pool(pool);
    };
  });

export const transform_maybe = (event, f) =>
  (push) =>
    event((value) => {
      const x = f(value);

      // *some
      if (x.$ === 1) {
        return push(x.a);
      }
    });

// TODO test this
export const throttle = (event, delay) => {
  if (delay < 1) {
    return crash(error_from("delay must be 1 or greater"));
  }

  return (push) => {
    let timer = null;

    return event((value) => {
      const now = Date["now"]();

      if (timer === null || now >= timer) {
        timer = now + delay;
        return push(value);
      }
    });
  };
};

export const merge = (a, b) =>
  (push) => {
    const kill1 = a(push);
    const kill2 = b(push);

    return () => {
      kill1();
      return kill2();
    };
  };
