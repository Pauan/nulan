import { crash, error_from } from "./crash";
import { noop, try_catch } from "./util";
import { make_thread, run_in_thread, on_thread_kill,
         thread_success, thread_error, run_in_sub_thread,
         thread_kill, kill_threads } from "./unsafe/thread.js";


// TODO test this
const run_catch = (thread, f, success, error) =>
  // TODO can this be made more efficient ?
  try_catch(f)((x) => run_in_sub_thread(thread, x, success, error), error);


// TODO test this
export const catch_error = (success, failure, f) =>
  (thread) =>
    run_catch(thread, () => f(null),
      (value) =>
        thread_success(thread, success(value)),
      (value) =>
        thread_success(thread, failure(value)));


// Guarantees:
//   1) success is ignored after being killed
//   2) does nothing when killed
export const ignore_kill = (task) =>
  (thread) => {
    let killed = false;

    on_thread_kill(thread, () => {
      killed = true;
    });

    return run_in_new_thread(task,
      (value) => {
        if (!killed) {
          return thread_success(thread, value);
        }
      },
      (value) => {
        // Never silently ignore errors
        if (killed) {
          return crash(value);

        } else {
          return thread_error(thread, value);
        }
      });
  };


// Guarantees:
//   1) success cannot be called after success, error, or kill
//   2) error cannot be called after success, error, or kill
//   3) will not be killed after success, error, or kill
export const async_killable = (f) =>
  (thread) => {
    let done = false;

    // TODO is this check needed ?
    on_thread_kill(thread, () => {
      if (done) {
        return crash(error_from("invalid kill"));

      } else {
        done = true;
        // TODO is it possible for this to happen before `f` returns ?
        return kill();
      }
    });

    // TODO use try_catch ?
    const kill = f(
      (value) => {
        if (done) {
          return crash(error_from("invalid success"));

        } else {
          done = true;
          return thread_success(thread, value);
        }
      },

      (value) => {
        if (done) {
          return crash(error_from("invalid error"));

        } else {
          done = true;
          return thread_error(thread, value);
        }
      }
    );
  };


// Guarantees:
//   1) success cannot be called after success or error
//   2) error cannot be called after success, error, or kill
//   3) success is ignored after being killed
//   4) does nothing when killed
export const async_unkillable = (f) =>
  // TODO this can be implemented more efficiently
  ignore_kill(async_killable((success, error) => {
    f(success, error);
    return noop;
  }));


// TODO handle cancellation ?
export const Promise_from = (task) =>
  // TODO can this be made more efficient ?
  new Promise((resolve, reject) =>
    run_in_new_thread(task, resolve, reject));

// TODO handle cancellation ?
export const from_Promise = (f) =>
  // TODO should this use a raw Task or async_unkillable ?
  async_unkillable((success, error) =>
    f().then(success, error));


// TODO test this
export const never = (thread) => {};


export const wrap = (value) =>
  (thread) =>
    thread_success(thread, value);


export const throw_error = (value) =>
  (thread) =>
    thread_error(thread, value);


export const chain = (task, f) =>
  (thread) => {
    const on_success = (value) =>
      thread_success(thread, value);

    const on_error = (value) =>
      thread_error(thread, value);

    return run_in_sub_thread(thread, task,
      (value) =>
        run_catch(thread, () => f(value), on_success, on_error),
      on_error);
  };


// TODO code duplication with concurrent
export const concurrent_null = (tasks) =>
  (thread) => {
    const length = tasks.length;

    if (length === 0) {
      return thread_success(thread, null);

    } else {
      let running = true;
      let pending = length;

      const threads = [];

      // TODO what about running ?
      on_thread_kill(thread, () =>
        kill_threads(threads));

      const on_success = (value) => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          return thread_success(thread, null);
        }
      };

      const on_error = (value) => {
        running = false;
        kill_threads(threads);
        return thread_error(thread, value);
      };

      for (let i = 0; i < length; ++i) {
        if (running) {
          const thread = make_thread(on_success, on_error);

          threads.push(thread);

          run_in_thread(thread, tasks[i]);

        } else {
          // TODO is this correct ?
          break;
        }
      }
    }
  };


export const concurrent = (tasks) =>
  (thread) => {
    const length = tasks.length;

    if (length === 0) {
      return thread_success(thread, tasks);

    } else {
      let running = true;
      let pending = length;

      const threads = [];
      const values = new Array(length);

      // TODO what about running ?
      on_thread_kill(thread, () =>
        kill_threads(threads));

      const on_success = () => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          return thread_success(thread, values);
        }
      };

      const on_error = (value) => {
        running = false;
        kill_threads(threads);
        return thread_error(thread, value);
      };

      for (let i = 0; i < length; ++i) {
        if (running) {
          const thread = make_thread(
            (value) => {
              values[i] = value;
              return on_success();
            },
            on_error
          );

          threads.push(thread);

          run_in_thread(thread, tasks[i]);

        } else {
          // TODO is this correct ?
          break;
        }
      }
    }
  };


// TODO test this
export const transform2 = (task1, task2, f) =>
  (thread) => {
    let other = { $: 0 };

    let running = true;

    const on_success = (value) =>
      thread_success(thread, value);

    const on_error = (value) => {
      running = false;
      thread_kill(thread1);
      thread_kill(thread2);

      return thread_error(thread, value);
    };

    const thread1 = make_thread(
      (value) => {
        if (other.$ === 0) {
          other = { $: 1, a: value };

        } else {
          // TODO is try_catch necessary ?
          return try_catch(() => f(value, other.a))(on_success, on_error);
        }
      },
      on_error
    );

    const thread2 = make_thread(
      (value) => {
        if (other.$ === 0) {
          other = { $: 1, a: value };

        } else {
          // TODO is try_catch necessary ?
          return try_catch(() => f(other.a, value))(on_success, on_error);
        }
      },
      on_error
    );

    on_thread_kill(thread, () => {
      thread_kill(thread1);
      return thread_kill(thread2);
    });

    run_in_thread(thread1, task1);

    // TODO is this needed ?
    // TODO test this
    if (running) {
      return run_in_thread(thread2, task2);
    }
  };


export const fastest = (task1, task2) =>
  (thread) => {
    let running = true;

    const on_done = () => {
      running = false;

      thread_kill(thread1);
      return thread_kill(thread2);
    };

    const on_success = (value) => {
      on_done();
      return thread_success(thread, value);
    };

    const on_error = (value) => {
      on_done();
      return thread_error(thread, value);
    };

    const thread1 = make_thread(on_success, on_error);
    const thread2 = make_thread(on_success, on_error);

    // TODO use on_done ?
    on_thread_kill(thread, () => {
      thread_kill(thread1);
      return thread_kill(thread2);
    });

    run_in_thread(thread1, task1);

    // TODO is this necessary ?
    if (running) {
      return run_in_thread(thread2, task2);
    }
  };


// Guarantees:
//   1) if create is killed, then it is ignored
//   2) if create is killed, then destroy is run
//   3) if destroy is killed, then it is ignored
//   4) if use succeeds, errors, or is killed, then destroy is run
/*
            | Success create | Success use |              | Success destroy -> Success use
            | Success create | Success use |              | Error destroy   -> Error destroy
            | Success create | Success use | Kill destroy | Success destroy -> Nothing
            | Success create | Success use | Kill destroy | Error destroy   -> Crash destroy
            | Success create | Error use   |              | Success destroy -> Error use
            | Success create | Error use   |              | Error destroy   -> Error destroy
            | Success create | Error use   | Kill destroy | Success destroy -> Crash use
            | Success create | Error use   | Kill destroy | Error destroy   -> Crash destroy
            | Success create | Kill use    |              | Success destroy -> Nothing
            | Success create | Kill use    |              | Error destroy   -> Crash destroy
            | Error create   |             |              |                 -> Error create
Kill create | Success create |             |              | Success destroy -> Nothing
Kill create | Success create |             |              | Error destroy   -> Crash destroy
Kill create | Error create   |             |              |                 -> Crash create
*/
// TODO is this correct ?
// TODO test this
export const with_resource = (create, use, destroy) =>
  (thread) => {
    let killed = false;
    let resource = { $: 0 };

    on_thread_kill(thread, () => {
      killed = true;

      thread_kill(thread_use);

      if (resource.$ === 1) {
        // TODO is this correct ? should this store `resource.a` in a variable ?
        // TODO don't use run_in_sub_thread ?
        return run_catch(thread_create, () => destroy(resource.a), noop, crash);
      }
    });

    // TODO code duplication with ignore_kill
    const on_success = (value) => {
      if (!killed) {
        return thread_success(thread, value);
      }
    };

    // TODO code duplication with ignore_kill
    const on_error = (value) => {
      if (killed) {
        return crash(value);

      } else {
        return thread_error(thread, value);
      }
    };

    const thread_use = make_thread(noop, crash);

    const thread_create = make_thread(
      (_resource) => {
        if (killed) {
          // TODO don't use run_in_sub_thread ?
          return run_catch(thread_create, () => destroy(_resource), noop, crash);

        } else {
          resource = { $: 1, a: _resource };

          // TODO don't use run_in_sub_thread ?
          return run_catch(thread_use, () => use(_resource),
            (value) => {
              resource = { $: 0 };

              // TODO don't use run_in_sub_thread ?
              return run_catch(thread_create, () => destroy(_resource),
                (_) =>
                  on_success(value),
                on_error);
            },

            (value) => {
              resource = { $: 0 };

              // TODO don't use run_in_sub_thread ?
              return run_catch(thread_create, () => destroy(_resource),
                (_) =>
                  on_error(value),
                on_error);
            });
        }
      },
      on_error
    );

    return run_in_thread(thread_create, create);
  };


export const wait = (ms) => {
  if (ms === 0) {
    return crash(error_from("cannot wait for 0 milliseconds"));
  }

  if (ms < 0) {
    return crash(error_from("expected positive number but got " + ms));
  }

  return async_killable((success, error) => {
    const x = setTimeout(() => success(null), ms);
    return () => clearTimeout(x);
  });
};


// TODO is this correct ?
// TODO remove this ?
export const new_thread = (task) =>
  // TODO maybe use async_unkillable instead ?
  async_killable((success, error) => {
    const thread = make_thread(noop, crash);

    run_in_thread(thread, task);

    success(thread);

    return () =>
      thread_kill(thread);
  });


export const unsafe_execute_main = (task) => {
  run_in_new_thread(task, noop, crash);
  return null;
};
