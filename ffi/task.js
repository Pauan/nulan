import { crash } from "../util/error";
import { noop, try_catch } from "./util";
import * as $array from "../util/array";


export const make_thread = () => {
  return {
    a: false, // is_killed
    b: noop   // on_kill
  };
};

const on_kill = (thread, f) => {
  if (thread.a) {
    return crash(new Error("invalid on_kill: thread is killed"));

  } else if (f === noop) {
    return crash(new Error("invalid on_kill: use reset_on_kill instead"));

  } else if (thread.b === noop) {
    thread.b = f;

  } else {
    return crash(new Error("invalid on_kill: thread has a pending on_kill"));
  }
};

const reset_on_kill = (thread) => {
  if (thread.a) {
    return crash(new Error("invalid reset_on_kill: thread is killed"));

  } else if (thread.b === noop) {
    return crash(new Error("invalid reset_on_kill: thread does not have a pending on_kill"));

  } else {
    thread.b = noop;
  }
};

export const kill_thread = (thread) => {
  if (thread.a) {
    return crash(new Error("invalid kill: thread is already killed"));

  } else {
    const on_kill = thread.b;

    thread.a = true;
    thread.b = noop;

    return on_kill();
  }
};

export const kill_threads = (a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    kill_thread(a[i]);
  }
};

// TODO should this use try_catch ?
export const run_in_thread = (thread, task, on_success, on_error) => {
  if (thread.a) {
    return crash(new Error("cannot run: thread is killed"));

  } else {
    return task(thread, on_success, on_error);
  }
};


export const make_thread_pool = (on_error) => {
  const pool = {
    // is_killed
    a: false,

    // threads
    b: [],

    // on_error
    // TODO is this a good idea ?
    // TODO does this need to use try_catch ?
    c: (value) => {
      // TODO is this a good idea ?
      kill_thread_pool(pool);
      return on_error(value);
    }
  };

  return pool;
};

export const kill_thread_pool = (pool) => {
  if (pool.a) {
    return crash(new Error("invalid kill: thread pool is already killed"));

  } else {
    const threads = pool.b;

    pool.a = true;
    pool.b = null; // TODO what about type deoptimizations ?
    pool.c = noop;

    return kill_threads(threads);
  }
};

export const run_in_thread_pool = (pool, task) => {
  if (pool.a) {
    return crash(new Error("cannot run: thread pool is killed"));

  } else {
    const thread = make_thread();

    pool.b["push"](thread);

    const on_success = (value) =>
      $array.remove(pool.b, thread);

    return run_in_thread(thread, task, on_success, pool.c);
  }
};


// TODO test this
export const catch_error = (f) =>
  (thread, success, error) => {
    const x = try_catch(f, null);

    if (x.$ === 0) {
      return run_in_thread(thread, x.a,
        (value) => success({ $: 0, a: value }),
        (value) => success({ $: 1, a: value })
      );

    } else {
      return success(x);
    }
  };


// Guarantees:
// * success is ignored after being killed
// * does nothing when killed
export const ignore_kill = (task) =>
  (thread, success, error) => {
    let killed = false;

    const x = make_thread();

    on_kill(thread, () => {
      killed = true;
    });

    return run_in_thread(x, task,
      (value) => {
        if (!killed) {
          reset_on_kill(thread);
          return success(value);
        }
      },

      (value) => {
        // Never silently ignore errors
        if (killed) {
          return crash(value);

        } else {
          reset_on_kill(thread);
          return error(value);
        }
      }
    );
  };


// Guarantees:
// * success cannot be called after success, error, or kill
// * error cannot be called after success, error, or kill
// * will not be killed after success, error, or kill
export const async_killable = (f) =>
  (thread, success, error) => {
    let done = false;

    // TODO is this check needed ?
    on_kill(thread, () => {
      if (done) {
        return crash(new Error("invalid kill"));

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
          return crash(new Error("invalid success"));

        } else {
          done = true;
          reset_on_kill(thread);
          return success(value);
        }
      },

      (value) => {
        if (done) {
          return crash(new Error("invalid error"));

        } else {
          done = true;
          reset_on_kill(thread);
          return error(value);
        }
      }
    );
  };


// Guarantees:
// * success cannot be called after success or error
// * error cannot be called after success, error, or kill
// * success is ignored after being killed
// * does nothing when killed
export const async_unkillable = (f) =>
  // TODO this can be implemented more efficiently
  ignore_kill(async_killable((success, error) => {
    f(success, error);
    return noop;
  }));


// TODO handle cancellation ?
export const Promise_from = (task) =>
  // TODO can this be made more efficient ?
  new Promise((resolve, reject) => {
    const x = make_thread();
    return run_in_thread(x, task, resolve, reject);
  });

// TODO handle cancellation ?
export const from_Promise = (f) =>
  // TODO should this use a raw Task or async_unkillable ?
  async_unkillable((success, error) =>
    f()["then"](success, error));


// TODO test this
export const never = (thread, success, error) => {};


export const reply = (value) =>
  (thread, success, error) =>
    success(value);


export const throw_error = (value) =>
  (thread, success, error) =>
    error(value);


// TODO test this
const run_catch = (thread, f, value, success, error) => {
  // TODO can this be made more efficient ?
  const x = try_catch(f, value);

  if (x.$ === 0) {
    return run_in_thread(thread, x.a, success, error);

  } else {
    return error(x.a);
  }
};


export const chain = (task, f) =>
  (thread, success, error) =>
    run_in_thread(thread, task,
      (value) =>
        run_catch(thread, f, value, success, error),
      error);


// TODO code duplication with concurrent
export const concurrent_null = (tasks) =>
  (thread, success, error) => {
    const length = tasks["length"];

    if (length === 0) {
      return success(null);

    } else {
      let running = true;
      let pending = length;

      const threads = [];

      // TODO what about running ?
      on_kill(thread, () =>
        kill_threads(threads));

      const on_success = (value) => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          reset_on_kill(thread);
          return success(null);
        }
      };

      const on_error = (value) => {
        running = false;
        reset_on_kill(thread);
        kill_threads(threads);
        return error(value);
      };

      for (let i = 0; i < length; ++i) {
        if (running) {
          const thread = make_thread();

          threads["push"](thread);

          run_in_thread(thread, tasks[i], on_success, on_error);

        } else {
          // TODO is this correct ?
          break;
        }
      }
    }
  };


export const concurrent = (tasks) =>
  (thread, success, error) => {
    const length = tasks["length"];

    if (length === 0) {
      return success(tasks);

    } else {
      let running = true;
      let pending = length;

      const threads = [];
      const values = new Array(length);

      // TODO what about running ?
      on_kill(thread, () =>
        kill_threads(threads));

      const on_success = () => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          reset_on_kill(thread);
          return success(values);
        }
      };

      const on_error = (value) => {
        running = false;
        reset_on_kill(thread);
        kill_threads(threads);
        return error(value);
      };

      for (let i = 0; i < length; ++i) {
        if (running) {
          const thread = make_thread();

          threads["push"](thread);

          run_in_thread(thread, tasks[i],
            (value) => {
              values[i] = value;
              return on_success();
            },
            on_error);

        } else {
          // TODO is this correct ?
          break;
        }
      }
    }
  };


// TODO test this
// TODO use try_catch ?
export const transform2 = (task1, task2, f) =>
  (thread, success, error) => {
    const thread1 = make_thread();
    const thread2 = make_thread();

    let other = { $: 0 };

    on_kill(thread, () => {
      kill_thread(thread1);
      return kill_thread(thread2);
    });

    const on_error = (value) => {
      reset_on_kill(thread);
      kill_thread(thread1);
      kill_thread(thread2);

      return error(value);
    };

    run_in_thread(thread1, task1,
      (value) => {
        if (other.$ === 0) {
          other = { $: 1, a: value };

        } else {
          return success(f(value, other.a));
        }
      },
      on_error);

    // TODO what if task1 errors synchronously ?
    return run_in_thread(thread2, task2,
      (value) => {
        if (other.$ === 0) {
          other = { $: 1, a: value };

        } else {
          return success(f(other.a, value));
        }
      },
      on_error);
  };


export const fastest = (task1, task2) =>
  (thread, success, error) => {
    const thread1 = make_thread();
    const thread2 = make_thread();

    let running = true;

    on_kill(thread, () => {
      kill_thread(thread1);
      return kill_thread(thread2);
    });

    const on_done = () => {
      running = false;

      reset_on_kill(thread);
      kill_thread(thread1);
      kill_thread(thread2);
    };

    const on_success = (value) => {
      on_done();
      return success(value);
    };

    const on_error = (value) => {
      on_done();
      return error(value);
    };

    run_in_thread(thread1, task1, on_success, on_error);

    // TODO is this necessary ?
    if (running) {
      return run_in_thread(thread2, task2, on_success, on_error);
    }
  };


/*
Success create               | Success use | Success destroy                -> Success use
Success create               | Success use | Error destroy                  -> Error destroy
Success create               | Success use | Kill destroy + Success destroy -> Nothing
Success create               | Success use | Kill destroy + Error destroy   -> Crash destroy
Success create               | Error use   | Success destroy                -> Error use
Success create               | Error use   | Error destroy                  -> Error destroy
Success create               | Error use   | Kill destroy + Success destroy -> Crash use
Success create               | Error use   | Kill destroy + Error destroy   -> Crash destroy
Success create               | Kill use    | Success destroy                -> Nothing
Success create               | Kill use    | Error destroy                  -> Crash destroy
Error create                 |             |                                -> Error create
Kill create + Success create |             | Success destroy                -> Nothing
Kill create + Success create |             | Error destroy                  -> Crash destroy
Kill create + Error create   |             |                                -> Crash create
*/
// TODO is this correct ?
// TODO test this
// TODO use try_catch ?
export const with_resource = (create, use, destroy) =>
  (thread, success, error) => {
    const thread_create = make_thread();
    const thread_use    = make_thread();

    let killed = false;
    let resource = { $: 0 };

    on_kill(thread, () => {
      killed = true;

      kill_thread(thread_use);

      if (resource.$ === 1) {
        return run_catch(thread_create, destroy, resource.a, noop, crash);
      }
    });

    // TODO code duplication with ignore_kill
    const on_success = (value) => {
      if (!killed) {
        reset_on_kill(thread);
        return success(value);
      }
    };

    // TODO code duplication with ignore_kill
    const on_error = (value) => {
      if (killed) {
        return crash(value);

      } else {
        reset_on_kill(thread);
        return error(value);
      }
    };

    return run_in_thread(thread_create, create,
      (_resource) => {
        if (killed) {
          return run_catch(thread_create, destroy, _resource, noop, crash);

        } else {
          resource = { $: 1, a: _resource };

          return run_catch(thread_use, use, _resource,
            (value) => {
              resource = { $: 0 };

              return run_catch(thread_create, destroy, _resource,
                (_) =>
                  on_success(value),
                on_error);
            },

            (value) => {
              resource = { $: 0 };

              return run_catch(thread_create, destroy, _resource,
                (_) =>
                  on_error(value),
                on_error);
            });
        }
      },
      on_error);
  };


export const wait = (ms) => {
  if (ms === 0) {
    return crash(new Error("cannot wait for 0 milliseconds"));
  }

  if (ms < 0) {
    return crash(new Error("expected positive number but got " + ms));
  }

  return async_killable((success, error) => {
    const x = setTimeout(() => success(null), ms);
    return () => clearTimeout(x);
  });
};


export const make_thread_run = (task) => {
  const x = make_thread();
  run_in_thread(x, task, noop, crash);
  return x;
};
