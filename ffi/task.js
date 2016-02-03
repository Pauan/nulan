import { crash } from "../util/error";
import { _null, none, some } from "./types";
import { noop, try_catch } from "./util";
import * as $array from "../util/array";


export const make_thread = () => {
  return {
    a: false, // is_killed
    b: null   // on_kill
  };
};

const _set_kill = (thread, f) => {
  if (thread.a) {
    crash(new Error("Invalid set_kill: thread is already killed"));

  } else if (f === null) {
    crash(new Error("Invalid set_kill: use reset_kill instead"));

  } else if (thread.b === null) {
    thread.b = f;

  } else {
    crash(new Error("Invalid set_kill: thread is already set"));
  }
};

const _reset_kill = (thread) => {
  if (thread.a) {
    crash(new Error("Invalid reset_kill: thread is already killed"));

  } else if (thread.b === null) {
    crash(new Error("Invalid reset_kill: thread is already reset"));

  } else {
    thread.b = null;
  }
};

export const kill_thread = (thread) => {
  if (thread.a) {
    crash(new Error("Invalid kill: thread is already killed"));

  } else {
    const kill = thread.b;

    thread.a = true;
    thread.b = null;

    if (kill !== null) {
      kill();
    }
  }
};

export const kill_threads = (a) => {
  for (let i = 0; i < a["length"]; ++i) {
    kill_thread(a[i]);
  }
};

// TODO rename to run_in_thread
export const run = (task, thread, on_success, on_error) => {
  if (thread.a) {
    crash(new Error("Cannot run: thread is killed"));

  } else {
    task(thread, on_success, on_error);
  }
};

export const make_thread_pool = (on_error) => {
  const pool = {
    // is_killed
    a: false,

    // threads
    b: [],

    // on_error
    c: (e) => {
      kill_thread_pool(pool);
      on_error(e);
    }
  };

  return pool;
};

export const kill_thread_pool = (pool) => {
  if (pool.a) {
    crash(new Error("Invalid kill: thread pool is already killed"));

  } else {
    const threads = pool.b;

    pool.a = true;
    pool.b = null;
    pool.c = null;

    kill_threads(threads);
  }
};

export const run_in_thread_pool = (pool, task) => {
  if (pool.a) {
    crash(new Error("Cannot run: thread pool is killed"));

  } else {
    const thread = make_thread();

    pool.b["push"](thread);

    task(thread, () => {
      $array.remove(pool.b, thread);
    }, pool.c);
  }
};


export const sync = (f) =>
  (thread, success, error) => {
    // TODO maybe use try/catch ?
    success(f());
  };

// TODO merge this into sync ?
export const catch_error = (f) =>
  (thread, success, error) => {
    const x = try_catch(f);

    if (x.$ === 0) {
      success(x.a);

    } else {
      error(x.a);
    }
  };

// Guarantees:
// * success is ignored after being killed
// * does nothing when killed
export const ignore_kill = (task) =>
  (thread, success, error) => {
    let killed = false;

    _set_kill(thread, () => {
      killed = true;
    });

    const x = make_thread();

    const on_success = (value) => {
      if (!killed) {
        _reset_kill(thread);
        success(value);
      }
    };

    const on_error = (value) => {
      // Never silently ignore errors
      if (killed) {
        crash(value);

      } else {
        _reset_kill(thread);
        error(value);
      }
    };

    task(x, on_success, on_error);
  };

// Guarantees:
// * success cannot be called after success, error, or kill
// * error cannot be called after success, error, or kill
// * will not be killed after success, error, or kill
export const async_killable = (f) =>
  (thread, success, error) => {
    let done = false;

    const on_success = (value) => {
      if (done) {
        crash(new Error("Invalid success"));

      } else {
        done = true;
        _reset_kill(thread);
        success(value);
      }
    };

    const on_error = (value) => {
      if (done) {
        crash(new Error("Invalid error"));

      } else {
        done = true;
        _reset_kill(thread);
        error(value);
      }
    };

    // TODO is this check needed ?
    _set_kill(thread, () => {
      if (done) {
        crash(new Error("Invalid kill"));

      } else {
        done = true;
        kill();
      }
    });

    const kill = f(on_success, on_error);
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
  new Promise((resolve, reject) => {
    const x = make_thread();
    task(x, resolve, reject);
  });

// TODO handle cancellation ?
export const from_Promise = (f) =>
  // TODO should this use a raw Task or async_unkillable ?
  async_unkillable((success, error) => {
    f()["then"](success, error);
  });


export const never = (thread, success, error) => {};

export const wrap = (value) =>
  (thread, success, error) => {
    success(value);
  };

export const throw_error = (value) =>
  (thread, success, error) => {
    error(value);
  };

export const transform = (task, f) =>
  (thread, success, error) => {
    task(thread, (value) => {
      success(f(value));
    }, error);
  };

export const flatten = (task) =>
  (thread, success, error) => {
    task(thread, (task) => {
      // Tail call, to allow for infinite recursion
      task(thread, success, error);
    }, error);
  };

// TODO code duplication with concurrent
export const concurrent_null = (a) =>
  (thread, success, error) => {
    let pending = a["length"];

    if (pending === 0) {
      success(_null);

    } else {
      let running = true;

      const threads = [];

      // TODO is this the correct place for this ?
      // TODO what about running ?
      _set_kill(thread, () => {
        kill_threads(threads);
      });

      const on_success = (_) => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          _reset_kill(thread);
          success(_null);
        }
      };

      const on_error = (value) => {
        running = false;
        _reset_kill(thread);
        kill_threads(threads);
        error(value);
      };

      for (let i = 0; i < a["length"]; ++i) {
        if (running) {
          threads["push"](make_thread());

          a[i](threads[i], on_success, on_error);

        } else {
          // TODO is this correct ?
          return;
        }
      }
    }
  };

export const concurrent = (a) =>
  (thread, success, error) => {
    let pending = a["length"];

    const values = new Array(pending);

    if (pending === 0) {
      success(values);

    } else {
      let running = true;

      const threads = [];

      // TODO is this the correct place for this ?
      // TODO what about running ?
      _set_kill(thread, () => {
        kill_threads(threads);
      });

      const on_success = () => {
        --pending;

        // TODO what about setting `running` ?
        if (pending === 0) {
          _reset_kill(thread);
          success(values);
        }
      };

      const on_error = (value) => {
        running = false;
        _reset_kill(thread);
        kill_threads(threads);
        error(value);
      };

      for (let i = 0; i < a["length"]; ++i) {
        if (running) {
          threads["push"](make_thread());

          a[i](threads[i], (value) => {
            values[i] = value;
            on_success();
          }, on_error);

        } else {
          // TODO is this correct ?
          return;
        }
      }
    }
  };

export const fastest = (task_a, task_b) =>
  (thread, success, error) => {
    let running = true;

    const thread_a = make_thread();
    const thread_b = make_thread();

    // TODO what about running ?
    _set_kill(thread, () => {
      kill_thread(thread_a);
      kill_thread(thread_b);
    });

    const on_success = (value) => {
      running = false;
      _reset_kill(thread);
      kill_thread(thread_a);
      kill_thread(thread_b);
      success(value);
    };

    const on_error = (value) => {
      running = false;
      _reset_kill(thread);
      kill_thread(thread_a);
      kill_thread(thread_b);
      error(value);
    };

    task_a(thread_a, on_success, on_error);

    if (running) {
      task_b(thread_b, on_success, on_error);
    }
  };


// TODO maybe handle kill ?
export const on_error = (task, on_success, on_error) =>
  (thread, success, error) => {
    task(thread, (value) => {
      success(on_success(value));
    }, (value) => {
      success(on_error(value));
    });
  };

// TODO is this correct ?
// TODO test this
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
export const with_resource = (create, use, destroy) =>
  (thread, success, error) => {
    const x = make_thread();
    const y = make_thread();

    let killed = false;
    let resource = none;

    _set_kill(thread, () => {
      killed = true;

      if (resource.$ === 1) {
        kill_thread(y);
        destroy(resource.a)(x, noop, crash);
      }
    });

    const on_success = (o) => {
      if (killed) {
        destroy(o)(x, noop, crash);

      } else {
        resource = some(o);

        const _success = (value) => {
          resource = none;

          destroy(o)(x, (_) => {
            if (!killed) {
              _reset_kill(thread);
              success(value);
            }
          }, on_error);
        };

        const _error = (value) => {
          resource = none;

          destroy(o)(x, (_) => {
            on_error(value);
          }, on_error);
        };

        use(o)(y, _success, _error);
      }
    };

    // TODO code duplication with ignore_kill
    const on_error = (value) => {
      if (killed) {
        crash(value);

      } else {
        _reset_kill(thread);
        error(value);
      }
    };

    create(x, on_success, on_error);
  };


const _yield_queue = [];

const _yield_queue_run = () => {
  const pending = _yield_queue["length"];

  for (let i = 0; i < pending; ++i) {
    // TODO faster implementation for this ?
    _yield_queue["shift"]()(_null);
  }

  if (_yield_queue["length"] !== 0) {
    setTimeout(_yield_queue_run, 0);
  }
};

const _yield_queue_add = (x) => {
  _yield_queue["push"](x);

  if (_yield_queue["length"] === 1) {
    setTimeout(_yield_queue_run, 0);
  }
};

// TODO guarantee that this cannot happen while calling _yield_queue_run ?
const _yield_queue_remove = (x) => {
  $array.remove(_yield_queue, x);
};

// TODO is this correct ?
export const _yield =
  async_killable((success, error) => {
    _yield_queue_add(success);

    return () => {
      _yield_queue_remove(success);
    };
  });


export const delay = (ms) => {
  if (ms === 0) {
    crash(new Error("Cannot delay for 0 milliseconds (maybe use yield instead?)"));
  }

  if (ms < 0) {
    crash(new Error("Expected positive number but got " + ms));
  }

  return async_killable((success, error) => {
    const x = setTimeout(() => {
      success(_null);
    }, ms);

    return () => {
      clearTimeout(x);
    };
  });
};

export const log = (s) =>
  sync(() => {
    console["log"](s);
    return _null;
  });


export const make_thread_run = (task) => {
  const x = make_thread();
  task(x, noop, crash);
  return x;
};



/*const x = fastest(
            forever(_yield),
            delay(10000)
          );

var start = Date.now();
console.log("STARTING");

x(_make_thread(), (value) => {
  console.log("VALUE", Date.now() - start, value);
}, (error) => {
  console.log("ERROR", Date.now() - start, error);
});


flatten(transform(read_file("foo"), (x) => write_file("bar", x)));
*/
