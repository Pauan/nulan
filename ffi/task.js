import { crash } from "../util/error";
import { _null } from "./types";
import { noop, try_catch } from "./util";


const _make_thread = () => {
  return {
    a: false, // is_killed
    b: null   // kill
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

const _kill = (thread) => {
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

const _kill_all = (a) => {
  for (let i = 0; i < a["length"]; ++i) {
    _kill(a[i]);
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

    const x = _make_thread();

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


export const make_thread = (task) =>
  sync(() => {
    const thread = _make_thread();

    // TODO use perform ?
    task(thread, noop, crash);

    return thread;
  });

export const kill_thread = (thread) =>
  sync(() => {
    _kill(thread);
    return _null;
  });


// TODO handle cancellation ?
export const Promise_from = (task) =>
  new Promise((resolve, reject) => {
    const x = _make_thread();
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
        _kill_all(threads);
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
        _kill_all(threads);
        error(value);
      };

      for (let i = 0; i < a["length"]; ++i) {
        if (running) {
          threads["push"](_make_thread());

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
        _kill_all(threads);
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
        _kill_all(threads);
        error(value);
      };

      for (let i = 0; i < a["length"]; ++i) {
        if (running) {
          threads["push"](_make_thread());

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

    const thread_a = _make_thread();
    const thread_b = _make_thread();

    // TODO what about running ?
    _set_kill(thread, () => {
      _kill(thread_a);
      _kill(thread_b);
    });

    const on_success = (value) => {
      running = false;
      _reset_kill(thread);
      _kill(thread_a);
      _kill(thread_b);
      success(value);
    };

    const on_error = (value) => {
      running = false;
      _reset_kill(thread);
      _kill(thread_a);
      _kill(thread_b);
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
Success create | Success use | Success destroy -> Success use
Success create | Error use   | Success destroy -> Error use
Success create | Kill use    | Success destroy -> Nothing
Success create | Success use | Error destroy   -> Error destroy
Success create | Error use   | Error destroy   -> Error destroy
Success create | Kill use    | Error destroy   -> Crash destroy
Success create | Success use | Kill destroy    -> Nothing
Success create | Error use   | Kill destroy    -> Crash use
Error create   |             |                 -> Error create
Kill create    |             | Success destroy -> Nothing
Kill create    |             | Error destroy   -> Crash destroy
*/
export const with_resource = (create, use, destroy) =>
  (thread, success, error) => {
    let killed = false;

    const on_kill = () => {
      killed = true;
    };

    _set_kill(thread, on_kill);

    const x = _make_thread();

    const on_error = (value) => {
      /*if (killed) {
        crash(value);

      } else {*/
        _reset_kill(thread);
        error(value);
      //}
    };

    create(x, (resource) => {
      if (killed) {
        destroy(resource)(x, noop, crash);

      } else {
        // TODO is this correct ?
        _set_kill(thread, () => {
          _kill(x);
          destroy(resource)(x, noop, crash);
        });

        use(resource)(x, (value) => {
          _set_kill(thread, on_kill);

          destroy(resource)(x, (_) => {
            // TODO is this correct ?
            if (!killed) {
              _reset_kill(thread);
              success(value);
            }
          }, on_error);

        }, (value) => {
          _set_kill(thread, on_kill);

          destroy(resource)(x, (_) => {
            on_error(value);
          }, on_error);
        });
      }
    }, on_error);
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
  const index = _yield_queue["indexOf"](x);

  if (index === -1) {
    crash(new Error("Invalid: could not kill yield (this should never happen)"));
  } else {
    _yield_queue["splice"](index, 1);
  }
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


export const perform = (task) => {
  const x = _make_thread();
  task(x, noop, crash);
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