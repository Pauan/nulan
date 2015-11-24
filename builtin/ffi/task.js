// TODO move to another module
const crash = (e) => {
  throw e;
};

// TODO move to another module
const _null = 0;

const noop = () => {};


export const sync = (f) =>
  (thread, success, error) => {
    // TODO does this need to set cancel to noop ?
    // TODO maybe use try/catch ?
    success(f());
  };

// Guarantees:
// * success cannot be called after success or error
// * error cannot be called after success or error
// * cleanup will not be run after success or error
// * cleanup will not be run twice
// * success or error is ignored after cleanup
export const async = (f) =>
  (thread, success, error) => {
    let done = false;

    console.log(new Error().stack);

    on_kill(thread, f((value) => {
      if (done) {
        crash(new Error("Invalid success"));
      } else {
        done = true;
        success(value);
      }

    }, (value) => {
      if (done) {
        crash(new Error("Invalid error"));
      } else {
        done = true;
        error(value);
      }
    }));
  };


const make_thread = () => {
  return {
    a: noop,
    b: []
  };
};

const on_kill = (thread, f) => {
  thread.a = f;
};

const off_kill = (thread) => {
  thread.a = noop;
};

// TODO guarantee that it can't be killed twice ?
const kill = (thread) => {
  thread.a();
};

const kill_all = (a) => {
  for (let i = 0; i < a["length"]; ++i) {
    kill(a[i]);
  }
};


export const wrap = (value) =>
  (thread, success, error) => {
    // TODO does this need to set cancel to noop ?
    success(value);
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

// TODO test this
export const sequential = (a) =>
  (thread, success, error) => {
    const length = a["length"];
    const values = new Array(length);

    const loop = (i) => {
      if (i < length) {
        a[i](thread, (value) => {
          values[i] = value;
          loop(i + 1);
        }, error);

      } else {
        success(values);
      }
    };

    loop(0);
  };

// TODO test this
export const concurrent = (a) =>
  (thread, success, error) => {
    let pending = a["length"];

    const running = new Array(pending);
    const values  = new Array(pending);

    let done = false;

    const kill = () => {
      if (done) {
        // TODO better error message ?
        crash(new Error("Invalid"));

      } else {
        done = true;
        kill_all(running);
      }
    };

    const on_success = () => {
      --pending;

      if (pending === 0) {
        done = true;
        success(values);
      }
    };

    // TODO what if a task is succeeded/errored immediately ?
    for (let i = 0; i < a["length"]; ++i) {
      running[i] = make_thread();

      a[i](running[i], (value) => {
        // TODO is this correct ?
        off_kill(running[i]);
        values[i] = value;
        on_success();

      }, (value) => {
        // TODO is this correct ?
        off_kill(running[i]);
        kill();
        error(value);
      });
    }

    on_kill(thread, kill);
  };

// TODO test this
export const fastest = (a) =>
  (thread, success, error) => {
    const running = new Array(a["length"]);

    let done = false;

    const kill = () => {
      if (done) {
        // TODO better error message ?
        crash(new Error("Invalid"));

      } else {
        done = true;
        kill_all(running);
      }
    };

    // TODO what if a task is succeeded/errored immediately ?
    for (let i = 0; i < a["length"]; ++i) {
      running[i] = make_thread();

      a[i](running[i], (value) => {
        // TODO is this correct ?
        off_kill(running[i]);
        kill();
        success(value);

      }, (value) => {
        // TODO is this correct ?
        off_kill(running[i]);
        kill();
        error(value);
      });
    }

    on_kill(thread, kill);
  };


// TODO is this correct ?
// TODO test this
/*export const _finally = (use, destroy) =>
  (thread, success, error) => {
    let succeeded = false;

    const x = make_thread();

    use(x, (value) => {
      succeeded = true;

      destroy(x, (_) => {
        success(value);
      }, error);

    }, (value) => {
      succeeded = true;

      destroy(x, (_) => {
        error(value);
      }, error);
    });

    on_kill(thread, () => {
      if (!succeeded) {
        succeeded = true;

        kill(x);
        destroy(x, noop, error);
      }
    });
  };*/

// TODO is this correct ?
// TODO test this
export const with_resource = (create, use, destroy) =>
  (thread, success, error) => {
    let killed    = false;
    let created   = false;
    let succeeded = false;

    const x = make_thread();

    create(x, (resource) => {
      created = true;

      if (killed) {
        destroy(resource)(x, noop, error);

      } else {
        use(resource)(x, (value) => {
          succeeded = true;

          destroy(resource)(x, (_) => {
            success(value);
          }, error);

        }, (value) => {
          succeeded = true;

          destroy(resource)(x, (_) => {
            error(value);
          }, error);
        });
      }
    }, error);

    on_kill(thread, () => {
      killed = true;

      if (created && !succeeded) {
        succeeded = true;

        kill(x);
        destroy(value)(x, noop, error);
      }
    });
  };


const _yield =
  async((success, error) => {
    const x = setImmediate(() => {
      success(_null);
    });

    return () => {
      clearImmediate(x);
    };
  });

export const delay = (ms) =>
  async((success, error) => {
    const x = setTimeout(() => {
      success(_null);
    }, ms);

    return () => {
      clearTimeout(x);
    };
  });

export const log = (s) =>
  sync(() => {
    console["log"](s);
    return _null;
  });

//const after = (task, f) =>
//  (thread, success, error) => {
//    task(thread, (value) => {
//      // Tail call, to allow for infinite recursion
//      f(value)(thread, success, error);
//    }, error);
//  };

const after = (a, f) =>
  flatten(transform(a, f));

const then = (a, b) =>
  after(a, (_) => b);

const forever = (a) =>
  after(a, (_) =>
    forever(a));


export const perform = (task) => {
  const x = make_thread();

  task(x, noop, crash);
};


const fs = require("fs");

const callback = (success, error) =>
  (err, value) => {
    if (err) {
      error(err);
    } else {
      success(value);
    }
  };

const callback_null = (success, error) =>
  (err) => {
    if (err) {
      error(err);
    } else {
      success(_null);
    }
  };

const read_file = (path) =>
  async((success, error) => {
    fs["readFile"](path, { "encoding": "utf8" }, callback(success, error));
    return noop;
  });

const write_file = (path, x) =>
  async((success, error) => {
    fs["writeFile"](path, x, { "encoding": "utf8" }, callback_null(success, error));
    return noop;
  });

const open_file = (path, flags, mode) =>
  async((success, error) => {
    fs["open"](path, flags, mode, callback(success, error));
    return noop;
  });

const close_fd = (fd) =>
  async((success, error) => {
    fs["close"](fd, callback_null(success, error));
    return noop;
  });

const with_file = (path, flags, mode, f) =>
  with_resource(open_file(path, flags, mode), f, close_fd);



const x = fastest([
            forever(_yield),
            delay(10000)
          ]);

var start = Date.now();
console.log("STARTING");

x(make_thread(), (value) => {
  console.log("VALUE", Date.now() - start, value);
}, (error) => {
  console.log("ERROR", Date.now() - start, error);
});


flatten(transform(read_file("foo"), (x) => write_file("bar", x)));
