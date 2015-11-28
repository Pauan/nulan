import { assert_crash, format_error, format_pretty, equal } from "../assert";
import { map } from "../../util/array";
import { crash, get_message } from "../../util/node";
import { sync, transform, flatten, perform, wrap, sequential,
         concurrent, delay, fastest, _yield, throw_error,
         ignore_kill, on_error, async_killable,
         async_unkillable, log, never,
         make_thread, kill_thread } from "../../ffi/task";


const after = (a, f) =>
  flatten(transform(a, f));

const then = (a, b) =>
  after(a, (_) => b);

const forever = (a) =>
  after(a, (_) =>
    forever(a));

const killed = (a, value) =>
  after(make_thread(a), (thread) =>
    transform(kill_thread(thread), (_) => value));

const ignore_yield = ignore_kill(_yield);

// TODO handle 0
const repeat = (x, i) => {
  if (i === 1) {
    return x;
  } else {
    return after(x, (_) => repeat(x, i - 1));
  }
};


const counter = (f) => {
  let _counter = 0;

  const increment = sync(() => {
    _counter += 1;
    return null;
  });

  return transform(f(increment), (_) => _counter);
};


const test_group = (group_name, a) =>
  then(concurrent(map(a, (f, index) => {
                    const name = group_name + " (test " + (index + 1) + ")";
                    return fastest([
                      f(name),
                      then(delay(10000),
                           throw_error(new Error(name + " took too long")))
                    ]);
                  })),
       log(group_name + ": all tests succeeded"));

const expect = (expected, task) =>
  (name) =>
    after(task, (value) =>
      (equal(value, expected)
        ? wrap(null)
        : throw_error(new Error(format_pretty(name, value, expected)))));

const expect_crash = (expected, fn) =>
  (name) =>
    after(on_error(fn(), (_) => null, (e) => e), (e) =>
      (e === null
        ? throw_error(new Error(format_error(name, null, expected)))
        : (get_message(e) === expected
            ? wrap(null)
            : throw_error(new Error(format_error(name, get_message(e), expected))))));


/*perform(fastest([
  ignore_kill(forever(then(_yield, log("Hi")))),
  delay(1000)
]));*/


assert_crash(() => {
  delay(-5);
}, "Expected positive number but got -5");

assert_crash(() => {
  delay(0);
}, "Cannot delay for 0 milliseconds (maybe use yield instead?)");

assert_crash(() => {
  sequential([]);
}, "Cannot use sequential on an empty list");

assert_crash(() => {
  concurrent([]);
}, "Cannot use concurrent on an empty list");

assert_crash(() => {
  fastest([]);
}, "Cannot use fastest on an empty list");


export const tests = test_group("task.js", [
  expect("1",
    async_killable((success, error) => {
      success("1");

      assert_crash(() => {
        success("2");
      }, "Invalid success");
    })),

  expect("1",
    async_killable((success, error) => {
      success("1");

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1", () =>
    async_killable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1", () =>
    async_killable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        success("2");
      }, "Invalid success");
    })),

  expect("2",
    killed(async_killable((success, error) => {
      return () => {
        assert_crash(() => {
          success("1");
        }, "Invalid success");
      };
    }), "2")),

  expect("2",
    killed(async_killable((success, error) => {
      return () => {
        assert_crash(() => {
          error(new Error("1"));
        }, "Invalid error");
      };
    }), "2")),

  expect("2",
    killed(async_killable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          success("1");
        }, "Invalid success");
      }, 0);
      return () => {};
    }), "2")),

  expect("2",
    killed(async_killable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          error(new Error("1"));
        }, "Invalid error");
      }, 0);
      return () => {};
    }), "2")),


  expect("1",
    async_unkillable((success, error) => {
      success("1");

      assert_crash(() => {
        success("2");
      }, "Invalid success");
    })),

  expect("1",
    async_unkillable((success, error) => {
      success("1");

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1", () =>
    async_unkillable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1", () =>
    async_unkillable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        success("2");
      }, "Invalid success");
    })),

  expect("2",
    killed(async_unkillable((success, error) => {
      success("1");
    }), "2")),

  expect("2",
    killed(async_unkillable((success, error) => {
      assert_crash(() => {
        error(new Error("1"));
      }, "1");
    }), "2")),

  expect("2",
    killed(async_unkillable((success, error) => {
      setTimeout(() => {
        success("1");
      }, 0);
    }), "2")),

  expect("2",
    killed(async_unkillable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          error(new Error("1"));
        }, "1");
      }, 0);
    }), "2")),


  expect("2",
    killed(ignore_kill(wrap("1")), "2")),

  expect("1",
    fastest([
      forever(ignore_kill(then(ignore_kill(_yield), ignore_kill(wrap("2"))))),
      then(delay(100), wrap("1"))
    ])),

  expect("1",
    fastest([
      then(then(_yield, _yield), wrap("1")),
      then(delay(100), wrap("2"))
    ])),

  expect("1",
    fastest([
      then(async_unkillable((success, error) => {
        success("2");
      }), never),
      wrap("1")
    ])),

  expect("1",
    fastest([
      forever(delay(10)),
      then(delay(100), wrap("1"))
    ])),

  expect("1",
    fastest([
      forever(ignore_kill(delay(10))),
      then(delay(100), wrap("1"))
    ])),

  expect("1",
    fastest([
      forever(_yield),
      then(delay(100), wrap("1"))
    ])),

  expect("1",
    fastest([
      forever(ignore_yield),
      then(delay(100), wrap("1"))
    ])),

  expect("1",
    fastest([
      fastest([
        then(ignore_yield, wrap("1")),
        then(ignore_yield, wrap("2"))
      ]),
      then(ignore_yield, wrap("3"))
    ])),

  expect("1",
    fastest([
      fastest([
        then(delay(10), wrap("1")),
        then(delay(10), wrap("2"))
      ]),
      then(delay(100), wrap("3"))
    ])),

  expect("3",
    fastest([
      fastest([
        then(delay(100), wrap("1")),
        then(delay(100), wrap("2"))
      ]),
      then(delay(10), wrap("3"))
    ])),

  expect("1",
    fastest([
      async_unkillable((success, error) => {
        success("1");
      }),
      wrap("3")
    ])),

  expect("3",
    fastest([
      wrap("3"),
      async_unkillable((success, error) => {
        success("1");
      })
    ])),

  expect("3",
    fastest([
      wrap("3"),
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      })
    ])),

  expect("3",
    fastest([
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      }),
      wrap("3")
    ])),

  expect_crash("Hi1", () =>
    concurrent([
      wrap("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      wrap("4")
    ])),

  expect("3",
    fastest([
      wrap("3"),
      throw_error(new Error("Hi"))
    ])),

  expect("3",
    fastest([
      wrap("3"),
      async_killable((success, error) => {
        success("1");
        return () => {};
      })
    ])),

  expect("3",
    fastest([
      fastest([
        wrap("3"),
        async_killable((success, error) => {
          success("1");
          return () => {};
        })
      ]),
      wrap("4")
    ])),

  expect("1",
    fastest([
      async_killable((success, error) => {
        success("1");
        return () => {
          crash(new Error("2"));
        };
      }),
      wrap("3")
    ])),

  expect(5,
    fastest([_yield, wrap(5)])),

  expect(5,
    fastest([wrap(5), _yield])),

  expect(1,
    counter((increment) =>
      fastest([increment, increment]))),

  expect(2,
    counter((increment) =>
      concurrent([
        increment,
        increment
      ]))),

  expect_crash("Hi", () =>
    counter((increment) =>
      concurrent([
        throw_error(new Error("Hi")),
        increment,
        increment
      ]))),

  expect(1,
    counter((increment) =>
      fastest([
        increment,
        increment
      ]))),

  expect(1,
    counter((increment) =>
      then(fastest([
        repeat(after(ignore_yield, (_) => increment), 2),
        _yield
      ]), _yield)))
]);

perform(tests);
