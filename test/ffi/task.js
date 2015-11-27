import { assert_crash, format_error, format_pretty, equal } from "../assert";
import { map } from "../../util/array";
import { crash } from "../../util/node";
import { sync, transform, flatten, perform, wrap, sequential,
         concurrent, delay, fastest, _yield, throw_error,
         ignore_kill, on_error, async_killable,
         async_unkillable, log, killed, never } from "../../builtin/ffi/task";


const after = (a, f) =>
  flatten(transform(a, f));

const then = (a, b) =>
  after(a, (_) => b);

const forever = (a) =>
  after(a, (_) =>
    forever(a));

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


const tests = (a) =>
  then(concurrent(map(a, (f, index) => {
                    const name = "test " + (index + 1);
                    return fastest([
                      f(name),
                      then(delay(10000),
                           throw_error(new Error(name + " took too long")))
                    ]);
                  })),
       log("All tests succeeded"));

const test = (expected, task) =>
  (name) =>
    after(task, (value) =>
      (equal(value, expected)
        ? wrap(null)
        : throw_error(new Error(format_pretty(name, value, expected)))));

const test_crash = (expected, task) =>
  (name) =>
    after(on_error(task, (_) => null, (e) => e), (e) =>
      (e === null
        ? throw_error(new Error(format_error(name, null, expected)))
        : (e["message"] === expected
            ? wrap(null)
            : throw_error(new Error(format_error(name, e["message"], expected))))));



assert_crash(() => {
  perform(async_killable((success, error) => {
    success("1");
    success("2");
  }));
}, "Invalid success");

/*assert_crash(() => {
  perform(async_killable((success, error) => {
    error(new Error("1"));
    error(new Error("2"));
  }));
}, "Invalid error");*/

assert_crash(() => {
  perform(async_killable((success, error) => {
    success("1");
    error(new Error("2"));
  }));
}, "Invalid error:\nError: 2");

/*assert_crash(() => {
  perform(async_killable((success, error) => {
    error(new Error("1"));
    success("2");
  }));
}, "Invalid success");*/

assert_crash(() => {
  perform(killed(async_killable((success, error) => {
    return () => {
      success("1");
    };
  }), "2"));
}, "Invalid success");

assert_crash(() => {
  perform(killed(async_killable((success, error) => {
    return () => {
      error(new Error("1"));
    };
  }), "2"));
}, "Invalid error:\nError: 1");


/*console.log("START");
perform(killed(async_killable((success, error) => {
  setTimeout(() => {
    console.log("Success");
    success("1");
  }, 0);
  return () => {
    console.log("Killed");
  };
}), "2"));
console.log("END");*/


assert_crash(() => {
  perform(async_unkillable((success, error) => {
    success("1");
    success("2");
  }));
}, "Invalid success");

/*assert_crash(() => {
  perform(async_unkillable((success, error) => {
    error(new Error("1"));
    error(new Error("2"));
  }));
}, "Invalid error");*/

assert_crash(() => {
  perform(async_unkillable((success, error) => {
    success("1");
    error(new Error("2"));
  }));
}, "Invalid error:\nError: 2");

/*assert_crash(() => {
  perform(async_unkillable((success, error) => {
    error(new Error("1"));
    success("2");
  }));
}, "Invalid success");*/

assert_crash(() => {
  perform(killed(async_unkillable((success, error) => {
    success("1");
    success("2");
  }), "3"));
}, "Invalid success");

assert_crash(() => {
  perform(killed(async_unkillable((success, error) => {
    error(new Error("1"));
  }), "2"));
}, "1");


/*console.log("START");
perform(killed(async_unkillable((success, error) => {
  setTimeout(() => {
    error(new Error("1"));
  }, 0);
}), "2"));
console.log("END");*/


/*perform(fastest([
  forever(then(_yield, log("Hi"))),
  delay(1000)
]));


assert_crash(() => {
  concurrent([]);
}, "u");

assert_crash(() => {
  fastest([]);
}, "u");*/


/*perform(tests([
  test("1",
    fastest([
      then(async_unkillable((success, error) => {
        success("2");
      }), never),
      wrap("1")
    ])),

  test("1",
    killed(async_unkillable((success, error) => {
      setTimeout(() => {
        success("2");
      }, 0);
    }), "1")),

  test("1",
    fastest([
      forever(delay(0)),
      then(delay(1000), wrap("1"))
    ])),

  test("1",
    fastest([
      forever(ignore_kill(delay(0))),
      then(delay(1000), wrap("1"))
    ])),

  test("1",
    fastest([
      forever(_yield),
      then(delay(1000), wrap("1"))
    ])),

  test("1",
    fastest([
      forever(ignore_yield),
      then(delay(1000), wrap("1"))
    ])),

  test("1",
    fastest([
      fastest([
        then(ignore_yield, wrap("1")),
        then(ignore_yield, wrap("2"))
      ]),
      then(ignore_yield, wrap("3"))
    ])),

  test("1",
    fastest([
      fastest([
        then(delay(0), wrap("1")),
        then(delay(0), wrap("2"))
      ]),
      then(delay(1000), wrap("3"))
    ])),

  test("3",
    fastest([
      fastest([
        then(delay(1000), wrap("1")),
        then(delay(1000), wrap("2"))
      ]),
      then(delay(0), wrap("3"))
    ])),

  test("2",
    killed(async_unkillable((success, error) => {
      success("1");
    }), "2")),

  test("1",
    fastest([
      async_unkillable((success, error) => {
        success("1");
      }),
      wrap("3")
    ])),

  test("3",
    fastest([
      wrap("3"),
      async_unkillable((success, error) => {
        success("1");
      })
    ])),

  test("3",
    fastest([
      wrap("3"),
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      })
    ])),

  test("3",
    fastest([
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      }),
      wrap("3")
    ])),

  test_crash("Hi1",
    concurrent([
      wrap("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      wrap("4")
    ])),

  test("3",
    fastest([
      wrap("3"),
      throw_error(new Error("Hi"))
    ])),

  test("3",
    fastest([
      wrap("3"),
      async_killable((success, error) => {
        success("1");
        return () => {};
      })
    ])),

  test("3",
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

  test("1",
    fastest([
      async_killable((success, error) => {
        success("1");
        return () => {
          crash(new Error("2"));
        };
      }),
      wrap("3")
    ])),

  test(5,
    fastest([_yield, wrap(5)])),

  test(5,
    fastest([wrap(5), _yield])),

  test(1,
    counter((increment) =>
      fastest([increment, increment]))),

  test(2,
    counter((increment) =>
      concurrent([
        increment,
        increment
      ]))),

  test_crash("Hi",
    counter((increment) =>
      concurrent([
        throw_error(new Error("Hi")),
        increment,
        increment
      ]))),

  test(1,
    counter((increment) =>
      fastest([
        increment,
        increment
      ]))),

  test(1,
    counter((increment) =>
      then(fastest([
        repeat(after(ignore_yield, (_) => increment), 2),
        _yield
      ]), _yield)))
]));
*/
