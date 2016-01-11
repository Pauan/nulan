import { expect, expect_crash, assert_crash } from "../assert";
import { crash } from "../../util/error";
import { sync, transform, flatten, wrap, concurrent, concurrent_null,
         delay, fastest, _yield, throw_error, ignore_kill, async_killable,
         async_unkillable, never, make_thread, kill_thread,
         catch_error, on_error, perform } from "../../ffi/task";
import { _null } from "../../ffi/types";


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

const test_crash = (task, value) =>
  sync(() => {
    assert_crash(() => {
      perform(after(fastest(task, never), (_) => {
        crash(new Error("CRASHING!"));
      }));
    }, "CRASHING!");

    return value;
  });


/*perform(fastest(
  ignore_kill(forever(then(_yield, log("Hi")))),
  delay(1000)
));*/


export default [
  expect_crash("Expected positive number but got -5",
    catch_error(() => delay(-5))),

  expect_crash("Cannot delay for 0 milliseconds (maybe use yield instead?)",
    catch_error(() => delay(0))),


  expect(_null,
    concurrent_null([])),

  expect(_null,
    concurrent_null([
      wrap("3"),
      wrap("4")
    ])),

  expect_crash("Hi1",
    concurrent_null([
      wrap("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      wrap("4")
    ])),

  expect(2,
    counter((increment) =>
      concurrent_null([
        increment,
        increment
      ]))),

  expect_crash("Hi",
    counter((increment) =>
      concurrent_null([
        throw_error(new Error("Hi")),
        increment,
        increment
      ]))),


  expect(2,
    on_error(throw_error(new Error("Hi")), (_) => 1, (_) => 2)),

  expect(2,
    on_error(fastest(throw_error(new Error("Hi")), wrap(3)), (_) => 1, (_) => 2)),

  expect(2,
    fastest(
      on_error(throw_error(new Error("Hi")), (_) => 1, (_) => 2),
      wrap(3)
    )),

  expect(3,
    fastest(
      on_error(delay(100), (_) => 1, (_) => 2),
      wrap(3)
    )),


  expect(2,
    test_crash(catch_error(() => 1), 2)),

  expect_crash("3",
    then(catch_error(() => 1),
         throw_error(new Error("3")))),

  expect_crash("3",
    then(on_error(then(catch_error(() => 1),
                       throw_error(new Error("2"))),
                  (x) => x,
                  (_) => null),
         throw_error(new Error("3")))),


  expect([],
    concurrent([])),

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

  expect_crash("1",
    async_killable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1",
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

  expect_crash("1",
    async_unkillable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "Invalid error");
    })),

  expect_crash("1",
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


  expect("3",
    killed(ignore_kill(async_killable((success, error) => {
      success("1");
      return () => {
        crash(new Error("2"));
      };
    })), "3")),

  expect("2",
    killed(ignore_kill(async_killable((success, error) => {
      return () => {
        crash(new Error("1"));
      };
    })), "2")),

  expect("2",
    killed(ignore_kill(wrap("1")), "2")),

  expect("1",
    fastest(
      forever(ignore_kill(then(_yield, wrap("2")))),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_kill(then(ignore_kill(_yield), wrap("2")))),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      then(then(_yield, _yield), wrap("1")),
      then(delay(100), wrap("2"))
    )),

  expect("1",
    fastest(
      then(async_unkillable((success, error) => {
        success("2");
      }), never),
      wrap("1")
    )),

  expect("1",
    fastest(
      forever(delay(10)),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_kill(delay(10))),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      forever(_yield),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_yield),
      then(delay(100), wrap("1"))
    )),

  expect("1",
    fastest(
      fastest(
        then(ignore_yield, wrap("1")),
        then(ignore_yield, wrap("2"))
      ),
      then(ignore_yield, wrap("3"))
    )),

  expect("1",
    fastest(
      fastest(
        then(delay(10), wrap("1")),
        then(delay(10), wrap("2"))
      ),
      then(delay(100), wrap("3"))
    )),

  expect("3",
    fastest(
      fastest(
        then(delay(100), wrap("1")),
        then(delay(100), wrap("2"))
      ),
      then(delay(10), wrap("3"))
    )),

  expect("1",
    fastest(
      async_unkillable((success, error) => {
        success("1");
      }),
      wrap("3")
    )),

  expect("3",
    fastest(
      wrap("3"),
      async_unkillable((success, error) => {
        success("1");
      })
    )),

  expect("3",
    fastest(
      wrap("3"),
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      })
    )),

  expect("3",
    fastest(
      async_unkillable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
      }),
      wrap("3")
    )),

  expect_crash("Hi1",
    concurrent([
      wrap("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      wrap("4")
    ])),

  expect("3",
    fastest(
      wrap("3"),
      throw_error(new Error("Hi"))
    )),

  expect("3",
    fastest(
      wrap("3"),
      async_killable((success, error) => {
        success("1");
        return () => {};
      })
    )),

  expect("3",
    fastest(
      fastest(
        wrap("3"),
        async_killable((success, error) => {
          success("1");
          return () => {};
        })
      ),
      wrap("4")
    )),

  expect("1",
    ignore_kill(fastest(
      async_killable((success, error) => {
        success("1");
        return () => {
          crash(new Error("2"));
        };
      }),
      wrap("3")
    ))),

  expect(5,
    fastest(_yield, wrap(5))),

  expect(5,
    fastest(wrap(5), _yield)),

  expect(1,
    counter((increment) =>
      fastest(increment, increment))),

  expect(2,
    counter((increment) =>
      concurrent([
        increment,
        increment
      ]))),

  expect_crash("Hi",
    counter((increment) =>
      concurrent([
        throw_error(new Error("Hi")),
        increment,
        increment
      ]))),

  expect(1,
    counter((increment) =>
      fastest(
        increment,
        increment
      ))),

  expect(1,
    counter((increment) =>
      then(fastest(
        repeat(after(ignore_yield, (_) => increment), 2),
        _yield
      ), _yield))),

  expect("4",
    ignore_kill(fastest(
      then(async_killable((success, error) => {
        setTimeout(() => {
          success("1");
        }, 0);
        return () => {
          crash(new Error("2"));
        };
      }), never),
      then(delay(100), wrap("4"))
    )))
];