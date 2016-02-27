import { expect, expect_crash, assert_crash } from "../assert";
import { crash } from "../../util/error";
import { get_message } from "../../util/node";
import { transform, chain, reply, concurrent, concurrent_null,
         wait, fastest, _yield, throw_error, ignore_kill, async_killable,
         async_unkillable, never, kill_thread,
         make_thread_run, with_resource, catch_error } from "../../ffi/task";
import * as $blocking from "../../ffi/blocking-task";
import * as $mutable from "../../ffi/mutable";
import { _null } from "../../ffi/types";


const then = (a, b) =>
  chain(a, (_) => b);

const forever = (a) =>
  chain(a, (_) =>
    forever(a));

const sync = (a) =>
  async_unkillable((success, error) => {
    success(a());
  });

const killed = (a, value) =>
  chain(sync(() => {
          kill_thread(make_thread_run(a));
        }), (_) => value);

const ignore_yield = ignore_kill(_yield);

// TODO handle 0
const repeat = (x, i) => {
  if (i === 1) {
    return x;
  } else {
    return chain(x, (_) => repeat(x, i - 1));
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

const queue = (f) =>
  chain(sync(() => []), (queue) =>
    transform(catch_error(() =>
                f((value) =>
                  sync(() => {
                    queue["push"](value);
                    return _null;
                  }))), (result) => {
      if (result.$ === 0) {
        return {
          queue: queue,
          value: result.a,
          error: null
        };

      } else {
        return {
          queue: queue,
          value: null,
          error: get_message(result.a)
        };
      }
    }));

const test_crash = (task, value) =>
  sync(() => {
    assert_crash(() => {
      make_thread_run(chain(fastest(task, never), (_) => {
        crash(new Error("CRASHING!"));
      }));
    }, "CRASHING!");

    return value;
  });


/*make_thread_run(fastest(
  ignore_kill(forever(then(_yield, log("Hi")))),
  wait(1000)
));*/


// TODO move this into test/ffi/blocking-task.js ?
const loop = (mut, max) =>
  $blocking.chain($mutable.get(mut), (i) => {
    if (i < max) {
      return $blocking.chain($mutable.set(mut, i + 1), (_) =>
                             loop(mut, max));
    } else {
      return $blocking.reply(i);
    }
  });


export default [
  // TODO move this into test/ffi/blocking-task.js ?
  expect(1000000,
    $blocking.task_from($blocking.chain($mutable.mutable(0), (mut) =>
                                        loop(mut, 1000000)))),

  expect_crash("expected positive number but got -5", () =>
    wait(-5)),

  expect_crash("cannot wait for 0 milliseconds (maybe use yield instead?)", () =>
    wait(0)),


  expect(_null,
    concurrent_null([])),

  expect(_null,
    concurrent_null([
      reply("3"),
      reply("4")
    ])),

  expect_crash("Hi1", () =>
    concurrent_null([
      reply("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      reply("4")
    ])),

  expect(2,
    counter((increment) =>
      concurrent_null([
        increment,
        increment
      ]))),

  expect_crash("Hi", () =>
    counter((increment) =>
      concurrent_null([
        throw_error(new Error("Hi")),
        increment,
        increment
      ]))),


  expect([],
    concurrent([])),

  expect("1",
    async_killable((success, error) => {
      success("1");

      assert_crash(() => {
        success("2");
      }, "invalid success");
    })),

  expect("1",
    async_killable((success, error) => {
      success("1");

      assert_crash(() => {
        error(new Error("2"));
      }, "invalid error");
    })),

  expect_crash("1", () =>
    async_killable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "invalid error");
    })),

  expect_crash("1", () =>
    async_killable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        success("2");
      }, "invalid success");
    })),

  expect("2",
    killed(async_killable((success, error) => {
      return () => {
        assert_crash(() => {
          success("1");
        }, "invalid success");
      };
    }), reply("2"))),

  expect("2",
    killed(async_killable((success, error) => {
      return () => {
        assert_crash(() => {
          error(new Error("1"));
        }, "invalid error");
      };
    }), reply("2"))),

  expect("2",
    killed(async_killable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          success("1");
        }, "invalid success");
      }, 0);
      return () => {};
    }), reply("2"))),

  expect("2",
    killed(async_killable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          error(new Error("1"));
        }, "invalid error");
      }, 0);
      return () => {};
    }), reply("2"))),


  expect("1",
    async_unkillable((success, error) => {
      success("1");

      assert_crash(() => {
        success("2");
      }, "invalid success");
    })),

  expect("1",
    async_unkillable((success, error) => {
      success("1");

      assert_crash(() => {
        error(new Error("2"));
      }, "invalid error");
    })),

  expect_crash("1", () =>
    async_unkillable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        error(new Error("2"));
      }, "invalid error");
    })),

  expect_crash("1", () =>
    async_unkillable((success, error) => {
      error(new Error("1"));

      assert_crash(() => {
        success("2");
      }, "invalid success");
    })),

  expect("2",
    killed(async_unkillable((success, error) => {
      success("1");
    }), reply("2"))),

  expect("2",
    killed(async_unkillable((success, error) => {
      assert_crash(() => {
        error(new Error("1"));
      }, "1");
    }), reply("2"))),

  expect("2",
    killed(async_unkillable((success, error) => {
      setTimeout(() => {
        success("1");
      }, 0);
    }), reply("2"))),

  expect("2",
    killed(async_unkillable((success, error) => {
      setTimeout(() => {
        assert_crash(() => {
          error(new Error("1"));
        }, "1");
      }, 0);
    }), reply("2"))),


  expect("3",
    killed(ignore_kill(async_killable((success, error) => {
      success("1");
      return () => {
        crash(new Error("2"));
      };
    })), reply("3"))),

  expect("2",
    killed(ignore_kill(async_killable((success, error) => {
      return () => {
        crash(new Error("1"));
      };
    })), reply("2"))),

  expect("2",
    killed(ignore_kill(reply("1")), reply("2"))),

  expect("1",
    fastest(
      forever(ignore_kill(then(_yield, reply("2")))),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_kill(then(ignore_kill(_yield), reply("2")))),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      then(then(_yield, _yield), reply("1")),
      then(wait(100), reply("2"))
    )),

  expect("1",
    fastest(
      then(async_unkillable((success, error) => {
        success("2");
      }), never),
      reply("1")
    )),

  expect("1",
    fastest(
      forever(wait(10)),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_kill(wait(10))),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      forever(_yield),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      forever(ignore_yield),
      then(wait(100), reply("1"))
    )),

  expect("1",
    fastest(
      fastest(
        then(ignore_yield, reply("1")),
        then(ignore_yield, reply("2"))
      ),
      then(ignore_yield, reply("3"))
    )),

  expect("1",
    fastest(
      fastest(
        then(wait(10), reply("1")),
        then(wait(10), reply("2"))
      ),
      then(wait(100), reply("3"))
    )),

  expect("3",
    fastest(
      fastest(
        then(wait(100), reply("1")),
        then(wait(100), reply("2"))
      ),
      then(wait(10), reply("3"))
    )),

  expect("1",
    fastest(
      async_unkillable((success, error) => {
        success("1");
      }),
      reply("3")
    )),

  expect("1",
    fastest(
      async_unkillable((success, error) => {
        success("1");
      }),
      async_unkillable((success, error) => {
        success("3");
      })
    )),

  expect("3",
    fastest(
      reply("3"),
      async_unkillable((success, error) => {
        success("1");
      })
    )),

  expect("3",
    fastest(
      reply("3"),
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
      reply("3")
    )),

  expect_crash("Hi1", () =>
    concurrent([
      reply("3"),
      throw_error(new Error("Hi1")),
      throw_error(new Error("Hi2")),
      reply("4")
    ])),

  expect("3",
    fastest(
      reply("3"),
      throw_error(new Error("Hi"))
    )),

  expect("3",
    fastest(
      reply("3"),
      async_killable((success, error) => {
        success("1");
        return () => {};
      })
    )),

  expect("3",
    fastest(
      fastest(
        reply("3"),
        async_killable((success, error) => {
          success("1");
          return () => {};
        })
      ),
      reply("4")
    )),

  expect("1",
    ignore_kill(fastest(
      async_killable((success, error) => {
        success("1");
        return () => {
          crash(new Error("2"));
        };
      }),
      reply("3")
    ))),

  expect("1",
    ignore_kill(fastest(
      async_killable((success, error) => {
        success("1");
        return () => {
          crash(new Error("2"));
        };
      }),
      async_unkillable((success, error) => {
        success("3");
      })
    ))),

  expect(5,
    fastest(_yield, reply(5))),

  expect(5,
    fastest(reply(5), _yield)),

  expect(1,
    counter((increment) =>
      fastest(increment, increment))),

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
      fastest(
        increment,
        increment
      ))),

  expect(1,
    counter((increment) =>
      then(fastest(
        repeat(chain(ignore_yield, (_) => increment), 2),
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
      then(wait(100), reply("4"))
    ))),


  // TODO Kill create | Error destroy -> Crash destroy
  // TODO Success create | Kill use | Error destroy -> Crash destroy
  // TODO Success create | Error use | Kill destroy -> Crash use

  // Success create | Success use | Success destroy -> Success use
  expect({
    queue: ["create", ["use", 1], ["destroy", 1]],
    value: 6,
    error: null
  }, queue((push) =>
       with_resource(then(push("create"),
                          reply(1)),
         (id) => then(push(["use", id]),
                      reply(id + 5)),
         (id) => then(push(["destroy", id]),
                      reply(id + 6))))),

  // Error create -> Error create
  expect({
    queue: ["create"],
    value: null,
    error: "Hi"
  }, queue((push) =>
       with_resource(then(push("create"),
                          throw_error(new Error("Hi"))),
         (id) => push(["use", id]),
         (id) => push(["destroy", id])))),

  // Kill create | Success destroy -> Nothing
  expect({
    queue: ["killed", "create", ["destroy", 1]],
    value: 50,
    error: null
  }, queue((push) =>
       killed(with_resource(then(then(wait(10), push("create")), reply(1)),
                (id) => push(["use", id]),
                (id) => push(["destroy", id])),
              then(push("killed"),
                   then(wait(100), reply(50)))))),

  // Success create | Error use | Success destroy -> Error use
  expect({
    queue: ["create", ["use", 1], ["destroy", 1]],
    value: null,
    error: "Hi"
  }, queue((push) =>
       with_resource(then(push("create"),
                          reply(1)),
         (id) => then(push(["use", id]),
                      throw_error(new Error("Hi"))),
         (id) => then(push(["destroy", id]),
                      reply(id + 6))))),

  // Success create | Success use | Error destroy -> Error destroy
  expect({
    queue: ["create", ["use", 1], ["destroy", 1]],
    value: null,
    error: "Hi2"
  }, queue((push) =>
       with_resource(then(push("create"),
                          reply(1)),
         (id) => then(push(["use", id]),
                      reply(id + 5)),
         (id) => then(push(["destroy", id]),
                      throw_error(new Error("Hi2")))))),

  // Success create | Error use | Error destroy -> Error destroy
  expect({
    queue: ["create", ["use", 1], ["destroy", 1]],
    value: null,
    error: "Hi2"
  }, queue((push) =>
       with_resource(then(push("create"),
                          reply(1)),
         (id) => then(push(["use", id]),
                      throw_error(new Error("Hi1"))),
         (id) => then(push(["destroy", id]),
                      throw_error(new Error("Hi2")))))),

  // Success create | Kill use | Success destroy -> Nothing
  expect({
    queue: ["create", ["use", 1], ["destroy", 1], "killed"],
    value: 50,
    error: null
  }, queue((push) =>
       killed(with_resource(then(push("create"),
                                 reply(1)),
                (id) => then(push(["use", id]),
                             then(wait(10),
                                  throw_error(new Error("Fail")))),
                (id) => then(push(["destroy", id]),
                             reply(id + 6))),
              then(push("killed"),
                   reply(50))))),

  // Success create | Success use | Kill destroy -> Nothing
  expect({
    queue: ["create", ["use", 1], ["destroy", 1], "killed"],
    value: 50,
    error: null
  }, queue((push) =>
       killed(with_resource(then(push("create"),
                                 reply(1)),
                (id) => then(push(["use", id]),
                             reply(id + 5)),
                (id) => then(push(["destroy", id]),
                             never)),
              then(push("killed"),
                   reply(50))))),
];
