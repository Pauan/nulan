import { crash } from "../util/error";
import { _null, none, some } from "./types";
import { noop, try_catch } from "./util";
import * as $array from "../util/array";


const thread_crash = (thread, value) =>
  crash(value);

export const make_thread = () => {
  return {
    a: false,        // is_killed
    b: noop,         // on_success
    c: thread_crash, // on_error
    d: noop,         // on_kill
    e: {}            // state
  };
};

export const kill_thread = (thread) => {
  if (thread.a) {
    return crash(new Error("invalid kill: thread is already killed"));

  } else {
    thread.a = true;
    return thread.d(thread);
  }
};

export const kill_threads = (a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    kill_thread(a[i]);
  }
};

const _run = (task, thread) =>
  task.a(task, thread);

export const run_in_thread = (task, thread, on_success, on_error) => {
  if (thread.a) {
    return crash(new Error("cannot run: thread is killed"));

  } else {
    // TODO is this a good idea ?
    // TODO declosureize these
    thread.b = (thread, value) => on_success(value);
    thread.c = (thread, value) => on_error(value);
    return _run(task, thread);
  }
};

export const make_thread_pool = (on_error) => {
  return {
    a: false,   // is_killed
    b: [],      // threads
    // TODO is this a good idea ?
    c: on_error // on_error
  };
};

export const kill_thread_pool = (pool) => {
  if (pool.a) {
    return crash(new Error("invalid kill: thread pool is already killed"));

  } else {
    pool.a = true;
    return kill_threads(pool.b);
  }
};

const run_in_thread_pool_success = (thread, value) =>
  $array.remove(thread.e.b, thread);

// TODO is this a good idea ?
const run_in_thread_pool_error = (thread, value) => {
  const pool = thread.e;
  kill_thread_pool(pool);
  return pool.c(value);
};

export const run_in_thread_pool = (pool, task) => {
  if (pool.a) {
    return crash(new Error("cannot run: thread pool is killed"));

  } else {
    const thread = make_thread();

    thread.b = run_in_thread_pool_success;
    thread.c = run_in_thread_pool_error;
    thread.e = pool;

    pool.b["push"](thread);

    return _run(task, thread);
  }
};


const success = (thread, value) =>
  thread.b(thread, value);

const error = (thread, value) =>
  thread.c(thread, value);


const run_sync = (task, thread) =>
  // TODO maybe use try/catch ?
  success(thread, task.b());

export const sync = (b) =>
  ({ a: run_sync, b });


const run_catch_error = (task, thread) => {
  const x = try_catch(task.b);

  if (x.$ === 0) {
    return success(thread, x.a);

  } else {
    return error(thread, x.a);
  }
};

// TODO merge this into sync ?
export const catch_error = (b) =>
  ({ a: run_catch_error, b });


const run_ignore_kill_success = (thread, value) => {
  const state = thread.e;

  if (!state.a) {
    const old_thread = state.b;
    old_thread.d = noop;
    // TODO is this correct ?
    // TODO test this
    old_thread.e = state.c;
    return success(old_thread, value);
  }
};

const run_ignore_kill_error = (thread, value) => {
  const state = thread.e;

  // Never silently ignore errors
  if (state.a) {
    return crash(value);

  } else {
    const old_thread = state.b;
    old_thread.d = noop;
    // TODO is this correct ?
    // TODO test this
    old_thread.e = state.c;
    return error(old_thread, value);
  }
};

const run_ignore_kill_kill = (thread) => {
  const state = thread.e;

  state.a = true;
};

const run_ignore_kill = (task, thread) => {
  const state = {
    a: false,   // killed
    b: thread,  // old_thread
    c: thread.e // old_state
  };

  const x = make_thread();

  x.b = run_ignore_kill_success;
  x.c = run_ignore_kill_error;
  x.e = state;

  thread.d = run_ignore_kill_kill;
  thread.e = state;

  return _run(task.b, x);
};

// Guarantees:
// * success is ignored after being killed
// * does nothing when killed
export const ignore_kill = (b) =>
  ({ a: run_ignore_kill, b });


const run_async_killable_success = (thread, state, value) => {
  if (state.a) {
    return crash(new Error("invalid success"));

  } else {
    state.a = true;
    thread.d = noop;
    thread.e = state.b;
    return success(thread, value);
  }
};

const run_async_killable_error = (thread, state, value) => {
  if (state.a) {
    return crash(new Error("invalid error"));

  } else {
    state.a = true;
    thread.d = noop;
    thread.e = state.b;
    return error(thread, value);
  }
};

// TODO is this check needed ?
const run_async_killable_kill = (thread) => {
  const state = thread.e;

  if (state.a) {
    return crash(new Error("invalid kill"));

  } else {
    state.a = true;
    return state.c();
  }
};

const run_async_killable = (task, thread) => {
  const state = {
    a: false,    // done
    b: thread.e, // old_state
    c: noop      // kill
  };

  thread.d = run_async_killable_kill;
  thread.e = state;

  const on_success = (value) => {
    run_async_killable_success(thread, state, value);
  };

  const on_error = (value) => {
    run_async_killable_error(thread, state, value);
  };

  state.c = task.b(on_success, on_error);
};

// Guarantees:
// * success cannot be called after success, error, or kill
// * error cannot be called after success, error, or kill
// * will not be killed after success, error, or kill
export const async_killable = (b) =>
  ({ a: run_async_killable, b });


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
    return run_in_thread(task, x, resolve, reject);
  });

// TODO handle cancellation ?
export const from_Promise = (f) =>
  // TODO should this use a raw Task or async_unkillable ?
  async_unkillable((success, error) =>
    f()["then"](success, error));


export const never = { a: noop };


const run_wrap = (task, thread) =>
  success(thread, task.b);

export const wrap = (b) =>
  ({ a: run_wrap, b });


const run_throw_error = (task, thread) =>
  error(thread, task.b);

export const throw_error = (b) =>
  ({ a: run_throw_error, b });


const run_transform_success = (thread, value) => {
  const state = thread.e;

  thread.b = state.b;
  thread.c = state.c;
  thread.d = noop;
  thread.e = state.d;

  return success(thread, state.a(value));
};

// TODO is this correct ?
// TODO is this needed ?
const run_transform_error = (thread, value) => {
  const state = thread.e;

  thread.b = state.b;
  thread.c = state.c;
  thread.d = noop;
  thread.e = state.d;

  return error(thread, value);
};

const run_transform = (task, thread) => {
  const state = {
    a: task.c,   // map
    b: thread.b, // old_success
    c: thread.c, // old_error
    d: thread.e  // old_state
  };

  thread.b = run_transform_success;
  thread.c = run_transform_error;
  thread.e = state;

  return _run(task.b, thread);
};

export const transform = (b, c) =>
  ({ a: run_transform, b, c });


const run_flatten_success = (thread, value) => {
  const state = thread.e;

  thread.b = state.a;
  thread.c = state.b;
  thread.e = state.c;

  return _run(value, thread);
};

// TODO is this correct ?
// TODO is this needed ?
const run_flatten_error = (thread, value) => {
  const state = thread.e;

  thread.b = state.a;
  thread.c = state.b;
  thread.d = noop;
  thread.e = state.c;

  return error(thread, value);
};

const run_flatten = (task, thread) => {
  const state = {
    a: thread.b, // old_success
    b: thread.c, // old_error
    c: thread.e  // old_state
  };

  thread.b = run_flatten_success;
  thread.c = run_flatten_error;
  thread.e = state;

  return _run(task.b, thread);
};

export const flatten = (b) =>
  ({ a: run_flatten, b });


const run_concurrent_null_success = (thread, value) => {
  const state = thread.e;

  --state.b;

  // TODO what about setting `running` ?
  if (state.b === 0) {
    const old_thread = state.d;
    old_thread.d = noop;
    old_thread.e = state.e;
    return success(old_thread, _null);
  }
};

const run_concurrent_null_error = (thread, value) => {
  const state = thread.e;
  const old_thread = state.d;

  state.a = false;
  old_thread.d = noop;
  old_thread.e = state.e;
  kill_threads(state.c);
  return error(old_thread, value);
};

// TODO what about running ?
const run_concurrent_null_kill = (thread) => {
  const state = thread.e;

  return kill_threads(state.c);
};

const run_concurrent_null = (task, thread) => {
  const a = task.b;

  const length = a["length"];

  if (length === 0) {
    return success(thread, _null);

  } else {
    const state = {
      a: true,    // running
      b: length,  // pending
      c: [],      // threads
      d: thread,  // thread
      e: thread.e // old_state
    };

    thread.d = run_concurrent_null_kill;
    thread.e = state;

    for (let i = 0; i < length; ++i) {
      if (state.a) {
        const thread = make_thread();

        thread.b = run_concurrent_null_success;
        thread.c = run_concurrent_null_error;
        thread.e = state;

        state.c["push"](thread);

        _run(a[i], thread);

      } else {
        // TODO is this correct ?
        break;
      }
    }
  }
};

// TODO code duplication with concurrent
export const concurrent_null = (b) =>
  ({ a: run_concurrent_null, b });


const run_concurrent_success = (thread, value) => {
  const _state = thread.e;
  const index = _state.a;
  const state = _state.b;
  const values = state.f;

  values[index] = value;

  --state.b;

  // TODO what about setting `running` ?
  if (state.b === 0) {
    const old_thread = state.d;
    old_thread.d = noop;
    old_thread.e = state.e;
    return success(old_thread, values);
  }
};

const run_concurrent_error = (thread, value) => {
  const state = thread.e.b;
  const old_thread = state.d;

  state.a = false;
  old_thread.d = noop;
  old_thread.e = state.e;
  kill_threads(state.c);
  return error(old_thread, value);
};

// TODO what about running ?
const run_concurrent_kill = (thread) => {
  const state = thread.e;

  return kill_threads(state.c);
};

const run_concurrent = (task, thread) => {
  const a = task.b;

  const length = a["length"];

  const values = new Array(length);

  if (length === 0) {
    return success(thread, values);

  } else {
    const state = {
      a: true,     // running
      b: length,   // pending
      c: [],       // threads
      d: thread,   // thread
      e: thread.e, // old_state
      f: values    // values
    };

    thread.d = run_concurrent_kill;
    thread.e = state;

    for (let i = 0; i < length; ++i) {
      if (state.a) {
        const thread = make_thread();

        thread.b = run_concurrent_success;
        thread.c = run_concurrent_error;

        thread.e = {
          a: i,    // index
          b: state // state
        };

        state.c["push"](thread);

        _run(a[i], thread);

      } else {
        // TODO is this correct ?
        break;
      }
    }
  }
};

export const concurrent = (b) =>
  ({ a: run_concurrent, b });


const run_transform2_success = (thread, value) => {
  const _state = thread.e;
  const state = _state.b;

  const value2 = state.a;

  if (value2.$ === 0) {
    state.a = { $: 1, a: value };

  } else {
    const old_thread = state.e;

    old_thread.d = noop;
    old_thread.e = state.f;

    const output = (_state.a === 0
                     ? state.b(value, value2.a)
                     : state.b(value2.a, value));

    return success(old_thread, output);
  }
};

const run_transform2_error = (thread, value) => {
  const _state = thread.e;
  const state = _state.b;

  const old_thread = state.e;

  old_thread.d = noop;
  old_thread.e = state.f;

  kill_thread(state.c);
  kill_thread(state.d);

  return error(old_thread, value);
};

const run_transform2_kill = (thread) => {
  const state = thread.e;

  kill_thread(state.c);
  return kill_thread(state.d);
};

const run_transform2 = (task, thread) => {
  const thread1 = make_thread();
  const thread2 = make_thread();

  const state = {
    a: none,    // value
    b: task.d,  // transform
    c: thread1, // thread1
    d: thread2, // thread2
    e: thread,  // old_thread
    f: thread.e // old_state
  };

  thread.d = run_transform2_kill;
  thread.e = state;

  thread1.b = run_transform2_success;
  thread1.c = run_transform2_error;
  thread1.e = {
    a: 0,
    b: state
  };

  thread2.b = run_transform2_success;
  thread2.c = run_transform2_error;
  thread2.e = {
    a: 1,
    b: state
  };

  _run(task.b, thread1);

  // TODO what if task.b errors synchronously ?
  return _run(task.c, thread2);
};

// TODO test this
export const transform2 = (b, c, d) =>
  ({ a: run_transform2, b, c, d });


const run_fastest_success = (thread, value) => {
  const state = thread.e;
  const old_thread = state.d;

  state.a = false;

  old_thread.d = noop;
  old_thread.e = state.e;

  kill_thread(state.b);
  kill_thread(state.c);

  return success(old_thread, value);
};

const run_fastest_error = (thread, value) => {
  const state = thread.e;
  const old_thread = state.d;

  state.a = false;

  old_thread.d = noop;
  old_thread.e = state.e;

  kill_thread(state.b);
  kill_thread(state.c);

  return error(old_thread, value);
};

const run_fastest_kill = (thread) => {
  const state = thread.e;

  kill_thread(state.b);
  return kill_thread(state.c);
};

const run_fastest = (task, thread) => {
  const thread1 = make_thread();
  const thread2 = make_thread();

  const state = {
    a: true,    // running
    b: thread1, // thread1
    c: thread2, // thread2
    d: thread,  // old_thread
    e: thread.e // old_state
  };

  thread.d = run_fastest_kill;
  thread.e = state;

  thread1.b = run_fastest_success;
  thread1.c = run_fastest_error;
  thread1.e = state;

  thread2.b = run_fastest_success;
  thread2.c = run_fastest_error;
  thread2.e = state;

  _run(task.b, thread1);

  if (state.a) {
    return _run(task.c, thread2);
  }
};

export const fastest = (b, c) =>
  ({ a: run_fastest, b, c });


const run_on_error_success = (thread, value) => {
  const state = thread.e;

  thread.b = state.c;
  thread.c = state.d;
  thread.d = noop;
  thread.e = state.e;

  return success(thread, state.a(value));
};

const run_on_error_error = (thread, value) => {
  const state = thread.e;

  thread.b = state.c;
  thread.c = state.d;
  thread.d = noop;
  thread.e = state.e;

  return success(thread, state.b(value));
};

const run_on_error = (task, thread) => {
  const state = {
    a: task.c,   // on_success
    b: task.d,   // on_error
    c: thread.b, // old_success
    d: thread.c, // old_error
    e: thread.e  // old_state
  };

  thread.b = run_on_error_success;
  thread.c = run_on_error_error;
  // TODO handle kill ?
  thread.e = state;

  return _run(task.b, thread);
};

// TODO maybe handle kill ?
export const on_error = (b, c, d) =>
  ({ a: run_on_error, b, c, d });


const run_with_resource_destroy_success = (thread, value) => {
  const state = thread.e;
  const result = state.c;

  // TODO check for 0 ?
  if (result.$ === 1) {
    if (!state.a) {
      const old_thread = state.h;
      old_thread.d = noop;
      old_thread.e = state.i;
      return success(old_thread, result.a);
    }

  } else {
    // TODO is this correct ?
    return run_with_resource_error(thread, result.a);
  }
};

const run_with_resource_use = (thread, type, value) => {
  const state = thread.e;
  const resource = state.b;
  const thread_create = state.f;

  state.b = none;
  state.c = { $: type, a: value };

  thread_create.b = run_with_resource_destroy_success;

  // TODO assert that the resource exists ?
  return _run(state.e(resource.a), thread_create);
};

const run_with_resource_use_success = (thread, value) =>
  run_with_resource_use(thread, 1, value);

const run_with_resource_use_error = (thread, value) =>
  run_with_resource_use(thread, 2, value);

const run_with_resource_success = (thread, value) => {
  const state = thread.e;

  if (state.a) {
    const thread_create = state.f;

    thread_create.b = noop;
    thread_create.c = thread_crash;

    return _run(state.e(value), thread_create);

  } else {
    state.b = some(value);

    return _run(state.d(value), state.g);
  }
};

// TODO code duplication with ignore_kill
const run_with_resource_error = (thread, value) => {
  const state = thread.e;

  if (state.a) {
    return crash(value);

  } else {
    const old_thread = state.h;
    old_thread.d = noop;
    old_thread.e = state.i;
    return error(old_thread, value);
  }
};

const run_with_resource_kill = (thread) => {
  const state = thread.e;
  const resource = state.b;

  state.a = true;

  if (resource.$ === 1) {
    kill_thread(state.g);

    const thread_create = state.f;

    thread_create.b = noop;
    thread_create.c = thread_crash;

    return _run(state.e(resource.a), thread_create);
  }
};

const run_with_resource = (task, thread) => {
  const thread_create = make_thread();
  const thread_use    = make_thread();

  const state = {
    a: false,         // killed
    b: none,          // resource
    c: { $: 0 },      // result
    d: task.c,        // use
    e: task.d,        // destroy
    f: thread_create, // thread_create
    g: thread_use,    // thread_use
    h: thread,        // old_thread
    i: thread.e       // old_state
  };

  thread.d = run_with_resource_kill;
  thread.e = state;

  thread_create.b = run_with_resource_success;
  thread_create.c = run_with_resource_error;
  thread_create.e = state;

  thread_use.b = run_with_resource_use_success;
  thread_use.c = run_with_resource_use_error;
  thread_use.e = state;

  return _run(task.b, thread_create);
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
export const with_resource = (b, c, d) =>
  ({ a: run_with_resource, b, c, d });


const _yield_queue = [];

const _yield_queue_run = () => {
  const pending = _yield_queue["length"];

  for (let i = 0; i < pending; ++i) {
    // TODO faster implementation for this ?
    _yield_queue["shift"]()(_null);
  }

  if (_yield_queue["length"] !== 0) {
    return setTimeout(_yield_queue_run, 0);
  }
};

const _yield_queue_add = (x) => {
  _yield_queue["push"](x);

  if (_yield_queue["length"] === 1) {
    return setTimeout(_yield_queue_run, 0);
  }
};

// TODO guarantee that this cannot happen while calling _yield_queue_run ?
const _yield_queue_remove = (x) =>
  $array.remove(_yield_queue, x);

// TODO is this correct ?
export const _yield =
  async_killable((success, error) => {
    _yield_queue_add(success);

    return () =>
      _yield_queue_remove(success);
  });


export const wait = (ms) => {
  if (ms === 0) {
    return crash(new Error("cannot wait for 0 milliseconds (maybe use yield instead?)"));
  }

  if (ms < 0) {
    return crash(new Error("expected positive number but got " + ms));
  }

  return async_killable((success, error) => {
    const x = setTimeout(() => success(_null), ms);
    return () => clearTimeout(x);
  });
};

export const log = (s) =>
  sync(() => {
    console["log"](s);
    return _null;
  });


export const make_thread_run = (task) => {
  const x = make_thread();
  _run(task, x);
  return x;
};
