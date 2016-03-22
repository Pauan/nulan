import { crash } from "../util/error";
import { noop } from "./util";
import * as $array from "../util/array";


const make_frame = (state, on_success, on_error, on_kill) => {
  return {
    a: state,
    b: on_success,
    c: on_error,
    d: on_kill
  };
};

export const make_thread = () => {
  return {
    a: false, // is_killed
    // TODO use a faster stack implementation ?
    b: []     // frames
  };
};

const get_frame = (thread) => {
  // TODO is this correct ?
  if (thread.b["length"] === 0) {
    // TODO what about type deoptimizations ?
    return null;

  } else {
    return thread.b["pop"]();
  }
};

export const kill_thread = (thread) => {
  if (thread.a) {
    return crash(new Error("invalid kill: thread is already killed"));

  } else {
    const frame = get_frame(thread);

    thread.a = true;
    // TODO is this correct ?
    // TODO what about type deoptimizations ?
    thread.b = null;

    if (frame !== null) {
      return frame.d(thread, frame.a);
    }
  }
};

export const kill_threads = (a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    kill_thread(a[i]);
  }
};

export const push_frame = (thread, state, on_success, on_error, on_kill) =>
  thread.b["push"](make_frame(state, on_success, on_error, on_kill));

export const run_in_thread = (thread, task) => {
  if (thread.a) {
    return crash(new Error("cannot run: thread is killed"));

  } else {
    return task.a(task, thread);
  }
};

export const success = (thread, value) => {
  // TODO is this correct ?
  if (thread.b["length"] !== 0) {
    const frame = thread.b["pop"]();

    return frame.b(thread, frame.a, value);
  }
};

export const error = (thread, value) => {
  // TODO is this correct ?
  if (thread.b["length"] === 0) {
    return crash(value);

  } else {
    const frame = thread.b["pop"]();

    return frame.c(thread, frame.a, value);
  }
};



/*export const run_in_thread = (task, thread, on_success, on_error) => {
  if (thread.a) {
    return crash(new Error("cannot run: thread is killed"));

  } else {
    // TODO is this a good idea ?
    // TODO declosureize these
    thread.b = (thread, value) => on_success(value);
    thread.c = (thread, value) => on_error(value);
    return _run(task, thread);
  }
};*/

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

const run_in_thread_pool_success = (thread, pool, value) =>
  $array.remove(pool.b, thread);

// TODO is this a good idea ?
const run_in_thread_pool_error = (thread, pool, value) => {
  kill_thread_pool(pool);
  return pool.c(value);
};

// TODO is this correct ?
const run_in_thread_pool_kill = (thread, state) =>
  $array.remove(state.b, thread);

export const run_in_thread_pool = (pool, task) => {
  if (pool.a) {
    return crash(new Error("cannot run: thread pool is killed"));

  } else {
    const thread = make_thread();

    pool.b["push"](thread);

    push_frame(
      thread,
      pool,
      run_in_thread_pool_success,
      run_in_thread_pool_error,
      run_in_thread_pool_kill
    );

    return run_in_thread(thread, task);
  }
};
