import { crash, error_from } from "./crash";
import { noop } from "./util";
import { unsafe_remove_element } from "./unsafe/list";


export const make_thread = (on_success, on_error) => {
  return {
    a: false,      // is_killed
    b: false,      // task_running
    c: noop,       // on_thread_kill
    d: on_success, // on_success
    e: on_error    // on_error
  };
};


export const run_in_new_thread = (task, on_success, on_error) => {
  const thread = make_thread(on_success, on_error);
  // TODO code duplication
  thread.b = true;
  return task(thread);
};


export export const on_thread_kill = (thread, f) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid on_thread_kill: thread is killed"));

  // TODO remove this check later ?
  } else if (thread.b) {
    return crash(error_from("invalid on_thread_kill: thread is already running"));

  // TODO maybe get rid of this ?
  } else if (f === noop) {
    return crash(error_from("invalid on_thread_kill: cannot use noop"));

  // TODO remove this check later ?
  } else if (thread.c === noop) {
    thread.c = f;

  } else {
    return crash(error_from("invalid on_thread_kill: thread has a pending on_thread_kill"));
  }
};


export const thread_success = (thread, value) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid thread_success: thread is killed"));

  // TODO remove this check later ?
  } else if (thread.b) {
    thread.b = false;
    thread.c = noop;
    return thread.e(value);

  } else {
    return crash(error_from("invalid thread_success: thread is not running"));
  }
};


export const thread_error = (thread, value) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid thread_error: thread is killed"));

  // TODO remove this check later ?
  } else if (thread.b) {
    thread.b = false;
    thread.c = noop;
    return thread.f(value);

  } else {
    return crash(error_from("invalid thread_error: thread is not running"));
  }
};


export const thread_kill = (thread) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid thread_kill: thread is already killed"));

  } else {
    const on_thread_kill = thread.c;

    thread.a = true;
    thread.b = false;
    thread.c = noop;
    thread.d = noop;
    thread.e = crash; // TODO is this correct ?

    // TODO only call this if the thread is currently running ?
    return on_thread_kill();
  }
};


// TODO figure out a better way of doing this ?
// TODO test this
// TODO code duplication
export const run_in_sub_thread = (thread, task, on_success, on_error) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid run_in_sub_thread: thread is killed"));

  // TODO remove this check later ?
  } else if (thread.b) {
    return crash(error_from("invalid run_in_sub_thread: another task is already running"));

  } else {
    const old_success = thread.d;
    const old_error = thread.e;

    thread.d = (value) => {
      thread.d = old_success;
      thread.e = old_error;
      return on_success(value);
    };

    thread.e = (value) => {
      thread.d = old_success;
      thread.e = old_error;
      return on_error(value);
    };

    // TODO more robust checks ?
    thread.b = true;

    return task(thread);
  }
};


// TODO should this use try_catch ?
export const run_in_thread = (thread, task) => {
  // TODO remove this check later ?
  if (thread.a) {
    return crash(error_from("invalid run_in_thread: thread is killed"));

  // TODO remove this check later ?
  } else if (thread.b) {
    return crash(error_from("invalid run_in_thread: another task is already running"));

  } else {
    // TODO more robust checks ?
    thread.b = true;

    return task(thread);
  }
};


export const kill_threads = (a) => {
  const length = a.length;

  for (let i = 0; i < length; ++i) {
    thread_kill(a[i]);
  }
};


export const make_thread_pool = () => {
  return {
    a: false, // is_killed
    b: []     // threads
  };
};


export const kill_thread_pool = (pool) => {
  if (pool.a) {
    return crash(error_from("invalid kill_thread_pool: thread pool is already killed"));

  } else {
    const threads = pool.b;

    pool.a = true;
    pool.b = null; // TODO what about type deoptimizations ?

    return kill_threads(threads);
  }
};


export const run_in_thread_pool = (pool, _on_error, task) => {
  if (pool.a) {
    return crash(error_from("invalid run_in_thread_pool: thread pool is killed"));

  } else {
    const thread = make_thread(
      (value) =>
        unsafe_remove_element(pool.b, thread),

      (error) => {
        unsafe_remove_element(pool.b, thread);
        return _on_error(error);
      }
    );

    pool.b.push(thread);

    // TODO use make_thread_run ?
    return run_in_thread(thread, task);
  }
};
