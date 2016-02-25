import { _null } from "./types";
import { async_unkillable } from "./task";


export const wrap = (a) =>
  ({ $: 0, a });

export const sync = (a) =>
  ({ $: 1, a });

export const transform = (a, b) =>
  ({ $: 2, a, b });

export const flatten = (a) =>
  ({ $: 3, a });

export const transform2 = (a, b, c) =>
  ({ $: 4, a, b, c });


export const log = (s) =>
  sync(() => {
    console["log"](s);
    return _null;
  });


const run = (task) => {
  for (;;) {
    switch (task.$) {
    case 0:
      return task.a;
    case 1:
      return task.a();
    case 2:
      return task.b(run(task.a));
    case 3:
      // Tail recursive
      task = run(task.a);
      break;
    default:
      return task.c(run(task.a), run(task.b));
    }
  }
};


// TODO this can be made more efficient
export const task_from = (task) =>
  async_unkillable((success, error) => {
    // TODO does this need to use try_catch ?
    success(run(task));
  });
