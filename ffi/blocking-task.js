import { _null } from "./types";
import { async_unkillable } from "./task";


export const blocking = (a) =>
  ({ $: 0, a });

export const reply = (a) =>
  ({ $: 1, a });

export const chain = (a, b) =>
  ({ $: 2, a, b });

// TODO remove transform and flatten, since they are redundant ?
export const transform = (a, b) =>
  ({ $: 3, a, b });

export const flatten = (a) =>
  ({ $: 4, a });

export const transform2 = (a, b, c) =>
  ({ $: 5, a, b, c });


export const log = (s) =>
  blocking(() => {
    console["log"](s);
    return _null;
  });


export const run = (task) => {
  for (;;) {
    switch (task.$) {
    // *blocking
    case 0:
      return task.a();

    // *reply
    case 1:
      return task.a;

    // *chain
    case 2:
      // Tail recursive
      task = task.b(run(task.a));
      break;

    // *transform
    case 3:
      return task.b(run(task.a));

    // *flatten
    case 4:
      // Tail recursive
      task = run(task.a);
      break;

    // *transform2
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
