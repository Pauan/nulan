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


export const log = (s) =>
  blocking(() => {
    console["log"](s);
    return null;
  });

// TODO is this biased ?
// TODO use crypto ?
export const random_integer = (min, max) =>
  blocking(() =>
    Math["floor"](Math["random"]() * (max - min + 1)) + min);


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
    default:
      // Tail recursive
      task = run(task.a);
      break;
    }
  }
};


// TODO this can be made more efficient
export const task_from = (task) =>
  async_unkillable((success, error) =>
    success(run(task)));
