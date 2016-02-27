import { crash } from "../util/error";
import { pretty, eol, get_message } from "../util/node";
import { indent } from "../util/string";
import { map, length } from "../util/array";
import { fastest, flatten, transform, reply, throw_error,
         wait, concurrent_null, catch_error, make_thread_run } from "../ffi/task";
import { task_from, log } from "../ffi/blocking-task";


const isObject = (x) =>
  x !== null && typeof x === "object";

const isArray = (x) =>
  Array["isArray"](x);

export const equal = (x, y) => {
  if (x === y) {
    return true;

  } else if (isObject(x) && isObject(y)) {
    if (isArray(x)) {
      if (isArray(y) && x["length"] === y["length"]) {
        for (let i = 0; i < x["length"]; ++i) {
          if (!equal(x[i], y[i])) {
            return false;
          }
        }

        return true;

      } else {
        return false;
      }

    } else if (isArray(y)) {
      return false;

    // TODO use equal ?
    } else if (Object["getPrototypeOf"](x) === Object["getPrototypeOf"](y)) {
      for (let key in x) {
        if (!(key in y && equal(x[key], y[key]))) {
          return false;
        }
      }

      for (let key in y) {
        if (!(key in x)) {
          return false;
        }
      }

      return true;

    } else {
      return false;
    }

  } else {
    return false;
  }
};


export const format_error = (message, value, expected) =>
  message + eol +
  "  Expected:" + eol +
  "    " + indent(expected, "    ") + eol +
  "  Got:" + eol +
  (value === null
    ? ""
    : "    " + indent(value, "    "));

export const format_pretty = (message, value, expected) =>
  format_error(message, pretty(value), pretty(expected));


const token = {};

export const test_group = (group_name, a) => {
  const tasks = map(a, (f, index) => {
    const name = group_name + " (test " + (index + 1) + ")";

    return fastest(
      flatten(transform(f(name), (x) => {
        if (x === token) {
          return reply(x);
        } else {
          return throw_error(new Error(name + " invalid unit test"));
        }
      })),

      flatten(transform(wait(10000), (_) =>
        throw_error(new Error(name + " took too long"))))
    );
  });

  return flatten(transform(concurrent_null(tasks), (_) =>
           task_from(log(group_name + ": " + length(tasks) + " tests succeeded\n"))));
};

export const expect = (expected, task) =>
  (name) =>
    flatten(transform(task, (value) =>
      (equal(value, expected)
        ? reply(token)
        : throw_error(new Error(format_pretty(name, value, expected))))));

export const expect_crash = (expected, f) =>
  (name) =>
    flatten(transform(catch_error(f), (e) =>
      (e.$ === 0
        ? throw_error(new Error(format_error(name, null, expected)))
        : (get_message(e.a) === expected
            ? reply(token)
            : throw_error(new Error(format_error(name, get_message(e.a), expected)))))));

export const run_tests = (a) => {
  make_thread_run(flatten(transform(task_from(log("---- Starting unit tests\n")), (_) =>
                  flatten(transform(concurrent_null(a), (_) =>
                          task_from(log("---- All unit tests succeeded")))))));
};


export const assert_crash = (f, expected, message) => {
  try {
    f();

  } catch (e) {
    const value = get_message(e);

    if (value === expected) {
      return;

    } else {
      crash(new Error(format_error(message, value, expected)));
    }
  }

  crash(new Error(format_error(message, null, expected)));
};
