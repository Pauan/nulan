import { NulanError, warning, assertExists, assert } from "./error";
import { loc, position } from "./loc";
import { EOL } from "./node";


test("assertExists", () => {
  expect(assertExists(1)).toBe(1);
  expect(assertExists("1")).toBe("1");

  expect(() => assertExists(null)).toThrow("Value cannot be null or undefined");
  expect(() => assertExists(undefined)).toThrow("Value cannot be null or undefined");
});


test("assert", () => {
  expect(assert(true)).toBe(undefined);
  expect(() => assert(false)).toThrow("Assertion failed");
});


test("NulanError", () => {
  expect(new NulanError(loc("foo.nul", position(0, 1, 2), position(3, 4, 5)), "test\ntest\ntest").toString()).toBe(
    "\u001b[1m\u001b[31m\u001b[2mError in file \u001b[32m\"foo.nul\"\u001b[31m at position \u001b[32m2:3\u001b[31m to \u001b[32m5:6\u001b[31m:\u001b[1m\u001b[39m\u001b[22m\n\u001b[1m  test\u001b[22m\n\u001b[1m  test\u001b[22m\n\u001b[1m  test\u001b[22m".replace(/\n/g, EOL)
  );
});


test("warning", () => {
  var old = global.console;

  global.console = <any>{
    warn: jest.fn()
  };

  try {
    warning(loc("foo.nul", position(0, 1, 2), position(3, 4, 5)), "test\ntest\ntest");

    expect(console.warn).toHaveBeenCalledWith(
      "\u001b[1m\u001b[35m\u001b[2mWarning in file \u001b[32m\"foo.nul\"\u001b[35m at position \u001b[32m2:3\u001b[35m to \u001b[32m5:6\u001b[35m:\u001b[1m\u001b[39m\u001b[22m\n\u001b[1m  test\u001b[22m\n\u001b[1m  test\u001b[22m\n\u001b[1m  test\u001b[22m".replace(/\n/g, EOL)
    );

  } finally {
    global.console = old;
  }
});
