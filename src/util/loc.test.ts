import { concat, pretty, loc, position } from "./loc";


test("position", () => {
  expect(position(0, 0, 0)).toEqual({
    index: 0,
    line: 0,
    column: 0
  });

  expect(position(1, 0, 0)).toEqual({
    index: 1,
    line: 0,
    column: 0
  });

  expect(position(1, 2, 0)).toEqual({
    index: 1,
    line: 2,
    column: 0
  });

  expect(position(1, 2, 3)).toEqual({
    index: 1,
    line: 2,
    column: 3
  });
});


test("loc", () => {
  expect(loc("foo.nul", position(0, 1, 2), position(3, 4, 5))).toEqual({
    filename: "foo.nul",
    start: {
      index: 0,
      line: 1,
      column: 2
    },
    end: {
      index: 3,
      line: 4,
      column: 5
    }
  });
});


test("concat", () => {
  expect(concat(loc("foo.nul", position(0, 1, 2), position(3, 4, 5)),
                loc("bar.nul", position(6, 7, 8), position(9, 10, 11)))).toEqual({
    filename: "bar.nul",
    start: {
      index: 0,
      line: 1,
      column: 2
    },
    end: {
      index: 9,
      line: 10,
      column: 11
    }
  });
});


test("pretty", () => {
  expect(pretty(loc("foo.nul", position(0, 1, 2), position(3, 4, 5)))).toBe("file \u001b[32m\"foo.nul\"\u001b[39m at position \u001b[32m2:3\u001b[39m to \u001b[32m5:6\u001b[39m");
});
