import { order, pretty, prettyCharacter, plural, repeat } from "./string";


test("order", () => {
  expect(order("foo", "foo")).toBe(0);

  expect(order("bar", "foo")).toBe(-1);
  expect(order("foo", "bar")).toBe(1);

  expect(order("foo", "foobar")).toBe(-1);
  expect(order("foobar", "foo")).toBe(1);
});


test("pretty", () => {
  expect(pretty("foo")).toBe("\"foo\"");
  expect(pretty("foo'$%\u0000")).toBe("\"foo'$%\u0000\"");
  expect(pretty("foo'$%\u0000\"\\bar")).toBe("\"foo'$%\u0000\\\"\\\\bar\"");
});


test("prettyCharacter", () => {
  expect(prettyCharacter("a")).toBe("a");
  expect(prettyCharacter("foo")).toBe("foo");
  expect(prettyCharacter("\n")).toBe("<NEWLINE>");
  expect(prettyCharacter("\r")).toBe("<NEWLINE>");
  expect(prettyCharacter("\r\n")).toBe("<NEWLINE>");
  expect(prettyCharacter("\n\r")).toBe("<NEWLINE>");
});


test("plural", () => {
  expect(plural(-1, "foo")).toBe("-1foos");
  expect(plural(0, "foo")).toBe("0foos");
  expect(plural(1, "foo")).toBe("1foo");
  expect(plural(2, "foo")).toBe("2foos");
});


test("repeat", () => {
  expect(repeat("1", -3)).toBe("");
  expect(repeat("1", -2)).toBe("");
  expect(repeat("1", -1)).toBe("");
  expect(repeat("1", 0)).toBe("");
  expect(repeat("1", 1)).toBe("1");
  expect(repeat("1", 2)).toBe("11");
  expect(repeat("1", 3)).toBe("111");
  expect(repeat("123", 3)).toBe("123123123");
});
