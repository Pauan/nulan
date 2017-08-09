import { order, pretty, prettyCharacter } from "./string";


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
