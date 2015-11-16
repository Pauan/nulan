import { assert_equal, assert_crash } from "../assert";
import { string, symbol, integer } from "../../src/parser/types.js";
import { tokenize } from "../../src/parser/tokenize.js";
import { lines } from "../../src/parser/string.js";


const file = "tokenize.test";

const test = (input, f) => {
  const x = lines(input);
  assert_equal(tokenize(x, file), f(file, x), input);
};

const test_crash = (input, expected) => {
  assert_crash(() => {
    const x = lines(input);
    tokenize(x, file);
  }, expected, input);
};


test("", (file, lines) =>
  []);

test("\n\n\n\n\n", (file, lines) =>
  []);


test("foo", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 3 })]);

test("foo\n", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 3 })]);

test("\nfoo\n", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 1, column: 0 },
          { line: 1, column: 3 })]);

test("\nfoo\nbar\n", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 1, column: 0 },
          { line: 1, column: 3 }),
   symbol("bar", file, lines,
          { line: 2, column: 0 },
          { line: 2, column: 3 })]);

test("\n foo\n   bar\n", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 1, column: 1 },
          { line: 1, column: 4 }),
   symbol("bar", file, lines,
          { line: 2, column: 3 },
          { line: 2, column: 6 })]);


test("0", (file, lines) =>
  [integer(0, file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 1 })]);

test("0a", (file, lines) =>
  [symbol("0a", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 2 })]);

test("a0", (file, lines) =>
  [symbol("a0", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 2 })]);


test(" foo bar", (file, lines) =>
  [symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 })]);

test("(foo(bar(", (file, lines) =>
  [symbol("(", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("(", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("(", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test(")foo)bar)", (file, lines) =>
  [symbol(")", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol(")", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol(")", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("[foo[bar[", (file, lines) =>
  [symbol("[", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("[", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("[", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("]foo]bar]", (file, lines) =>
  [symbol("]", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("]", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("]", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("{foo{bar{", (file, lines) =>
  [symbol("{", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("{", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("{", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("}foo}bar}", (file, lines) =>
  [symbol("}", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("}", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("}", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("&foo&bar&", (file, lines) =>
  [symbol("&", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("&", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("&", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("~foo~bar~", (file, lines) =>
  [symbol("~", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("~", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("~", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("@foo@bar@", (file, lines) =>
  [symbol("@", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol("@", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol("@", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test(".foo.bar.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("foo", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 }),
   symbol("bar", file, lines,
          { line: 0, column: 5 },
          { line: 0, column: 8 }),
   symbol(".", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);


test(".(.(.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("(", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("(", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".).).", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol(")", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol(")", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".[.[.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("[", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("[", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".].].", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("]", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("]", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".{.{.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("{", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("{", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".}.}.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("}", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("}", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".&.&.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("&", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("&", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".~.~.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("~", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("~", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".@.@.", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("@", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol("@", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);

test(".....", (file, lines) =>
  [symbol(".", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol(".", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   symbol(".", file, lines,
          { line: 0, column: 2 },
          { line: 0, column: 3 }),
   symbol(".", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   symbol(".", file, lines,
          { line: 0, column: 4 },
          { line: 0, column: 5 })]);


test_crash("    ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:1)\n" +
  "      \n" +
  "  ^---");

test_crash("\n    ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 2:1)\n" +
  "      \n" +
  "  ^---");

test_crash("\n    \n",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 2:1)\n" +
  "      \n" +
  "  ^---");

test_crash("\"  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
  "  \"  \n" +
  "   ^-");

test_crash("\"\n\n\n  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 4:1)\n" +
  "    \n" +
  "  ^-");

test_crash("#  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
  "    \n" +
  "  ^-");

test_crash("#/  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
  "    \n" +
  "  ^-");


test("\"\\u{21}\\u{1D306}\"", (file, lines) =>
  [string("!\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 17 })]);
