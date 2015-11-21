import { assert_equal, assert_crash } from "../assert";
import { string, symbol, integer } from "../../src/parser/ast.js";
import { tokenize } from "../../src/parser/tokenize.js";
import { lines } from "../../src/util/string.js";


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

test("0.0", (file, lines) =>
  [integer(0, file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 1 }),
   symbol(".", file, lines,
          { line: 0, column: 1 },
          { line: 0, column: 2 }),
   integer(0, file, lines,
           { line: 0, column: 2 },
           { line: 0, column: 3 })]);


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

test_crash("  foo   ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:6)\n" +
  "    foo   \n" +
  "       ^--");

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
  "  #  \n" +
  "   ^-");

test_crash("#/  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:3)\n" +
  "  #/  \n" +
  "    ^-");

test_crash("#/\n\n\n  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 4:1)\n" +
  "    \n" +
  "  ^-");


test_crash("\n   \t   \n",
  "Error: tabs (U+0009) are not allowed  (tokenize.test 2:4)\n" +
  "     \t   \n" +
  "     ^");

test_crash("\n   \t\t\t\t\t   \n",
  "Error: tabs (U+0009) are not allowed  (tokenize.test 2:4)\n" +
  "     \t\t\t\t\t   \n" +
  "     ^----");


test("a#a a a a a a\na", (file, lines) =>
  [symbol("a", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("a", file, lines,
          { line: 1, column: 0 },
          { line: 1, column: 1 })]);

test("a#/a a a a a a/#a", (file, lines) =>
  [symbol("a", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("a", file, lines,
          { line: 0, column: 16 },
          { line: 0, column: 17 })]);

test("a#/a\na a\na a\na/#a", (file, lines) =>
  [symbol("a", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 1 }),
   symbol("a", file, lines,
          { line: 3, column: 3 },
          { line: 3, column: 4 })]);

test_crash("a# \na",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:3)\n" +
  "  a# \n" +
  "    ^");

test_crash("a#/a\n",
  "Error: missing ending /#  (tokenize.test 1:2)\n" +
  "  a#/a\n" +
  "   ^-");

test_crash("a#/a#/a\n",
  "Error: missing ending /#  (tokenize.test 1:5)\n" +
  "  a#/a#/a\n" +
  "      ^-");

test_crash("a#/a#/a/#\n",
  "Error: missing ending /#  (tokenize.test 1:2)\n" +
  "  a#/a#/a/#\n" +
  "   ^-");


test_crash("\"",
  "Error: missing ending \"  (tokenize.test 1:1)\n" +
  "  \"\n" +
  "  ^");

test_crash("\"  ",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
  "  \"  \n" +
  "   ^-");

test_crash("\"foo\nbar",
  "Error: there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "  bar\n" +
  "  ^");

test_crash("\"\n\"",
  "Error: there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "  \"\n" +
  "  ^");

test_crash("\"\nfoo\"",
  "Error: there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "  foo\"\n" +
  "  ^");

test_crash("    \"\nfoo\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "  foo\"\n" +
  "  ^");

test_crash("    \"\n foo\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "   foo\"\n" +
  "  ^");

test_crash("    \"\n  foo\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "    foo\"\n" +
  "  ^-");

test_crash("    \"\n   foo\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "     foo\"\n" +
  "  ^--");

test_crash("    \"\n    foo\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
  "      foo\"\n" +
  "  ^---");

test("    \"\n     foo\"", (file, lines) =>
  [string("\nfoo", file, lines,
          { line: 0, column: 4 },
          { line: 1, column: 9 })]);

test_crash("    \"\n\n\n     foo\n    bar\"",
  "Error: there must be 5 or more spaces (U+0020)  (tokenize.test 5:1)\n" +
  "      bar\"\n" +
  "  ^---");

test("    \"\n\n\n     foo\n     bar\"", (file, lines) =>
  [string("\n\n\nfoo\nbar", file, lines,
          { line: 0, column: 4 },
          { line: 4, column: 9 })]);

test_crash("    \" foo\n       bar\n \n\n     qux\"",
  "Error: spaces (U+0020) are not allowed at the end of the line  (tokenize.test 3:1)\n" +
  "   \n" +
  "  ^");

test("\"\n \"", (file, lines) =>
  [string("\n", file, lines,
          { line: 0, column: 0 },
          { line: 1, column: 2 })]);

test("\"foo\n bar\"", (file, lines) =>
  [string("foo\nbar", file, lines,
          { line: 0, column: 0 },
          { line: 1, column: 5 })]);

test("    \"foo\n     bar\"", (file, lines) =>
  [string("foo\nbar", file, lines,
          { line: 0, column: 4 },
          { line: 1, column: 9 })]);

test("    \" foo\n       bar\"", (file, lines) =>
  [string(" foo\n  bar", file, lines,
          { line: 0, column: 4 },
          { line: 1, column: 11 })]);

test("    \" foo\n       bar\n\n\n     qux\"", (file, lines) =>
  [string(" foo\n  bar\n\n\nqux", file, lines,
          { line: 0, column: 4 },
          { line: 4, column: 9 })]);


test("\"foobar\"", (file, lines) =>
  [string("foobar", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 8 })]);

test("\"foobar\"a", (file, lines) =>
  [string("foobar", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 8 }),
   symbol("a", file, lines,
          { line: 0, column: 8 },
          { line: 0, column: 9 })]);

test("\"foo   bar  \"", (file, lines) =>
  [string("foo   bar  ", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 13 })]);

// TODO should treat all Unicode characters as a single width
test("\"!\uD834\uDF06\"", (file, lines) =>
  [string("!\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 5 })]);


test("\"\\\\\"", (file, lines) =>
  [string("\\", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 4 })]);

test("\"\\\"\"", (file, lines) =>
  [string("\"", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 4 })]);

test("\"\\s\"", (file, lines) =>
  [string(" ", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 4 })]);

test("\"\\n\"", (file, lines) =>
  [string("\n", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 4 })]);

test_crash("\"\\a\"",
  "Error: expected one of [\\\\ \\\" \\s \\n \\u] but got \\a  (tokenize.test 1:2)\n" +
  "  \"\\a\"\n" +
  "   ^-");

test_crash("\"\\t\"",
  "Error: expected one of [\\\\ \\\" \\s \\n \\u] but got \\t  (tokenize.test 1:2)\n" +
  "  \"\\t\"\n" +
  "   ^-");

test_crash("\"\\\n",
  "Error: expected one of [\\\\ \\\" \\s \\n \\u] but got \\  (tokenize.test 1:2)\n" +
  "  \"\\\n" +
  "   ^");


test_crash("\"\\u\"",
  "Error: expected \\u[ but got \\u\"  (tokenize.test 1:2)\n" +
  "  \"\\u\"\n" +
  "   ^--");

test_crash("\"\\u\n\"",
  "Error: expected \\u[ but got \\u  (tokenize.test 1:2)\n" +
  "  \"\\u\n" +
  "   ^-");

test_crash("\"\\ua\"",
  "Error: expected \\u[ but got \\ua  (tokenize.test 1:2)\n" +
  "  \"\\ua\"\n" +
  "   ^--");

test("\"\\u[21 1D306]\"", (file, lines) =>
  [string("!\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 14 })]);

test("\"\\u[21 1D306]a\"", (file, lines) =>
  [string("!\uD834\uDF06a", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 15 })]);

test("\"\\u[21]\\u[1D306]\"", (file, lines) =>
  [string("!\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 17 })]);

test_crash("\"\\u[]",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got ]  (tokenize.test 1:5)\n" +
  "  \"\\u[]\n" +
  "      ^");

test_crash("\"\\u[",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:5)\n" +
  "  \"\\u[\n" +
  "      ^");

test_crash("\"\\u[\n",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:5)\n" +
  "  \"\\u[\n" +
  "      ^");

test_crash("\"\\u[a",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got a  (tokenize.test 1:5)\n" +
  "  \"\\u[a\n" +
  "      ^");

test_crash("\"\\u[A",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:6)\n" +
  "  \"\\u[A\n" +
  "       ^");

test_crash("\"\\u[A\n",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:6)\n" +
  "  \"\\u[A\n" +
  "       ^");

test_crash("\"\\u[A  A",
  "Error: expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got    (tokenize.test 1:7)\n" +
  "  \"\\u[A  A\n" +
  "        ^");


// TODO should treat all Unicode characters as a single width
test("\uD834\uDF06", (file, lines) =>
  [symbol("\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 2 })]);


test("1 1.5 1,5 (foo bar qux) u@q\nqux\n(nou)\n~\n~foo\n~@foo", (file, lines) =>
  [integer(1, file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 1 }),
   integer(1, file, lines,
           { line: 0, column: 2 },
           { line: 0, column: 3 }),
   symbol(".", file, lines,
          { line: 0, column: 3 },
          { line: 0, column: 4 }),
   integer(5, file, lines,
           { line: 0, column: 4 },
           { line: 0, column: 5 }),
   symbol("1,5", file, lines,
          { line: 0, column: 6 },
          { line: 0, column: 9 }),
   symbol("(", file, lines,
          { line: 0, column: 10 },
          { line: 0, column: 11 }),
   symbol("foo", file, lines,
          { line: 0, column: 11 },
          { line: 0, column: 14 }),
   symbol("bar", file, lines,
          { line: 0, column: 15 },
          { line: 0, column: 18 }),
   symbol("qux", file, lines,
          { line: 0, column: 19 },
          { line: 0, column: 22 }),
   symbol(")", file, lines,
          { line: 0, column: 22 },
          { line: 0, column: 23 }),
   symbol("u", file, lines,
          { line: 0, column: 24 },
          { line: 0, column: 25 }),
   symbol("@", file, lines,
          { line: 0, column: 25 },
          { line: 0, column: 26 }),
   symbol("q", file, lines,
          { line: 0, column: 26 },
          { line: 0, column: 27 }),
   symbol("qux", file, lines,
          { line: 1, column: 0 },
          { line: 1, column: 3 }),
   symbol("(", file, lines,
          { line: 2, column: 0 },
          { line: 2, column: 1 }),
   symbol("nou", file, lines,
          { line: 2, column: 1 },
          { line: 2, column: 4 }),
   symbol(")", file, lines,
          { line: 2, column: 4 },
          { line: 2, column: 5 }),
   symbol("~", file, lines,
          { line: 3, column: 0 },
          { line: 3, column: 1 }),
   symbol("~", file, lines,
          { line: 4, column: 0 },
          { line: 4, column: 1 }),
   symbol("foo", file, lines,
          { line: 4, column: 1 },
          { line: 4, column: 4 }),
   symbol("~", file, lines,
          { line: 5, column: 0 },
          { line: 5, column: 1 }),
   symbol("@", file, lines,
          { line: 5, column: 1 },
          { line: 5, column: 2 }),
   symbol("foo", file, lines,
          { line: 5, column: 2 },
          { line: 5, column: 5 })]);
