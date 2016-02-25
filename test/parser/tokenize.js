import { expect, expect_crash } from "../assert";
import { wrap } from "../../ffi/task";
import { loc, text, symbol, integer } from "../../src/parser/ast";
import { tokenize } from "../../src/parser/tokenize";
import { lines } from "../../util/string";


const file = "tokenize.test";

const test = (input, f) => {
  const x = lines(input);

  const _loc = (line1, column1, line2, column2) =>
    loc(file, x,
        { line: line1, column: column1 },
        { line: line2, column: column2 });

  return expect(f(_loc),
           wrap(tokenize(x, file)));
};

const test_crash = (input, expected) =>
  expect_crash("Error: " + expected, () => {
    const x = lines(input);
    return wrap(tokenize(x, file));
  });


export default [
  test("", (loc) =>
    []),

  test("\n\n\n\n\n", (loc) =>
    []),


  test("foo", (loc) => [
    symbol("foo", loc(0, 0,
                      0, 3))
  ]),

  test("foo\n", (loc) => [
    symbol("foo", loc(0, 0,
                      0, 3))
  ]),

  test("\nfoo\n", (loc) => [
    symbol("foo", loc(1, 0,
                      1, 3))
  ]),

  test("\nfoo\nbar\n", (loc) => [
    symbol("foo", loc(1, 0,
                      1, 3)),
    symbol("bar", loc(2, 0,
                      2, 3))
  ]),

  test("\n foo\n   bar\n", (loc) => [
    symbol("foo", loc(1, 1,
                      1, 4)),
    symbol("bar", loc(2, 3,
                      2, 6))
  ]),


  test("0", (loc) => [
    integer(0, loc(0, 0,
                   0, 1))
  ]),

  test("0a", (loc) => [
    symbol("0a", loc(0, 0,
                     0, 2))
  ]),

  test("a0", (loc) => [
    symbol("a0", loc(0, 0,
                     0, 2))
  ]),

  test("0.0", (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(".", loc(0, 1,
                    0, 2)),
    integer(0, loc(0, 2,
                   0, 3))
  ]),


  test("*foo", (loc) => [
    symbol("*foo", loc(0, 0,
                       0, 4))
  ]),

  test("$foo", (loc) => [
    symbol("$foo", loc(0, 0,
                       0, 4))
  ]),


  test(" foo bar", (loc) => [
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("bar", loc(0, 5,
                      0, 8))
  ]),

  test("(foo(bar(", (loc) => [
    symbol("(", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("(", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("(", loc(0, 8,
                    0, 9))
  ]),

  test(")foo)bar)", (loc) => [
    symbol(")", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol(")", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol(")", loc(0, 8,
                    0, 9))
  ]),

  test("[foo[bar[", (loc) => [
    symbol("[", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("[", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("[", loc(0, 8,
                    0, 9))
  ]),

  test("]foo]bar]", (loc) => [
    symbol("]", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("]", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("]", loc(0, 8,
                    0, 9))
  ]),

  test("{foo{bar{", (loc) => [
    symbol("{", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("{", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("{", loc(0, 8,
                    0, 9))
  ]),

  test("}foo}bar}", (loc) => [
    symbol("}", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("}", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("}", loc(0, 8,
                    0, 9))
  ]),

  test("&foo&bar&", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("&", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("&", loc(0, 8,
                    0, 9))
  ]),

  test("~foo~bar~", (loc) => [
    symbol("~", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("~", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("~", loc(0, 8,
                    0, 9))
  ]),

  test("@foo@bar@", (loc) => [
    symbol("@", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("@", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol("@", loc(0, 8,
                    0, 9))
  ]),

  test(".foo.bar.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol(".", loc(0, 4,
                    0, 5)),
    symbol("bar", loc(0, 5,
                      0, 8)),
    symbol(".", loc(0, 8,
                    0, 9))
  ]),


  test(".(.(.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("(", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("(", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".).).", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol(")", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol(")", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".[.[.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("[", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("[", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".].].", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("]", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("]", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".{.{.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("{", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("{", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".}.}.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("}", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("}", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".&.&.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("&", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".~.~.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("~", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("~", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".@.@.", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol("@", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol("@", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),

  test(".....", (loc) => [
    symbol(".", loc(0, 0,
                    0, 1)),
    symbol(".", loc(0, 1,
                    0, 2)),
    symbol(".", loc(0, 2,
                    0, 3)),
    symbol(".", loc(0, 3,
                    0, 4)),
    symbol(".", loc(0, 4,
                    0, 5))
  ]),


  test_crash("    ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:1)\n" +
    "      \n" +
    "  ^---"),

  test_crash("\n    ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 2:1)\n" +
    "      \n" +
    "  ^---"),

  test_crash("\n    \n",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 2:1)\n" +
    "      \n" +
    "  ^---"),

  test_crash("  foo   ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:6)\n" +
    "    foo   \n" +
    "       ^--"),

  test_crash("\"  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
    "  \"  \n" +
    "   ^-"),

  test_crash("\"\n\n\n  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 4:1)\n" +
    "    \n" +
    "  ^-"),

  test_crash("#  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
    "  #  \n" +
    "   ^-"),

  test_crash("#/  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:3)\n" +
    "  #/  \n" +
    "    ^-"),

  test_crash("#/\n\n\n  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 4:1)\n" +
    "    \n" +
    "  ^-"),


  test_crash("\n   \t   \n",
    "tabs (U+0009) are not allowed  (tokenize.test 2:4)\n" +
    "     \t   \n" +
    "     ^"),

  test_crash("\n   \t\t\t\t\t   \n",
    "tabs (U+0009) are not allowed  (tokenize.test 2:4)\n" +
    "     \t\t\t\t\t   \n" +
    "     ^----"),


  test("a#a a a a a a\na", (loc) => [
    symbol("a", loc(0, 0,
                    0, 1)),
    symbol("a", loc(1, 0,
                    1, 1))
  ]),

  test("a#/a a a a a a/#a", (loc) => [
    symbol("a", loc(0, 0,
                    0, 1)),
    symbol("a", loc(0, 16,
                    0, 17))
  ]),

  test("a#/a a #/a a/# a a/#a", (loc) => [
    symbol("a", loc(0, 0,
                    0, 1)),
    symbol("a", loc(0, 20,
                    0, 21))
  ]),

  test("a#/a\na a\na a\na/#a", (loc) => [
    symbol("a", loc(0, 0,
                    0, 1)),
    symbol("a", loc(3, 3,
                    3, 4))
  ]),

  test_crash("a# \na",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:3)\n" +
    "  a# \n" +
    "    ^"),

  test_crash("a#/a\n",
    "missing ending /#  (tokenize.test 1:2)\n" +
    "  a#/a\n" +
    "   ^-"),

  test_crash("a#/a#/a\n",
    "missing ending /#  (tokenize.test 1:5)\n" +
    "  a#/a#/a\n" +
    "      ^-"),

  test_crash("a#/a#/a/#\n",
    "missing ending /#  (tokenize.test 1:2)\n" +
    "  a#/a#/a/#\n" +
    "   ^-"),


  test_crash("\"",
    "missing ending \"  (tokenize.test 1:1)\n" +
    "  \"\n" +
    "  ^"),

  test_crash("\"  ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:2)\n" +
    "  \"  \n" +
    "   ^-"),

  test_crash("\"foo\nbar",
    "there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "  bar\n" +
    "  ^"),

  test_crash("\"\n\"",
    "there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "  \"\n" +
    "  ^"),

  test_crash("\"\nfoo\"",
    "there must be 1 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "  foo\"\n" +
    "  ^"),

  test_crash("    \"\nfoo\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "  foo\"\n" +
    "  ^"),

  test_crash("    \"\n foo\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "   foo\"\n" +
    "  ^"),

  test_crash("    \"\n  foo\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "    foo\"\n" +
    "  ^-"),

  test_crash("    \"\n   foo\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "     foo\"\n" +
    "  ^--"),

  test_crash("    \"\n    foo\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "      foo\"\n" +
    "  ^---"),

  test("    \"\n     foo\"", (loc) => [
    text("\nfoo", loc(0, 4,
                      1, 9))
  ]),

  test_crash("    \"\n\n\n     foo\n    bar\"",
    "there must be 5 or more spaces (U+0020)  (tokenize.test 5:1)\n" +
    "      bar\"\n" +
    "  ^---"),

  test("    \"\n\n\n     foo\n     bar\"", (loc) => [
    text("\n\n\nfoo\nbar", loc(0, 4,
                               4, 9))
  ]),

  test_crash("    \" foo\n       bar\n \n\n     qux\"",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 3:1)\n" +
    "   \n" +
    "  ^"),

  test("\"\n \"", (loc) => [
    text("\n", loc(0, 0,
                   1, 2))
  ]),

  test("\"foo\n bar\"", (loc) => [
    text("foo\nbar", loc(0, 0,
                         1, 5))
  ]),

  test("    \"foo\n     bar\"", (loc) => [
    text("foo\nbar", loc(0, 4,
                         1, 9))
  ]),

  test("    \" foo\n       bar\"", (loc) => [
    text(" foo\n  bar", loc(0, 4,
                            1, 11))
  ]),

  test("    \" foo\n       bar\n\n\n     qux\"", (loc) => [
    text(" foo\n  bar\n\n\nqux", loc(0, 4,
                                     4, 9))
  ]),


  test("\"foobar\"", (loc) => [
    text("foobar", loc(0, 0,
                       0, 8))
  ]),

  test("\"foobar\"a", (loc) => [
    text("foobar", loc(0, 0,
                       0, 8)),
    symbol("a", loc(0, 8,
                    0, 9))
  ]),

  test("\"foo   bar  \"", (loc) => [
    text("foo   bar  ", loc(0, 0,
                            0, 13))
  ]),

  // TODO should treat all Unicode characters as a single width
  test("\"!\uD834\uDF06\"", (loc) => [
    text("!\uD834\uDF06", loc(0, 0,
                              0, 5))
  ]),


  test("\"\\\\\"", (loc) => [
    text("\\", loc(0, 0,
                   0, 4))
  ]),

  test("\"\\\"\"", (loc) => [
    text("\"", loc(0, 0,
                   0, 4))
  ]),

  test("\"\\s\"", (loc) => [
    text(" ", loc(0, 0,
                  0, 4))
  ]),

  test("\"\\n\"", (loc) => [
    text("\n", loc(0, 0,
                   0, 4))
  ]),

  test_crash("\"\\a\"",
    "expected one of [\\\\ \\\" \\s \\n \\u] but got \\a  (tokenize.test 1:2)\n" +
    "  \"\\a\"\n" +
    "   ^-"),

  test_crash("\"\\t\"",
    "expected one of [\\\\ \\\" \\s \\n \\u] but got \\t  (tokenize.test 1:2)\n" +
    "  \"\\t\"\n" +
    "   ^-"),

  test_crash("\"\\\n",
    "expected one of [\\\\ \\\" \\s \\n \\u] but got \\  (tokenize.test 1:2)\n" +
    "  \"\\\n" +
    "   ^"),


  test_crash("\"\\u\"",
    "expected \\u[ but got \\u\"  (tokenize.test 1:2)\n" +
    "  \"\\u\"\n" +
    "   ^--"),

  test_crash("\"\\u\n\"",
    "expected \\u[ but got \\u  (tokenize.test 1:2)\n" +
    "  \"\\u\n" +
    "   ^-"),

  test_crash("\"\\ua\"",
    "expected \\u[ but got \\ua  (tokenize.test 1:2)\n" +
    "  \"\\ua\"\n" +
    "   ^--"),

  test("\"\\u[21 1D306]\"", (loc) => [
    text("!\uD834\uDF06", loc(0, 0,
                              0, 14))
  ]),

  test("\"\\u[21 1D306]a\"", (loc) => [
    text("!\uD834\uDF06a", loc(0, 0,
                               0, 15))
  ]),

  test("\"\\u[21]\\u[1D306]\"", (loc) => [
    text("!\uD834\uDF06", loc(0, 0,
                              0, 17))
  ]),

  test_crash("\"\\u[]",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got ]  (tokenize.test 1:5)\n" +
    "  \"\\u[]\n" +
    "      ^"),

  test_crash("\"\\u[",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:5)\n" +
    "  \"\\u[\n" +
    "      ^"),

  test_crash("\"\\u[\n",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:5)\n" +
    "  \"\\u[\n" +
    "      ^"),

  test_crash("\"\\u[a",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got a  (tokenize.test 1:5)\n" +
    "  \"\\u[a\n" +
    "      ^"),

  test_crash("\"\\u[A",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:6)\n" +
    "  \"\\u[A\n" +
    "       ^"),

  test_crash("\"\\u[A\n",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got #<EOL>  (tokenize.test 1:6)\n" +
    "  \"\\u[A\n" +
    "       ^"),

  test_crash("\"\\u[A  A",
    "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got    (tokenize.test 1:7)\n" +
    "  \"\\u[A  A\n" +
    "        ^"),


  // TODO should treat all Unicode characters as a single width
  test("\uD834\uDF06", (loc) => [
    symbol("\uD834\uDF06", loc(0, 0,
                               0, 2))
  ]),


  test("1 1.5 1,5 (foo bar qux) u@q\nqux\n(nou)\n~\n~foo\n~@foo", (loc) => [
    integer(1, loc(0, 0,
                   0, 1)),
    integer(1, loc(0, 2,
                   0, 3)),
    symbol(".", loc(0, 3,
                    0, 4)),
    integer(5, loc(0, 4,
                   0, 5)),
    symbol("1,5", loc(0, 6,
                      0, 9)),
    symbol("(", loc(0, 10,
                    0, 11)),
    symbol("foo", loc(0, 11,
                      0, 14)),
    symbol("bar", loc(0, 15,
                      0, 18)),
    symbol("qux", loc(0, 19,
                      0, 22)),
    symbol(")", loc(0, 22,
                    0, 23)),
    symbol("u", loc(0, 24,
                    0, 25)),
    symbol("@", loc(0, 25,
                    0, 26)),
    symbol("q", loc(0, 26,
                    0, 27)),
    symbol("qux", loc(1, 0,
                      1, 3)),
    symbol("(", loc(2, 0,
                    2, 1)),
    symbol("nou", loc(2, 1,
                      2, 4)),
    symbol(")", loc(2, 4,
                    2, 5)),
    symbol("~", loc(3, 0,
                    3, 1)),
    symbol("~", loc(4, 0,
                    4, 1)),
    symbol("foo", loc(4, 1,
                      4, 4)),
    symbol("~", loc(5, 0,
                    5, 1)),
    symbol("@", loc(5, 1,
                    5, 2)),
    symbol("foo", loc(5, 2,
                      5, 5))
  ]),
];
