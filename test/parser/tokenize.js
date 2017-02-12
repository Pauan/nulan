import { expect, expect_crash } from "../assert";
import { reply } from "../../builtin/ffi/task";
import { loc, text, symbol, integer, number } from "../../src/parser/type";
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
           reply(tokenize(x, file)));
};

const test_crash = (input, expected) =>
  expect_crash("Error: " + expected, () => {
    const x = lines(input);
    return reply(tokenize(x, file));
  });


const test_prefix = (input) => [
  test_crash("0" + input + "1",
    "cannot have an expression on the left side of " + input + "  (tokenize.test 1:2)\n" +
    "  0" + input + "1\n" +
    "   ^"),

  test("(" + input + "1", (loc) => [
    symbol("(", loc(0, 0,
                    0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    integer(1, loc(0, 2,
                   0, 3))
  ]),

  test(input + input + "1", (loc) => [
    symbol(input, loc(0, 0,
                      0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    integer(1, loc(0, 2,
                   0, 3))
  ]),

  test("0 " + input + "1", (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(input, loc(0, 2,
                      0, 3)),
    integer(1, loc(0, 3,
                   0, 4))
  ]),

  test_crash("0 " + input + " 1",
    "spaces (U+0020) are not allowed after " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + " 1\n" +
    "    ^"),

  test_crash("0 " + input,
    "missing expression on the right side of " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + "\n" +
    "    ^"),

  test_crash("0 " + input + "\n",
    "missing expression on the right side of " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + "\n" +
    "    ^")
];


const test_suffix = (input) => [
  test_crash("0" + input + "1",
    "cannot have an expression on the right side of " + input + "  (tokenize.test 1:2)\n" +
    "  0" + input + "1\n" +
    "   ^"),

  test("0" + input + ")", (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    symbol(")", loc(0, 2,
                    0, 3))
  ]),

  test("0" + input + input, (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    symbol(input, loc(0, 2,
                      0, 3))
  ]),

  test("0" + input + " 1", (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    integer(1, loc(0, 3,
                   0, 4))
  ]),

  test_crash("0 " + input + "1",
    "spaces (U+0020) are not allowed before " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + "1\n" +
    "    ^"),

  test_crash(input,
    "missing expression on the left side of " + input + "  (tokenize.test 1:1)\n" +
    "  " + input + "\n" +
    "  ^")
];


const test_infix = (input) => [
  test("0" + input + "1", (loc) => [
    integer(0, loc(0, 0,
                   0, 1)),
    symbol(input, loc(0, 1,
                      0, 2)),
    integer(1, loc(0, 2,
                   0, 3))
  ]),

  test_crash("0" + input + " 1",
    "spaces (U+0020) are not allowed after " + input + "  (tokenize.test 1:2)\n" +
    "  0" + input + " 1\n" +
    "   ^"),

  test_crash("0 " + input + "1",
    "spaces (U+0020) are not allowed before " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + "1\n" +
    "    ^"),

  test_crash("0 " + input + " 1",
    "spaces (U+0020) are not allowed before " + input + "  (tokenize.test 1:3)\n" +
    "  0 " + input + " 1\n" +
    "    ^"),

  test_crash(input,
    "missing expression on the left side of " + input + "  (tokenize.test 1:1)\n" +
    "  " + input + "\n" +
    "  ^"),

  test_crash("0" + input,
    "missing expression on the right side of " + input + "  (tokenize.test 1:2)\n" +
    "  0" + input + "\n" +
    "   ^"),

  test_crash("0" + input + "\n",
    "missing expression on the right side of " + input + "  (tokenize.test 1:2)\n" +
    "  0" + input + "\n" +
    "   ^"),

  test_crash(input + "0",
    "missing expression on the left side of " + input + "  (tokenize.test 1:1)\n" +
    "  " + input + "0\n" +
    "  ^")
];


const test_brackets = (start, end, is_space) => {
  const space = (is_space ? " " : "");

  const start2 = start + space;

  const end2 = space + end;

  const l = (is_space ? 2 : 1);

  return [
    test(start2 + "foo\n  bar" + end2, (loc) => [
      symbol(start, loc(0, 0,
                        0, 1)),
      symbol("foo", loc(0, l,
                        0, l + 3)),
      symbol("bar", loc(1, 2,
                        1, 5)),
      symbol(end, loc(1, l + 4,
                      1, l + 5))
    ]),

    test(start2 + "foo\n  bar " + start2 + "qux\n        corge" + end2 + end2, (loc) => [
      symbol(start, loc(0, 0,
                        0, 1)),
      symbol("foo", loc(0, l,
                        0, l + 3)),
      symbol("bar", loc(1, 2,
                        1, 5)),
      symbol(start, loc(1, 6,
                        1, 7)),
      symbol("qux", loc(1, l + 6,
                        1, l + 6 + 3)),
      symbol("corge", loc(2, 8,
                          2, 13)),
      symbol(end, loc(2, l + 12,
                      2, l + 13)),
      symbol(end, loc(2, l + l + 12,
                      2, l + l + 13))
    ]),

    test(start2 + "foo " + start2 + "bar " + start + end + end2 + end2, (loc) => [
      symbol(start, loc(0, 0,
                        0, 1)),
      symbol("foo", loc(0, l,
                        0, l + 3)),
      symbol(start, loc(0, l + 4,
                        0, l + 5)),
      symbol("bar", loc(0, l + 4 + l,
                        0, l + 4 + l + 3)),
      symbol(start, loc(0, l + 4 + l + 4,
                        0, l + 4 + l + 5)),
      symbol(end, loc(0, l + 4 + l + 5,
                      0, l + 4 + l + 6)),
      symbol(end, loc(0, l + 4 + l + 5 + l,
                      0, l + 4 + l + 5 + l + 1)),
      symbol(end, loc(0, l + 4 + l + 5 + l + l,
                      0, l + 4 + l + 5 + l + l + 1))
    ]),

    test_crash(start2 + "foo\n  #/foo\n/# nou" + end2,
      "expected 2 spaces but got 0  (tokenize.test 3:1)\n" +
      "  /# nou" + end2 + "\n" +
      "  ^"),

    test_crash(" " + start2 + "foo\n bar" + end2,
      "expected 0 spaces but got 1  (tokenize.test 1:1)\n" +
      "   " + start2 + "foo\n" +
      "  ^"),

    test_crash(start2 + "foo\n bar" + end2,
      "expected 2 spaces but got 1  (tokenize.test 2:1)\n" +
      "   bar" + end2 + "\n" +
      "  ^"),

    test_crash(start2 + "foo\n  " + start2 + "bar\n   qux" + end2 + end2,
      "expected 4 spaces but got 3  (tokenize.test 3:1)\n" +
      "     qux" + end2 + end2 + "\n" +
      "  ^--"),

    test_crash(start2 + "foo " + start2 + "bar\n        " + space + "qux" + end2 + end2,
      "expected " + (l + 6) + " spaces but got " + (l + 7) + "  (tokenize.test 2:1)\n" +
      "          " + space + "qux" + end2 + end2 + "\n" +
      "  ^-------" + (space ? "-" : "")),

    test(start2 + "foo " + start2 + "bar\n       " + space + "qux" + end2 + end2, (loc) => [
      symbol(start, loc(0, 0,
                        0, 1)),
      symbol("foo", loc(0, l,
                        0, l + 3)),
      symbol(start, loc(0, l + 3 + 1,
                        0, l + 3 + 2)),
      symbol("bar", loc(0, l + 3 + 1 + l,
                        0, l + 3 + 1 + l + 3)),
      symbol("qux", loc(1, l + 6,
                        1, l + 6 + 3)),
      symbol(end, loc(1, l + 6 + l + 2,
                      1, l + 6 + l + 3)),
      symbol(end, loc(1, l + 6 + l + 2 + l,
                      1, l + 6 + l + 3 + l))
    ]),

    test_crash(start2 + "foo\n    bar" + end2,
      "expected 2 spaces but got 4  (tokenize.test 2:1)\n" +
      "      bar" + end2 + "\n" +
      "  ^---"),


    test_crash("0" + start + end + "1",
      "cannot have an expression on the left side of " + start + "  (tokenize.test 1:2)\n" +
      "  0" + start + end + "1\n" +
      "   ^"),

    test_crash("0" + start + end + " 1",
      "cannot have an expression on the left side of " + start + "  (tokenize.test 1:2)\n" +
      "  0" + start + end + " 1\n" +
      "   ^"),

    test_crash("0 " + start + end + "1",
      "cannot have an expression on the right side of " + end + "  (tokenize.test 1:4)\n" +
      "  0 " + start + end + "1\n" +
      "     ^"),


    test("&" + start2 + "1" + end2, (loc) => [
      symbol("&", loc(0, 0,
                      0, 1)),
      symbol(start, loc(0, 1,
                        0, 2)),
      integer(1, loc(0, l + 1,
                     0, l + 2)),
      symbol(end, loc(0, l + l + 1,
                      0, l + l + 2))
    ]),

    test(start2 + start2 + "1" + end2 + end2, (loc) => [
      symbol(start, loc(0, 0,
                        0, 1)),
      symbol(start, loc(0, l,
                        0, l + 1)),
      integer(1, loc(0, l + l,
                     0, l + l + 1)),
      symbol(end, loc(0, l + l + l,
                      0, l + l + l + 1)),
      symbol(end, loc(0, l + l + l + l,
                      0, l + l + l + l + 1))
    ]),

    test("0 " + start + end + " 1", (loc) => [
      integer(0, loc(0, 0,
                     0, 1)),
      symbol(start, loc(0, 2,
                        0, 3)),
      symbol(end, loc(0, 3,
                      0, 4)),
      integer(1, loc(0, 5,
                     0, 6))
    ]),


    ...(space ? [
      test_crash("0 " + start + "1",
        "must have a space or " + end + " on the right side of " + start + "  (tokenize.test 1:3)\n" +
        "  0 " + start + "1\n" +
        "    ^"),

      test_crash("0 " + start2 + "foo" + end + "1",
        "must have a space or " + start + " on the left side of " + end + "  (tokenize.test 1:8)\n" +
        "  0 " + start2 + "foo" + end + "1\n" +
        "         ^")
    ] : [
      test_crash("0 " + start + " 1",
        "spaces (U+0020) are not allowed after " + start + "  (tokenize.test 1:3)\n" +
        "  0 " + start + " 1\n" +
        "    ^"),

      test_crash("0 " + start + "foo " + end + "1",
        "spaces (U+0020) are not allowed before " + end + "  (tokenize.test 1:8)\n" +
        "  0 " + start + "foo " + end + "1\n" +
        "         ^")
    ]),


    test_crash("0 " + start,
      "missing expression on the right side of " + start + "  (tokenize.test 1:3)\n" +
      "  0 " + start + "\n" +
      "    ^"),

    test_crash("0 " + start + "\n",
      "missing expression on the right side of " + start + "  (tokenize.test 1:3)\n" +
      "  0 " + start + "\n" +
      "    ^")
  ];
};


export default [
  ...test_prefix("&"),
  ...test_prefix("~"),
  ...test_prefix("@"),

  //...test_infix("."),

  ...test_brackets("(", ")", false),
  ...test_brackets("[", "]", true),
  ...test_brackets("{", "}", true),


  test("", (loc) =>
    []),

  test("\n\n\n\n\n", (loc) =>
    []),


  test_crash("foo  bar",
    "expected 1 space but got 2  (tokenize.test 1:4)\n" +
    "  foo  bar\n" +
    "     ^-"),

  test_crash("foo     bar",
    "expected 1 space but got 5  (tokenize.test 1:4)\n" +
    "  foo     bar\n" +
    "     ^----"),

  test("foo bar", (loc) => [
    symbol("foo", loc(0, 0,
                      0, 3)),
    symbol("bar", loc(0, 4,
                      0, 7))
  ]),


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

  test_crash("\n foo\n   bar\n",
    "expected 0 spaces but got 1  (tokenize.test 2:1)\n" +
    "   foo\n" +
    "  ^"),

  test_crash("\n   foo\n   bar\n",
    "expected 0 spaces but got 3  (tokenize.test 2:1)\n" +
    "     foo\n" +
    "  ^--"),


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
    number(0, loc(0, 0,
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


  test("foo bar", (loc) => [
    symbol("foo", loc(0, 0,
                      0, 3)),
    symbol("bar", loc(0, 4,
                      0, 7))
  ]),

  test("&foo &bar", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("&", loc(0, 5,
                    0, 6)),
    symbol("bar", loc(0, 6,
                      0, 9))
  ]),

  test("~foo ~bar", (loc) => [
    symbol("~", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("~", loc(0, 5,
                    0, 6)),
    symbol("bar", loc(0, 6,
                      0, 9))
  ]),

  test("@foo @bar", (loc) => [
    symbol("@", loc(0, 0,
                    0, 1)),
    symbol("foo", loc(0, 1,
                      0, 4)),
    symbol("@", loc(0, 5,
                    0, 6)),
    symbol("bar", loc(0, 6,
                      0, 9))
  ]),

  test("foo.bar", (loc) => [
    symbol("foo.bar", loc(0, 0,
                          0, 7))
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

  test_crash("foo   ",
    "spaces (U+0020) are not allowed at the end of the line  (tokenize.test 1:4)\n" +
    "  foo   \n" +
    "     ^--"),

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


  test_crash("\n\t   \n",
    "tabs (U+0009) are not allowed  (tokenize.test 2:1)\n" +
    "  \t   \n" +
    "  ^"),

  test_crash("\nfoo\t   \n",
    "tabs (U+0009) are not allowed  (tokenize.test 2:4)\n" +
    "  foo\t   \n" +
    "     ^"),

  test_crash("\n\t\t\t\t\t   \n",
    "tabs (U+0009) are not allowed  (tokenize.test 2:1)\n" +
    "  \t\t\t\t\t   \n" +
    "  ^----"),


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

  test_crash("&&\"\nfoo\"",
    "there must be 3 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "  foo\"\n" +
    "  ^"),

  test_crash("&&\"\n foo\"",
    "there must be 3 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "   foo\"\n" +
    "  ^"),

  test_crash("&&\"\n  foo\"",
    "there must be 3 or more spaces (U+0020)  (tokenize.test 2:1)\n" +
    "    foo\"\n" +
    "  ^-"),

  test("&&\"\n   foo\"", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    text("\nfoo", loc(0, 2,
                      1, 7))
  ]),

  test("foo \"\n     bar\"", (loc) => [
    symbol("foo", loc(0, 0,
                      0, 3)),
    text("\nbar", loc(0, 4,
                      1, 9))
  ]),

  test_crash("&&\"\n\n\n   foo\n  bar\"",
    "there must be 3 or more spaces (U+0020)  (tokenize.test 5:1)\n" +
    "    bar\"\n" +
    "  ^-"),

  test("&&\"\n\n\n   foo\n   bar\"", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    text("\n\n\nfoo\nbar", loc(0, 2,
                               4, 7))
  ]),

  test_crash("&&\" foo\n       bar\n \n\n     qux\"",
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

  test("&&\"foo\n   bar\"", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    text("foo\nbar", loc(0, 2,
                         1, 7))
  ]),

  test("&&\" foo\n     bar\"", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    text(" foo\n  bar", loc(0, 2,
                            1, 9))
  ]),

  test("&&\" foo\n     bar\n\n\n   qux\"", (loc) => [
    symbol("&", loc(0, 0,
                    0, 1)),
    symbol("&", loc(0, 1,
                    0, 2)),
    text(" foo\n  bar\n\n\nqux", loc(0, 2,
                                     4, 7))
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


  test("1 1.5 1 ~5 (foo bar qux) u @q\nqux\n(nou)\n~foo\n~@foo", (loc) => [
    integer(1, loc(0, 0,
                   0, 1)),
    number(1.5, loc(0, 2,
                    0, 5)),
    integer(1, loc(0, 6,
                   0, 7)),
    symbol("~", loc(0, 8,
                    0, 9)),
    integer(5, loc(0, 9,
                   0, 10)),
    symbol("(", loc(0, 11,
                    0, 12)),
    symbol("foo", loc(0, 12,
                      0, 15)),
    symbol("bar", loc(0, 16,
                      0, 19)),
    symbol("qux", loc(0, 20,
                      0, 23)),
    symbol(")", loc(0, 23,
                    0, 24)),
    symbol("u", loc(0, 25,
                    0, 26)),
    symbol("@", loc(0, 27,
                    0, 28)),
    symbol("q", loc(0, 28,
                    0, 29)),
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
    symbol("foo", loc(3, 1,
                      3, 4)),
    symbol("~", loc(4, 0,
                    4, 1)),
    symbol("@", loc(4, 1,
                    4, 2)),
    symbol("foo", loc(4, 2,
                      4, 5))
  ]),
];
