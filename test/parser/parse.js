import { expect, expect_crash } from "../assert";
import { wrap, catch_error } from "../../ffi/task";
import { tokenize } from "../../src/parser/tokenize";
import { parse } from "../../src/parser/parse";
import { lines } from "../../util/string";
import { string, symbol, integer,
         call, list, record, lambda,
         dot } from "../../src/parser/ast";


const file = "parse.test";

const test = (input, f) => {
  const x = lines(input);
  return expect(f(file, x),
           wrap(parse(tokenize(x, file))));
};

const test_crash = (input, expected) =>
  expect_crash("Error: " + expected,
    catch_error(() => {
      const x = lines(input);
      return parse(tokenize(x, file));
    }));


const test_brackets = (start, end, make) =>
  [
    test_crash("foo" + end,
      "missing starting " + start + "  (parse.test 1:4)\n" +
      "  foo" + end + "\n" +
      "     ^"),

    test_crash(start + "foo",
      "missing ending " + end + "  (parse.test 1:1)\n" +
      "  " + start + "foo\n" +
      "  ^"),

    test_crash("foo" + end + "bar" + end,
      "missing starting " + start + "  (parse.test 1:4)\n" +
      "  foo" + end + "bar" + end + "\n" +
      "     ^"),

    test_crash(start + "foo " + start + "bar",
      "missing ending " + end + "  (parse.test 1:6)\n" +
      "  " + start + "foo " + start + "bar\n" +
      "       ^"),

    test(start + "foo bar qux" + end, (file, lines) => [
      make([symbol("foo", file, lines,
                   { line: 0, column: 1 },
                   { line: 0, column: 4 }),
            symbol("bar", file, lines,
                   { line: 0, column: 5 },
                   { line: 0, column: 8 }),
            symbol("qux", file, lines,
                   { line: 0, column: 9 },
                   { line: 0, column: 12 })], file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 13 })
    ]),

    test(start + start + "foo" + end + " " +
                 start + "bar" + end + " " +
                 start + "qux" + end + end, (file, lines) => [
      make([make([symbol("foo", file, lines,
                         { line: 0, column: 2 },
                         { line: 0, column: 5 })], file, lines,
                 { line: 0, column: 1 },
                 { line: 0, column: 6 }),
            make([symbol("bar", file, lines,
                         { line: 0, column: 8 },
                         { line: 0, column: 11 })], file, lines,
                 { line: 0, column: 7 },
                 { line: 0, column: 12 }),
            make([symbol("qux", file, lines,
                         { line: 0, column: 14 },
                         { line: 0, column: 17 })], file, lines,
                 { line: 0, column: 13 },
                 { line: 0, column: 18 })], file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 19 })
    ])
  ];


export default [
  ...test_brackets("(", ")", call),
  ...test_brackets("[", "]", list),
  ...test_brackets("{", "}", record),


  test("{ hi }", (file, lines) => [
    record([symbol("hi", file, lines,
                    { line: 0, column: 2 },
                    { line: 0, column: 4 })], file, lines,
            { line: 0, column: 0 },
            { line: 0, column: 6 })
  ]),

  test("-> 1 2 3 4 5", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 }),
            integer(2, file, lines,
                    { line: 0, column: 5 },
                    { line: 0, column: 6 }),
            integer(3, file, lines,
                    { line: 0, column: 7 },
                    { line: 0, column: 8 }),
            integer(4, file, lines,
                    { line: 0, column: 9 },
                    { line: 0, column: 10 })],
           integer(5, file, lines,
                   { line: 0, column: 11 },
                   { line: 0, column: 12 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 12 })
  ]),

  /*test("(-> 1 2 3 4 5)", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 4 },
                    { line: 0, column: 5 }),
            integer(2, file, lines,
                    { line: 0, column: 6 },
                    { line: 0, column: 7 }),
            integer(3, file, lines,
                    { line: 0, column: 8 },
                    { line: 0, column: 9 }),
            integer(4, file, lines,
                    { line: 0, column: 10 },
                    { line: 0, column: 11 })],
           integer(5, file, lines,
                   { line: 0, column: 12 },
                   { line: 0, column: 13 }), file, lines,
           { line: 0, column: 1 },
           { line: 0, column: 13 })
  ]),*/

  test_crash("(-> 1)",
    "functions must have at least one argument  (parse.test 1:1)\n" +
    "  (-> 1)\n" +
    "  ^-----"),


  /*test("a.b", (file, lines) => [
    dot(symbol("a", file, lines,
               { line: 0, column: 0 },
               { line: 0, column: 1 }),
        symbol("b", file, lines,
               { line: 0, column: 2 },
               { line: 0, column: 3 }),
        file, lines,
        { line: 0, column: 0 },
        { line: 0, column: 3 })
  ]),*/
];
