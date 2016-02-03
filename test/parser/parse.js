import { expect, expect_crash } from "../assert";
import { wrap, catch_error } from "../../ffi/task";
import { tokenize } from "../../src/parser/tokenize";
import { parse } from "../../src/parser/parse";
import { lines, repeat } from "../../util/string";
import { loc, text, symbol, integer,
         call, list, record, lambda,
         dot, bar, assign, type,
         quote, unquote, splice } from "../../src/parser/ast";


const file = "parse.test";

const test = (input, f) => {
  const x = lines(input);

  const _loc = (line1, column1, line2, column2) =>
    loc(file, x,
        { line: line1, column: column1 },
        { line: line2, column: column2 });

  return expect(f(_loc),
           wrap(parse(tokenize(x, file))));
};

const test_crash = (input, expected) =>
  expect_crash("Error: " + expected,
    catch_error(() => {
      const x = lines(input);
      return parse(tokenize(x, file));
    }));

const marker = (name) =>
  "^" + repeat("-", name["length"] - 1);


const test_prefix = (name, make) =>
  [
    test_crash(name,
      "missing expression on the right side  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  ^"),

    test(name + " 1", (loc) => [
      make(integer(1, loc(0, name["length"] + 1,
                          0, name["length"] + 2)),
           loc(0, 0,
               0, name["length"] + 2))
    ]),

    test(name + " " + name + " 1", (loc) => [
      make(make(integer(1, loc(0, name["length"] + name["length"] + 2,
                               0, name["length"] + name["length"] + 3)),
                loc(0, name["length"] + 1,
                    0, name["length"] + name["length"] + 3)),
           loc(0, 0,
               0, name["length"] + name["length"] + 3))
    ]),

    test("(a\n" +
         "  " + name + " b\n" +
         "  " + name + " (c d))", (loc) => [
      call([symbol("a", loc(0, 1,
                            0, 2)),
            make(symbol("b", loc(1, name["length"] + 3,
                                 1, name["length"] + 4)),
                 loc(1, 2,
                     1, name["length"] + 4)),
            make(call([symbol("c", loc(2, name["length"] + 4,
                                       2, name["length"] + 5)),
                       symbol("d", loc(2, name["length"] + 6,
                                       2, name["length"] + 7))],
                      loc(2, name["length"] + 3,
                          2, name["length"] + 8)),
                 loc(2, 2,
                     2, name["length"] + 8))],
           loc(0, 0,
               2, name["length"] + 9))
    ])
  ];

const test_infix = (name, make, right_associative) =>
  [
    test_crash(name,
      "missing expression on the left side  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  " + marker(name)),

    test_crash(name + " 2",
      "missing expression on the left side  (parse.test 1:1)\n" +
      "  " + name + " 2\n" +
      "  " + marker(name)),

    test_crash("1 " + name,
      "missing expression on the right side  (parse.test 1:3)\n" +
      "  1 " + name + "\n" +
      "    " + marker(name)),

    test("1 " + name + " 2", (loc) => [
      make(integer(1, loc(0, 0,
                          0, 1)),
           integer(2, loc(0, name["length"] + 3,
                          0, name["length"] + 4)),
           loc(0, 0,
               0, name["length"] + 4))
    ]),

    (right_associative
      ? test("1 " + name + " 2 " + name + " 3", (loc) => [
          make(integer(1, loc(0, 0,
                              0, 1)),
               make(integer(2, loc(0, name["length"] + 3,
                                   0, name["length"] + 4)),
                    integer(3, loc(0, name["length"] + name["length"] + 6,
                                   0, name["length"] + name["length"] + 7)),
                    loc(0, name["length"] + 3,
                        0, name["length"] + name["length"] + 7)),
               loc(0, 0,
                   0, name["length"] + name["length"] + 7))
        ])
      : test("1 " + name + " 2 " + name + " 3", (loc) => [
          make(make(integer(1, loc(0, 0,
                                   0, 1)),
                    integer(2, loc(0, name["length"] + 3,
                                   0, name["length"] + 4)),
                    loc(0, 0,
                        0, name["length"] + 4)),
               integer(3, loc(0, name["length"] + name["length"] + 6,
                              0, name["length"] + name["length"] + 7)),
               loc(0, 0,
                   0, name["length"] + name["length"] + 7))
        ])),
  ];

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

    test(start + end, (loc) => [
      make([], loc(0, 0,
                   0, 2))
    ]),

    test(start + "foo bar qux" + end, (loc) => [
      make([symbol("foo", loc(0, 1,
                              0, 4)),
            symbol("bar", loc(0, 5,
                              0, 8)),
            symbol("qux", loc(0, 9,
                              0, 12))],
           loc(0, 0,
               0, 13))
    ]),

    test(start + start + "foo" + end + " " +
                 start + "bar" + end + " " +
                 start + "qux" + end + end, (loc) => [
      make([make([symbol("foo", loc(0, 2,
                                    0, 5))],
                 loc(0, 1,
                     0, 6)),
            make([symbol("bar", loc(0, 8,
                                    0, 11))],
                 loc(0, 7,
                     0, 12)),
            make([symbol("qux", loc(0, 14,
                                    0, 17))],
                 loc(0, 13,
                     0, 18))],
           loc(0, 0,
               0, 19))
    ])
  ];


export default [
  ...test_brackets("(", ")", call),
  ...test_brackets("[", "]", list),
  ...test_brackets("{", "}", record),

  ...test_prefix("|", bar),
  ...test_prefix("&", quote),
  ...test_prefix("~", unquote),
  ...test_prefix("@", splice),

  ...test_infix("<=", assign, true),
  ...test_infix(".", dot, false),
  ...test_infix("::", type, false),


  test("{ hi }", (loc) => [
    record([symbol("hi", loc(0, 2,
                             0, 4))],
           loc(0, 0,
               0, 6))
  ]),

  test("{ foo <= 1 | bar <= 2 }", (loc) => [
    record([assign(symbol("foo", loc(0, 2,
                                     0, 5)),
                   integer(1, loc(0, 9,
                                  0, 10)),
                   loc(0, 2,
                       0, 10)),
            bar(assign(symbol("bar", loc(0, 13,
                                         0, 16)),
                       integer(2, loc(0, 20,
                                      0, 21)),
                       loc(0, 13,
                           0, 21)),
                loc(0, 11,
                    0, 21))],
           loc(0, 0,
               0, 23))
  ]),


  test("-> 1 2 3 4 5", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4)),
            integer(2, loc(0, 5,
                           0, 6)),
            integer(3, loc(0, 7,
                           0, 8)),
            integer(4, loc(0, 9,
                           0, 10))],
           integer(5, loc(0, 11,
                          0, 12)),
           loc(0, 0,
               0, 12))
  ]),

  test("(-> 1 2 3 4 5)", (loc) => [
    call([lambda([integer(1, loc(0, 4,
                                 0, 5)),
                  integer(2, loc(0, 6,
                                 0, 7)),
                  integer(3, loc(0, 8,
                                 0, 9)),
                  integer(4, loc(0, 10,
                                 0, 11))],
                 integer(5, loc(0, 12,
                                0, 13)),
                 loc(0, 1,
                     0, 13))],
         loc(0, 0,
             0, 14))
  ]),

  test_crash("->",
    "functions must have at least 1 parameter  (parse.test 1:1)\n" +
    "  ->\n" +
    "  ^-"),

  test_crash("-> 1",
    "functions must have at least 1 parameter  (parse.test 1:1)\n" +
    "  -> 1\n" +
    "  ^-"),

  test("-> 1 2", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4))],
           integer(2, loc(0, 5,
                          0, 6)),
           loc(0, 0,
               0, 6))
  ]),

  test("-> 1 -> 2 3", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4))],
           lambda([integer(2, loc(0, 8,
                                  0, 9))],
                  integer(3, loc(0, 10,
                                 0, 11)),
                  loc(0, 5,
                      0, 11)),
           loc(0, 0,
               0, 11))
  ]),

  test("-> 1 -> 2 3 <= 4", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4))],
           lambda([integer(2, loc(0, 8,
                                  0, 9))],
                  assign(integer(3, loc(0, 10,
                                        0, 11)),
                         integer(4, loc(0, 15,
                                        0, 16)),
                         loc(0, 10,
                             0, 16)),
                  loc(0, 5,
                      0, 16)),
           loc(0, 0,
               0, 16))
  ]),

  test("-> 1 2 <= -> 3 4 <= 5", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4))],
           assign(integer(2, loc(0, 5,
                                 0, 6)),
                  lambda([integer(3, loc(0, 13,
                                         0, 14))],
                         assign(integer(4, loc(0, 15,
                                               0, 16)),
                                integer(5, loc(0, 20,
                                               0, 21)),
                                loc(0, 15,
                                    0, 21)),
                         loc(0, 10,
                             0, 21)),
                   loc(0, 5,
                       0, 21)),
           loc(0, 0,
               0, 21))
  ]),

  test("-> 1 &-> 2 ~@3", (loc) => [
    lambda([integer(1, loc(0, 3,
                           0, 4))],
           quote(lambda([integer(2, loc(0, 9,
                                        0, 10))],
                        unquote(splice(integer(3, loc(0, 13,
                                                      0, 14)),
                                       loc(0, 12,
                                           0, 14)),
                                loc(0, 11,
                                    0, 14)),
                        loc(0, 6,
                            0, 14)),
                 loc(0, 5,
                     0, 14)),
           loc(0, 0,
               0, 14))
  ]),

  test_crash("(->)",
    "functions must have at least 1 parameter  (parse.test 1:2)\n" +
    "  (->)\n" +
    "   ^-"),

  test_crash("(-> 1)",
    "functions must have at least 1 parameter  (parse.test 1:2)\n" +
    "  (-> 1)\n" +
    "   ^-"),


  test("a.b", (loc) => [
    dot(symbol("a", loc(0, 0,
                        0, 1)),
        symbol("b", loc(0, 2,
                        0, 3)),
        loc(0, 0,
            0, 3))
  ]),

  test("a.b <= c.d", (loc) => [
    assign(dot(symbol("a", loc(0, 0,
                               0, 1)),
               symbol("b", loc(0, 2,
                               0, 3)),
               loc(0, 0,
                   0, 3)),
           dot(symbol("c", loc(0, 7,
                               0, 8)),
               symbol("d", loc(0, 9,
                               0, 10)),
               loc(0, 7,
                   0, 10)),
           loc(0, 0,
               0, 10))
  ]),


  test("&a.~b", (loc) => [
    quote(dot(symbol("a", loc(0, 1,
                              0, 2)),
              unquote(symbol("b", loc(0, 4,
                                      0, 5)),
                      loc(0, 3,
                          0, 5)),
              loc(0, 1,
                  0, 5)),
          loc(0, 0,
              0, 5))
  ]),


  test("(REWRITE-RULE\n| (IF test &| ~then &| ~else)\n    &(MATCH ~test\n     | *true\n         ~then\n     | *false\n         ~else))", (loc) => [
    call([symbol("REWRITE-RULE", loc(0, 1,
                                     0, 13)),
          bar(call([symbol("IF", loc(1, 3,
                                     1, 5)),
                    symbol("test", loc(1, 6,
                                       1, 10)),
                    quote(bar(unquote(symbol("then", loc(1, 15,
                                                         1, 19)),
                                      loc(1, 14,
                                          1, 19)),
                              loc(1, 12,
                                  1, 19)),
                          loc(1, 11,
                              1, 19)),
                    quote(bar(unquote(symbol("else", loc(1, 24,
                                                         1, 28)),
                                      loc(1, 23,
                                          1, 28)),
                              loc(1, 21,
                                  1, 28)),
                          loc(1, 20,
                              1, 28))],
                   loc(1, 2,
                       1, 29)),
              loc(1, 0,
                  1, 29)),
          quote(call([symbol("MATCH", loc(2, 6,
                                          2, 11)),
                      unquote(symbol("test", loc(2, 13,
                                                 2, 17)),
                              loc(2, 12,
                                  2, 17)),
                      bar(symbol("*true", loc(3, 7,
                                              3, 12)),
                          loc(3, 5,
                              3, 12)),
                      unquote(symbol("then", loc(4, 10,
                                                 4, 14)),
                              loc(4, 9,
                                  4, 14)),
                      bar(symbol("*false", loc(5, 7,
                                               5, 13)),
                          loc(5, 5,
                              5, 13)),
                      unquote(symbol("else", loc(6, 10,
                                                 6, 14)),
                              loc(6, 9,
                                  6, 14))],
                     loc(2, 5,
                         6, 15)),
                loc(2, 4,
                    6, 15))],
         loc(0, 0,
             6, 16))
  ]),
];
