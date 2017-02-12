import { expect, expect_crash } from "../assert";
import { reply } from "../../builtin/ffi/task";
import { tokenize } from "../../src/parser/tokenize";
import { parse } from "../../src/parser/parse";
import { lines, repeat } from "../../util/string";
import { loc, text, symbol, integer,
         call, list, record, lambda,
         dot, bar, assign, type,
         quote, unquote, splice, number,
         wildcard } from "../../src/parser/type";


const file = "parse.test";

const test = (input, f) => {
  const x = lines(input);

  const _loc = (line1, column1, line2, column2) =>
    loc(file, x,
        { line: line1, column: column1 },
        { line: line2, column: column2 });

  return expect(f(_loc),
           reply(parse(tokenize(x, file))));
};

const test_crash = (input, expected) =>
  expect_crash("Error: " + expected, () => {
    const x = lines(input);
    return reply(parse(tokenize(x, file)));
  });

const marker = (name) =>
  "^" + repeat("-", name["length"] - 1);


const test_prefix = (name, make, space) => {
  const name2 = (space
                  ? name + " "
                  : name);

  return [
    test_crash(name,
      "missing expression on the right side of " + name + "  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  ^"),

    test_crash(name + "\n",
      "missing expression on the right side of " + name + "  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  ^"),

    test(name2 + "1", (loc) => [
      make(integer(1, loc(0, name2["length"],
                          0, name2["length"] + 1)),
           loc(0, 0,
               0, name2["length"] + 1))
    ]),

    test(name2 + name2 + "1", (loc) => [
      make(make(integer(1, loc(0, name2["length"] + name2["length"],
                               0, name2["length"] + name2["length"] + 1)),
                loc(0, name2["length"],
                    0, name2["length"] + name2["length"] + 1)),
           loc(0, 0,
               0, name2["length"] + name2["length"] + 1))
    ]),

    test("(a\n" +
         "  " + name2 + "b\n" +
         "  " + name2 + "(c d))", (loc) => [
      call([symbol("a", loc(0, 1,
                            0, 2)),
            make(symbol("b", loc(1, name2["length"] + 2,
                                 1, name2["length"] + 3)),
                 loc(1, 2,
                     1, name2["length"] + 3)),
            make(call([symbol("c", loc(2, name2["length"] + 3,
                                       2, name2["length"] + 4)),
                       symbol("d", loc(2, name2["length"] + 5,
                                       2, name2["length"] + 6))],
                      loc(2, name2["length"] + 2,
                          2, name2["length"] + 7)),
                 loc(2, 2,
                     2, name2["length"] + 7))],
           loc(0, 0,
               2, name2["length"] + 8))
    ])
  ];
};

const test_infix = (name, make, { right_associative, space }) => {
  const name2 = (space
                  ? " " + name + " "
                  : name);
  const test2 = (space
                  ? name + " b"
                  : name + "b");
  const test3 = (space
                  ? "a " + name
                  : "a" + name);
  return [
    test_crash(name,
      "missing expression on the left side of " + name + "  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  " + marker(name)),

    test_crash(test2,
      "missing expression on the left side of " + name + "  (parse.test 1:1)\n" +
      "  " + test2 + "\n" +
      "  " + marker(name)),

    test_crash(test3,
      "missing expression on the right side of " + name + "  (parse.test 1:" + (space ? "3" : "2") + ")\n" +
      "  " + test3 + "\n" +
      (space
        ? "    " + marker(name)
        : "   " + marker(name))),

    test("a" + name2 + "b", (loc) => [
      make(symbol("a", loc(0, 0,
                           0, 1)),
           symbol("b", loc(0, name2["length"] + 1,
                           0, name2["length"] + 2)),
           loc(0, 0,
               0, name2["length"] + 2))
    ]),

    (right_associative
      ? test("a" + name2 + "b" + name2 + "c", (loc) => [
          make(symbol("a", loc(0, 0,
                               0, 1)),
               make(symbol("b", loc(0, name2["length"] + 1,
                                    0, name2["length"] + 2)),
                    symbol("c", loc(0, name2["length"] + name2["length"] + 2,
                                    0, name2["length"] + name2["length"] + 3)),
                    loc(0, name2["length"] + 1,
                        0, name2["length"] + name2["length"] + 3)),
               loc(0, 0,
                   0, name2["length"] + name2["length"] + 3))
        ])
      : test("a" + name2 + "b" + name2 + "c", (loc) => [
          make(make(symbol("a", loc(0, 0,
                                    0, 1)),
                    symbol("b", loc(0, name2["length"] + 1,
                                    0, name2["length"] + 2)),
                    loc(0, 0,
                        0, name2["length"] + 2)),
               symbol("c", loc(0, name2["length"] + name2["length"] + 2,
                               0, name2["length"] + name2["length"] + 3)),
               loc(0, 0,
                   0, name2["length"] + name2["length"] + 3))
        ])),
  ];
};

const test_brackets = (start, end, make, space) => {
  const start2 = (space
                   ? start + " "
                   : start);
  const end2 = (space
                 ? " " + end
                 : end);
  const marker = (space
                   ? " "
                   : "");

  const startl = start2["length"];

  const line = (i, x) => (space ? i + x : i);

  return [
    test_crash("foo" + end2,
      "missing starting " + start + "  (parse.test 1:" + line(4, 1) + ")\n" +
      "  foo" + end2 + "\n" +
      "     " + marker + "^"),

    test_crash(start2 + "foo",
      "missing ending " + end + "  (parse.test 1:1)\n" +
      "  " + start2 + "foo\n" +
      "  ^"),

    test_crash(start2 + "#foo",
      "missing ending " + end + "  (parse.test 1:1)\n" +
      "  " + start2 + "#foo\n" +
      "  ^"),

    test_crash("foo" + end2 + " bar" + end2,
      "missing starting " + start + "  (parse.test 1:" + line(4, 1) + ")\n" +
      "  foo" + end2 + " bar" + end2 + "\n" +
      "     " + marker + "^"),

    test_crash(start2 + "foo " + start2 + "bar",
      "missing ending " + end + "  (parse.test 1:" + line(6, 1) + ")\n" +
      "  " + start2 + "foo " + start2 + "bar\n" +
      "       " + marker + "^"),

    test(start2 + "foo\n  bar" + end2, (loc) => [
      make([symbol("foo", loc(0, startl,
                              0, startl + 3)),
            symbol("bar", loc(1, 2,
                              1, 5))],
           loc(0, 0,
               1, startl + 5))
    ]),

    test(start2 + "foo\n  bar " + start2 + "qux\n        corge" + end2 + end2, (loc) => [
      make([symbol("foo", loc(0, startl,
                              0, startl + 3)),
            symbol("bar", loc(1, 2,
                              1, 5)),
            make([symbol("qux", loc(1, startl + 6,
                                    1, startl + 9)),
                  symbol("corge", loc(2, 8,
                                      2, 13))],
                 loc(1, 6,
                     2, 13 + startl))],
           loc(0, 0,
               2, 13 + startl + startl))
    ]),

    test(start + end, (loc) => [
      make([], loc(0, 0,
                   0, 2))
    ]),

    test(start2 + "foo bar qux" + end2, (loc) => [
      make([symbol("foo", loc(0, startl,
                              0, startl + 3)),
            symbol("bar", loc(0, startl + 4,
                              0, startl + 7)),
            symbol("qux", loc(0, startl + 8,
                              0, startl + 11))],
           loc(0, 0,
               0, startl + startl + 11))
    ]),

    test(start2 + start2 + "foo" + end2 + " " +
                  start2 + "bar" + end2 + " " +
                  start2 + "qux" + end2 + end2, (loc) => [
      make([make([symbol("foo", loc(0, startl + startl,
                                    0, startl + startl + 3))],
                 loc(0, startl,
                     0, startl + startl + startl + 3)),
            make([symbol("bar", loc(0, startl + startl + 3 + startl + 1 + startl,
                                    0, startl + startl + 3 + startl + 1 + startl + 3))],
                 loc(0, startl + startl + 3 + startl + 1,
                     0, startl + startl + 3 + startl + 1 + startl + 3 + startl)),
            make([symbol("qux", loc(0, startl + startl + 3 + startl + 1 + startl + 3 + startl + 1 + startl,
                                    0, startl + startl + 3 + startl + 1 + startl + 3 + startl + 1 + startl + 3))],
                 loc(0, startl + startl + 3 + startl + 1 + startl + 3 + startl + 1,
                     0, startl + startl + 3 + startl + 1 + startl + 3 + startl + 1 + startl + 3 + startl))],
           loc(0, 0,
               0, startl + startl + 3 + startl + 1 + startl + 3 + startl + 1 + startl + 3 + startl + startl))
    ])
  ];
};


export default [
  ...test_brackets("(", ")", call, false),
  ...test_brackets("[", "]", list, true),
  ...test_brackets("{", "}", record, true),

  ...test_prefix("&", quote, false),
  ...test_prefix("~", unquote, false),
  ...test_prefix("@", splice, false),

  ...test_infix(":", bar, { right_associative: true, space: true }),
  ...test_infix("<=", assign, { right_associative: true, space: true }),
  //...test_infix(".", dot, { right_associative: false, space: false }),
  ...test_infix("::", type, { right_associative: true, space: true }),


  test("{ hi }", (loc) => [
    record([symbol("hi", loc(0, 2,
                             0, 4))],
           loc(0, 0,
               0, 6))
  ]),

  test("{ foo <= 1 bar <= 2 }", (loc) => [
    record([assign(symbol("foo", loc(0, 2,
                                     0, 5)),
                   integer(1, loc(0, 9,
                                  0, 10)),
                   loc(0, 2,
                       0, 10)),
            assign(symbol("bar", loc(0, 11,
                                     0, 14)),
                   integer(2, loc(0, 18,
                                  0, 19)),
                   loc(0, 11,
                       0, 19))],
           loc(0, 0,
               0, 21))
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


  /*test("a.b", (loc) => [
    dot(symbol("a", loc(0, 0,
                        0, 1)),
        symbol("b", loc(0, 2,
                        0, 3)),
        loc(0, 0,
            0, 3))
  ]),*/

  test("0.1", (loc) => [
    number(0.1, loc(0, 0,
                    0, 3))
  ]),

  /*test("a.b <= c.d", (loc) => [
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
  ]),*/


  /*test("&a.~b", (loc) => [
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
  ]),*/


  test("(REWRITE-RULE\n  (IF ~test\n    ~then\n    ~else)\n  : &(MATCH ~test\n       *true\n       : ~then\n       *false\n       : ~else))", (loc) => [
    call([symbol("REWRITE-RULE", loc(0, 1,
                                     0, 13)),
          bar(call([symbol("IF", loc(1, 3,
                                     1, 5)),
                    unquote(symbol("test", loc(1, 7,
                                               1, 11)),
                            loc(1, 6,
                                1, 11)),
                    unquote(symbol("then", loc(2, 5,
                                               2, 9)),
                            loc(2, 4,
                                2, 9)),
                    unquote(symbol("else", loc(3, 5,
                                               3, 9)),
                            loc(3, 4,
                                3, 9))],
                   loc(1, 2,
                       3, 10)),
              quote(call([symbol("MATCH", loc(4, 6,
                                              4, 11)),
                          unquote(symbol("test", loc(4, 13,
                                                     4, 17)),
                                  loc(4, 12,
                                      4, 17)),
                          bar(symbol("*true", loc(5, 7,
                                                  5, 12)),
                              unquote(symbol("then", loc(6, 10,
                                                         6, 14)),
                                      loc(6, 9,
                                          6, 14)),
                              loc(5, 7,
                                  6, 14)),
                          bar(symbol("*false", loc(7, 7,
                                                   7, 13)),
                              unquote(symbol("else", loc(8, 10,
                                                         8, 14)),
                                      loc(8, 9,
                                          8, 14)),
                              loc(7, 7,
                                  8, 14))],
                         loc(4, 5,
                             8, 15)),
                    loc(4, 4,
                        8, 15)),
              loc(1, 2,
                  8, 15))],
         loc(0, 0,
             8, 16))
  ]),


  test("&(CHAIN-RESULT _ :: Null <= ~a ~b)", (loc) => [
    quote(call([symbol("CHAIN-RESULT", loc(0, 2,
                                           0, 14)),
                type(wildcard(loc(0, 15,
                                  0, 16)),
                     assign(symbol("Null", loc(0, 20,
                                               0, 24)),
                            unquote(symbol("a", loc(0, 29,
                                                    0, 30)),
                                    loc(0, 28,
                                        0, 30)),
                            loc(0, 20,
                                0, 30)),
                     loc(0, 15,
                         0, 30)),
                unquote(symbol("b", loc(0, 32,
                                        0, 33)),
                        loc(0, 31,
                            0, 33))],
               loc(0, 1,
                   0, 34)),
          loc(0, 0,
              0, 34))
  ]),


  test("a : b :: c <= d", (loc) => [
    bar(symbol("a", loc(0, 0,
                        0, 1)),
        type(symbol("b", loc(0, 4,
                             0, 5)),
             assign(symbol("c", loc(0, 9,
                                    0, 10)),
                    symbol("d", loc(0, 14,
                                    0, 15)),
                    loc(0, 9,
                        0, 15)),
             loc(0, 4,
                 0, 15)),
        loc(0, 0,
            0, 15))
  ]),
];
