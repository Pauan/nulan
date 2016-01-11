import { expect, expect_crash } from "../assert";
import { wrap, catch_error } from "../../ffi/task";
import { tokenize } from "../../src/parser/tokenize";
import { parse } from "../../src/parser/parse";
import { lines, repeat } from "../../util/string";
import { string, symbol, integer,
         call, list, record, lambda,
         dot, bar, assign, type,
         quote, unquote, splice } from "../../src/parser/ast";


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

const marker = (name) =>
  "^" + repeat("-", name["length"] - 1);


const test_prefix = (name, make) =>
  [
    test_crash(name,
      "missing expression on the right side  (parse.test 1:1)\n" +
      "  " + name + "\n" +
      "  ^"),

    test(name + " 1", (file, lines) => [
      make(integer(1, file, lines,
                   { line: 0, column: name["length"] + 1 },
                   { line: 0, column: name["length"] + 2 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: name["length"] + 2 })
    ]),

    test(name + " " + name + " 1", (file, lines) => [
      make(make(integer(1, file, lines,
                        { line: 0, column: name["length"] + name["length"] + 2 },
                        { line: 0, column: name["length"] + name["length"] + 3 }), file, lines,
                { line: 0, column: name["length"] + 1 },
                { line: 0, column: name["length"] + name["length"] + 3 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: name["length"] + name["length"] + 3 })
    ]),

    test("(a\n" +
         "  " + name + " b\n" +
         "  " + name + " (c d))", (file, lines) => [
      call([symbol("a", file, lines,
                   { line: 0, column: 1 },
                   { line: 0, column: 2 }),
            make(symbol("b", file, lines,
                        { line: 1, column: name["length"] + 3 },
                        { line: 1, column: name["length"] + 4 }), file, lines,
                 { line: 1, column: 2 },
                 { line: 1, column: name["length"] + 4 }),
            make(call([symbol("c", file, lines,
                              { line: 2, column: name["length"] + 4 },
                              { line: 2, column: name["length"] + 5 }),
                       symbol("d", file, lines,
                              { line: 2, column: name["length"] + 6 },
                              { line: 2, column: name["length"] + 7 })], file, lines,
                      { line: 2, column: name["length"] + 3 },
                      { line: 2, column: name["length"] + 8 }), file, lines,
                 { line: 2, column: 2 },
                 { line: 2, column: name["length"] + 8 })], file, lines,
           { line: 0, column: 0 },
           { line: 2, column: name["length"] + 9 })
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

    test("1 " + name + " 2", (file, lines) => [
      make(integer(1, file, lines,
                   { line: 0, column: 0 },
                   { line: 0, column: 1 }),
           integer(2, file, lines,
                   { line: 0, column: name["length"] + 3 },
                   { line: 0, column: name["length"] + 4 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: name["length"] + 4 })
    ]),

    (right_associative
      ? test("1 " + name + " 2 " + name + " 3", (file, lines) => [
          make(integer(1, file, lines,
                       { line: 0, column: 0 },
                       { line: 0, column: 1 }),
               make(integer(2, file, lines,
                            { line: 0, column: name["length"] + 3 },
                            { line: 0, column: name["length"] + 4 }),
                    integer(3, file, lines,
                            { line: 0, column: name["length"] + name["length"] + 6 },
                            { line: 0, column: name["length"] + name["length"] + 7 }), file, lines,
                    { line: 0, column: name["length"] + 3 },
                    { line: 0, column: name["length"] + name["length"] + 7 }), file, lines,
               { line: 0, column: 0 },
               { line: 0, column: name["length"] + name["length"] + 7 })
        ])
      : test("1 " + name + " 2 " + name + " 3", (file, lines) => [
          make(make(integer(1, file, lines,
                            { line: 0, column: 0 },
                            { line: 0, column: 1 }),
                    integer(2, file, lines,
                            { line: 0, column: name["length"] + 3 },
                            { line: 0, column: name["length"] + 4 }), file, lines,
                    { line: 0, column: 0 },
                    { line: 0, column: name["length"] + 4 }),
               integer(3, file, lines,
                       { line: 0, column: name["length"] + name["length"] + 6 },
                       { line: 0, column: name["length"] + name["length"] + 7 }), file, lines,
               { line: 0, column: 0 },
               { line: 0, column: name["length"] + name["length"] + 7 })
        ])),
  ];

const test_brackets = (start, end, make, unwrap) =>
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

    test(start + end, (file, lines) => [
      make([], file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 2 })
    ]),

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

    (unwrap
      ? test(start + start + "foo" + end + " " +
                     start + "bar" + end + " " +
                     start + "qux" + end + end, (file, lines) => [
          make([symbol("foo", file, lines,
                       { line: 0, column: 2 },
                       { line: 0, column: 5 }),
                symbol("bar", file, lines,
                       { line: 0, column: 8 },
                       { line: 0, column: 11 }),
                symbol("qux", file, lines,
                       { line: 0, column: 14 },
                       { line: 0, column: 17 })], file, lines,
               { line: 0, column: 0 },
               { line: 0, column: 19 })
        ])
      : test(start + start + "foo" + end + " " +
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
        ]))
  ];


export default [
  ...test_brackets("(", ")", call, true),
  ...test_brackets("[", "]", list, false),
  ...test_brackets("{", "}", record, false),

  ...test_prefix("|", bar),
  ...test_prefix("&", quote),
  ...test_prefix("~", unquote),
  ...test_prefix("@", splice),

  ...test_infix("<=", assign, true),
  ...test_infix(".", dot, false),
  ...test_infix("::", type, false),


  test("{ hi }", (file, lines) => [
    record([symbol("hi", file, lines,
                    { line: 0, column: 2 },
                    { line: 0, column: 4 })], file, lines,
            { line: 0, column: 0 },
            { line: 0, column: 6 })
  ]),

  test("{ foo <= 1 | bar <= 2 }", (file, lines) => [
    record([assign(symbol("foo", file, lines,
                          { line: 0, column: 2 },
                          { line: 0, column: 5 }),
                   integer(1, file, lines,
                           { line: 0, column: 9 },
                           { line: 0, column: 10 }), file, lines,
                   { line: 0, column: 2 },
                   { line: 0, column: 10 }),
            bar(assign(symbol("bar", file, lines,
                              { line: 0, column: 13 },
                              { line: 0, column: 16 }),
                       integer(2, file, lines,
                               { line: 0, column: 20 },
                               { line: 0, column: 21 }), file, lines,
                       { line: 0, column: 13 },
                       { line: 0, column: 21 }), file, lines,
                { line: 0, column: 11 },
                { line: 0, column: 21 })], file, lines,
            { line: 0, column: 0 },
            { line: 0, column: 23 })
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

  test("(-> 1 2 3 4 5)", (file, lines) => [
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
  ]),

  test_crash("->",
    "functions must have at least 1 parameter  (parse.test 1:1)\n" +
    "  ->\n" +
    "  ^-"),

  test_crash("-> 1",
    "functions must have at least 1 parameter  (parse.test 1:1)\n" +
    "  -> 1\n" +
    "  ^-"),

  test("-> 1 2", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 })],
           integer(2, file, lines,
                   { line: 0, column: 5 },
                   { line: 0, column: 6 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 6 })
  ]),

  test("-> 1 -> 2 3", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 })],
           lambda([integer(2, file, lines,
                           { line: 0, column: 8 },
                           { line: 0, column: 9 })],
                  integer(3, file, lines,
                          { line: 0, column: 10 },
                          { line: 0, column: 11 }), file, lines,
                  { line: 0, column: 5 },
                  { line: 0, column: 11 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 11 })
  ]),

  test("-> 1 -> 2 3 <= 4", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 })],
           lambda([integer(2, file, lines,
                           { line: 0, column: 8 },
                           { line: 0, column: 9 })],
                  assign(integer(3, file, lines,
                                 { line: 0, column: 10 },
                                 { line: 0, column: 11 }),
                         integer(4, file, lines,
                                 { line: 0, column: 15 },
                                 { line: 0, column: 16 }), file, lines,
                         { line: 0, column: 10 },
                         { line: 0, column: 16 }), file, lines,
                  { line: 0, column: 5 },
                  { line: 0, column: 16 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 16 })
  ]),

  test("-> 1 2 <= -> 3 4 <= 5", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 })],
           assign(integer(2, file, lines,
                          { line: 0, column: 5 },
                          { line: 0, column: 6 }),
                  lambda([integer(3, file, lines,
                                  { line: 0, column: 13 },
                                  { line: 0, column: 14 })],
                         assign(integer(4, file, lines,
                                        { line: 0, column: 15 },
                                        { line: 0, column: 16 }),
                                integer(5, file, lines,
                                        { line: 0, column: 20 },
                                        { line: 0, column: 21 }), file, lines,
                                { line: 0, column: 15 },
                                { line: 0, column: 21 }), file, lines,
                         { line: 0, column: 10 },
                         { line: 0, column: 21 }), file, lines,
                   { line: 0, column: 5 },
                   { line: 0, column: 21 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 21 })
  ]),

  test("-> 1 &-> 2 ~@3", (file, lines) => [
    lambda([integer(1, file, lines,
                    { line: 0, column: 3 },
                    { line: 0, column: 4 })],
           quote(lambda([integer(2, file, lines,
                                 { line: 0, column: 9 },
                                 { line: 0, column: 10 })],
                        unquote(splice(integer(3, file, lines,
                                               { line: 0, column: 13 },
                                               { line: 0, column: 14 }), file, lines,
                                       { line: 0, column: 12 },
                                       { line: 0, column: 14 }), file, lines,
                                { line: 0, column: 11 },
                                { line: 0, column: 14 }), file, lines,
                        { line: 0, column: 6 },
                        { line: 0, column: 14 }), file, lines,
                 { line: 0, column: 5 },
                 { line: 0, column: 14 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 14 })
  ]),

  test_crash("(->)",
    "functions must have at least 1 parameter  (parse.test 1:2)\n" +
    "  (->)\n" +
    "   ^-"),

  test_crash("(-> 1)",
    "functions must have at least 1 parameter  (parse.test 1:2)\n" +
    "  (-> 1)\n" +
    "   ^-"),


  test("a.b", (file, lines) => [
    dot(symbol("a", file, lines,
               { line: 0, column: 0 },
               { line: 0, column: 1 }),
        symbol("b", file, lines,
               { line: 0, column: 2 },
               { line: 0, column: 3 }),
        file, lines,
        { line: 0, column: 0 },
        { line: 0, column: 3 })
  ]),

  test("a.b <= c.d", (file, lines) => [
    assign(dot(symbol("a", file, lines,
                      { line: 0, column: 0 },
                      { line: 0, column: 1 }),
               symbol("b", file, lines,
                      { line: 0, column: 2 },
                      { line: 0, column: 3 }), file, lines,
               { line: 0, column: 0 },
               { line: 0, column: 3 }),
           dot(symbol("c", file, lines,
                      { line: 0, column: 7 },
                      { line: 0, column: 8 }),
               symbol("d", file, lines,
                      { line: 0, column: 9 },
                      { line: 0, column: 10 }), file, lines,
               { line: 0, column: 7 },
               { line: 0, column: 10 }), file, lines,
           { line: 0, column: 0 },
           { line: 0, column: 10 })
  ]),


  test("&a.~b", (file, lines) => [
    quote(dot(symbol("a", file, lines,
                     { line: 0, column: 1 },
                     { line: 0, column: 2 }),
              unquote(symbol("b", file, lines,
                             { line: 0, column: 4 },
                             { line: 0, column: 5 }), file, lines,
                      { line: 0, column: 3 },
                      { line: 0, column: 5 }), file, lines,
              { line: 0, column: 1 },
              { line: 0, column: 5 }), file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 5 })
  ]),
];
