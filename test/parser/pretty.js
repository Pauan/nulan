import { expect } from "../assert";
import { wrap } from "../../ffi/task";
import { pretty } from "../../src/parser/pretty";
import { lines } from "../../util/string";
import { string, symbol, integer,
         call, list, record, lambda,
         dot, bar, assign, type,
         quote, unquote, splice } from "../../src/parser/ast";


const test = (input, expected) =>
  expect(expected, wrap(pretty(input)));


const test_record = record([assign(symbol("a"), symbol("b")),
                            bar(assign(symbol("c"), symbol("d")))]);

const test_lambda = lambda([symbol("a"), symbol("b")],
                      call([symbol("c"), symbol("d")]));

export default [
  test(call([integer(1), integer(2), integer(3)]),
    "(1 2 3)"),

  test(call([]),
    "()"),

  test(call([call([call([call([symbol("foo")])])])]),
    "((((foo))))"),

  test(call([symbol("foo"),
             call([symbol("1"),
                   symbol("2"),
                   symbol("3")]),
             call([symbol("1"),
                   symbol("2"),
                   symbol("3")])]),
    "(foo\n" +
    "  (1 2 3)\n" +
    "  (1 2 3))"),

  test(call([string("foo"), string("bar"), string("qux")]),
    "(\"foo\" \"bar\" \"qux\")"),

  test(call([string("foo"), string("bar\nqux"), string("corge\nyes")]),
    "(\"foo\" \"bar\n" +
    "        qux\" \"corge\n" +
    "              yes\")"),

  /*test(call([string("foo\n\n")]),
    "(\"foo\n\n" +
    "  \")"),*/


  test(call([string("foo\nbar"), quote(string("qux\ncorge"))]),
    "(\"foo\n" +
    "  bar\" &\"qux\n" +
    "         corge\")"),

  test(call([string("foo\nbar"), unquote(string("qux\ncorge"))]),
    "(\"foo\n" +
    "  bar\" ~\"qux\n" +
    "         corge\")"),

  test(call([string("foo\nbar"), splice(string("qux\ncorge"))]),
    "(\"foo\n" +
    "  bar\" @\"qux\n" +
    "         corge\")"),

  test(assign(string("foo\nbar"), string("qux\ncorge")),
    "\"foo\n" +
    " bar\" <= \"qux\n" +
    "          corge\""),

  test(dot(string("foo\nbar"), string("qux\ncorge")),
    "\"foo\n" +
    " bar\".\"qux\n" +
    "       corge\""),

  test(type(string("foo\nbar"), string("qux\ncorge")),
    "\"foo\n" +
    " bar\" :: \"qux\n" +
    "          corge\""),

  test(call([string("foo\nbar"), string("bar"), string("qux")]),
    "(\"foo\n" +
    "  bar\" \"bar\" \"qux\")"),

  test(call([call([symbol("1"),
                   symbol("2"),
                   symbol("3")]),
             call([symbol("1"),
                   symbol("2"),
                   symbol("3")]),
             call([symbol("1"),
                   symbol("2"),
                   symbol("3")])]),
    "((1 2 3)\n" +
    "  (1 2 3)\n" +
    "  (1 2 3))"),


  test(assign(test_record, test_record),
    "{ a <= b\n" +
    "| c <= d } <= { a <= b\n" +
    "              | c <= d }"),

  test(call([symbol("1"),
             symbol("2"),
             test_record]),
    "(1 2\n"+
    "  { a <= b\n" +
    "  | c <= d })"),

  test(list([symbol("1"),
             symbol("2"),
             test_record]),
    "[ 1 2\n"+
    "{ a <= b\n" +
    "| c <= d } ]"),

  test(list([symbol("1"),
             symbol("2"),
             bar(test_record)]),
    "[ 1 2\n"+
    "| { a <= b\n" +
    "  | c <= d } ]"),

  test(record([symbol("a"),
               bar(assign(symbol("b"), symbol("c"))),
               bar(assign(symbol("a"), symbol("b")))]),
    "{ a\n" +
    "| b <= c\n" +
    "| a <= b }"),

  test(record([assign(symbol("a"), symbol("d")),
               bar(assign(symbol("b"), symbol("c"))),
               bar(assign(symbol("a"), symbol("b")))]),
    "{ a <= d\n" +
    "| b <= c\n" +
    "| a <= b }"),

  test(test_record,
    "{ a <= b\n" +
    "| c <= d }"),

  test(record([test_record]),
    "{ { a <= b\n" +
    "  | c <= d } }"),

  test(record([assign(symbol("a"), symbol("d")),
               bar(assign(symbol("b"), test_record)),
               bar(assign(symbol("a"), integer(5)))]),
    "{ a <= d\n" +
    "| b <= { a <= b\n" +
    "       | c <= d }\n" +
    "| a <= 5 }"),


  test(call([lambda([symbol("a"), symbol("b")], symbol("c")), symbol("d")]),
    "((-> a b\n" +
    "   c)\n" +
    "  d)"),

  test(call([lambda([], string("foo\nbar"))]),
    "((->\n" +
    "   \"foo\n" +
    "    bar\"))"),

  test(call([lambda([string("foo\nbar"), string("qux\ncorge")], symbol("c")), symbol("d")]),
    "((-> \"foo\n" +
    "      bar\" \"qux\n" +
    "            corge\"\n" +
    "   c)\n" +
    "  d)"),

  test(call([test_lambda, symbol("e")]),
    "((-> a b\n" +
    "   (c d))\n" +
    "  e)"),


  test(record([assign(symbol("foo"), test_lambda),
               bar(assign(symbol("bar"), test_lambda)),
               bar(assign(symbol("corge"), test_lambda))]),
    "{ foo <= (-> a b\n" +
    "           (c d))\n" +
    "| bar <= (-> a b\n" +
    "           (c d))\n" +
    "| corge <= (-> a b\n" +
    "             (c d)) }"),
];
