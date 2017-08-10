import { tokenize } from "./tokenize";
import { NulanError } from "../util/error";
import { loc as l, position as p } from "../util/loc";
import { AST, symbol, integer, float, string, tag, call, lambda, array,
         record, wildcard, bar, quote, unquote, splice, type, assign, match,
         dot, pretty } from "./ast";
import * as $parse from "./parse";


const f = "test.nul";

function parse(s: string, filename: string): Array<AST> {
  return $parse.parse(tokenize(s, filename));
}


test("parse", () => {
  expect(parse("(&foo) <= bar qux", f)).toEqual([
    assign(
      quote(symbol("foo", l(f, p(2, 0, 2), p(5, 0, 5))),
            l(f, p(0, 0, 0), p(6, 0, 6))),
      symbol("bar", l(f, p(10, 0, 10), p(13, 0, 13))),
      l(f, p(0, 0, 0), p(13, 0, 13))
    ),

    symbol("qux", l(f, p(14, 0, 14), p(17, 0, 17)))
  ]);

  expect(parse("(&foo <= bar) qux", f)).toEqual([
    quote(assign(
            symbol("foo", l(f, p(2, 0, 2), p(5, 0, 5))),
            symbol("bar", l(f, p(9, 0, 9), p(12, 0, 12))),
            l(f, p(2, 0, 2), p(12, 0, 12))
          ),
          l(f, p(0, 0, 0), p(13, 0, 13))),

    symbol("qux", l(f, p(14, 0, 14), p(17, 0, 17)))
  ]);

  expect(parse("(MATCH foo | &bar : qux <= corge)", f)).toEqual([
    call([
      symbol("MATCH", l(f, p(1, 0, 1), p(6, 0, 6))),
      symbol("foo", l(f, p(7, 0, 7), p(10, 0, 10))),
      match(
        bar(quote(symbol("bar", l(f, p(14, 0, 14), p(17, 0, 17))),
                  l(f, p(13, 0, 13), p(17, 0, 17))),
            l(f, p(11, 0, 11), p(17, 0, 17))),
        assign(
          symbol("qux", l(f, p(20, 0, 20), p(23, 0, 23))),
          symbol("corge", l(f, p(27, 0, 27), p(32, 0, 32))),
          l(f, p(20, 0, 20), p(32, 0, 32))
        ),
        l(f, p(11, 0, 11), p(32, 0, 32))
      )
    ], l(f, p(0, 0, 0), p(33, 0, 33))),
  ]);

  expect(parse("(MATCH foo [ &bar ] : qux <= corge)", f)).toEqual([
    call([
      symbol("MATCH", l(f, p(1, 0, 1), p(6, 0, 6))),
      symbol("foo", l(f, p(7, 0, 7), p(10, 0, 10))),
      match(
        array([
          quote(symbol("bar", l(f, p(14, 0, 14), p(17, 0, 17))),
                l(f, p(13, 0, 13), p(17, 0, 17)))
        ], l(f, p(11, 0, 11), p(19, 0, 19))),
        assign(
          symbol("qux", l(f, p(22, 0, 22), p(25, 0, 25))),
          symbol("corge", l(f, p(29, 0, 29), p(34, 0, 34))),
          l(f, p(22, 0, 22), p(34, 0, 34))
        ),
        l(f, p(11, 0, 11), p(34, 0, 34))
      )
    ], l(f, p(0, 0, 0), p(35, 0, 35))),
  ]);

	/*expect(parse("(-> foo bar &1) (-> 1) foo bar (qux) [corge] {} _ |1 ~ @1 & 1 : 2 <= 1 :: 1. 2 foo. 10 \"qux\" *yes", f)).toEqual([
    call([
      lambda([ symbol("foo", l(f, p(4, 0, 4), p(7, 0, 7))),
               symbol("bar", l(f, p(8, 0, 8), p(11, 0, 11))) ],
        quote(integer("1", l(f, p(13, 0, 13), p(14, 0, 14))),
              l(f, p(12, 0, 12), p(14, 0, 14))),
        l(f, p(1, 0, 1), p(14, 0, 14)))
    ], l(f, p(0, 0, 0), p(15, 0, 15))),

    call([
      lambda([],
        integer("1", l(f, p(20, 0, 20), p(21, 0, 21))),
        l(f, p(17, 0, 17), p(21, 0, 21)))
    ], l(f, p(16, 0, 16), p(22, 0, 22))),

		symbol("foo", l(f, p(23, 0, 23), p(26, 0, 26))),

    symbol("bar", l(f, p(27, 0, 27), p(30, 0, 30))),

    call([
      symbol("qux", l(f, p(32, 0, 32), p(35, 0, 35)))
    ], l(f, p(31, 0, 31), p(36, 0, 36))),

    array([
      symbol("corge", l(f, p(38, 0, 38), p(43, 0, 43)))
    ], l(f, p(37, 0, 37), p(44, 0, 44))),

    record([], l(f, p(45, 0, 45), p(46, 0, 46))),

    wildcard(l(f, p(47, 0, 47), p(48, 0, 48))),

    bar(integer("1", l(f, p(50, 0, 50), p(51, 0, 51))),
        l(f, p(49, 0, 49), p(51, 0, 51))),

    unquote(splice(integer("1", l(f, p(55, 0, 55), p(56, 0, 56))),
                   l(f, p(54, 0, 54), p(56, 0, 56))),
            l(f, p(52, 0, 52), p(56, 0, 56))),

    match(
      quote(integer("1", l(f, p(59, 0, 59), p(60, 0, 60))),
            l(f, p(57, 0, 57), p(60, 0, 60))),
      assign(
        integer("2", l(f, p(63, 0, 63), p(64, 0, 64))),
        type(
          integer("1", l(f, p(68, 0, 68), p(69, 0, 69))),
          float("1. 2", l(f, p(73, 0, 73), p(77, 0, 77))),
          l(f, p(68, 0, 68), p(77, 0, 77))
        ),
        l(f, p(63, 0, 63), p(77, 0, 77))
      ),
      l(f, p(57, 0, 57), p(77, 0, 77))
    ),

    dot(
      symbol("foo", l(f, p(78, 0, 78), p(81, 0, 81))),
      integer("10", l(f, p(83, 0, 83), p(84, 0, 84))),
      l(f, p(78, 0, 78), p(84, 0, 84))
    ),

    string("qux", l(f, p(85, 0, 85), p(90, 0, 90))),

    tag("yes", l(f, p(91, 0, 91), p(95, 0, 95)))
	]);*/
});
