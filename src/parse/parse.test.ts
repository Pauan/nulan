import * as $test from "../util/test";
import * as $loc from "../util/loc";
import * as $parse from "./parse";
import { tokenize } from "./tokenize";
import { NulanError } from "../util/error";
import { AST, symbol, integer, float, string, tag, call, array, record,
         wildcard, bar, quote, unquote, splice, type, assign, match,
         dot, pretty } from "./ast";


function expectParse(
  s: string,
  fn: (
    loc: (left: $loc.Position, right: $loc.Position) => $loc.Loc,
    position: (index: number, line: number, column: number) => $loc.Position
  ) => Array<AST>
): void {
  const filename = "test.nul";

  expect($parse.parse(tokenize(s, filename))).toEqual(
    fn(
      (l, r): $loc.Loc => $loc.loc(filename, l, r),
      $loc.position
    ));
}


function expectError(
  s: string,
  fn: (
    loc: (left: $loc.Position, right: $loc.Position) => $loc.Loc,
    position: (index: number, line: number, column: number) => $loc.Position
  ) => $loc.Loc,
  message: string
): void {
  const filename = "test.nul";

  $test.expectError(() => $parse.parse(tokenize(s, filename)),
    fn(
      (l, r): $loc.Loc => $loc.loc(filename, l, r),
      $loc.position
    ),
    message);
}


function testBrackets(start: string, end: string, make: (args: Array<AST>, loc: $loc.Loc) => AST): void {
  expectParse(start + end, (l, p) => [
    make([], l(p(0, 0, 0), p(2, 0, 2)))
  ]);

  expectParse(start + "a" + end, (l, p) => [
    make([
      symbol("a", l(p(1, 0, 1), p(2, 0, 2))),
    ], l(p(0, 0, 0), p(3, 0, 3)))
  ]);

  expectParse(start + "a b" + end, (l, p) => [
    make([
      symbol("a", l(p(1, 0, 1), p(2, 0, 2))),
      symbol("b", l(p(3, 0, 3), p(4, 0, 4))),
    ], l(p(0, 0, 0), p(5, 0, 5)))
  ]);

  expectError(start,
    (l, p) => l(p(0, 0, 0), p(1, 0, 1)),
    "Missing ending " + end);

  expectError(end,
    (l, p) => l(p(0, 0, 0), p(1, 0, 1)),
    "Missing starting " + start);

  expectError(start + "a",
    (l, p) => l(p(0, 0, 0), p(1, 0, 1)),
    "Missing ending " + end);

  expectError("a" + end,
    (l, p) => l(p(1, 0, 1), p(2, 0, 2)),
    "Missing starting " + start);

  expectError(start + end + end,
    (l, p) => l(p(2, 0, 2), p(3, 0, 3)),
    "Missing starting " + start);

  expectError(start + start + end,
    (l, p) => l(p(0, 0, 0), p(1, 0, 1)),
    "Missing ending " + end);
}


function testPrefix(s: string, make: (ast: AST, loc: $loc.Loc) => AST): void {
  expectParse(s + "1", (l, p) => [
    make(integer("1", l(p(1, 0, 1), p(2, 0, 2))),
         l(p(0, 0, 0), p(2, 0, 2)))
  ]);

  expectError(s,
    (l, p) => l(p(0, 0, 0), p(1, 0, 1)),
    "There must be an expression on the right of " + s);
}


function testInfix(s: string, space: string, make: (left: AST, right: AST, loc: $loc.Loc) => AST, message: string): void {
  const slen = space.length;
  const llen = slen + s.length;
  const len = llen + slen;

  expectParse("a" + space + s + space + "b", (l, p) => [
    make(
      symbol("a", l(p(0, 0, 0), p(1, 0, 1))),
      symbol("b", l(p(1 + len, 0, 1 + len), p(2 + len, 0, 2 + len))),
      l(p(0, 0, 0), p(2 + len, 0, 2 + len))
    )
  ]);

  expectError(space + s + space + "#",
    (l, p) => l(p(slen, 0, slen), p(llen, 0, llen)),
    "There " + message + " the left of " + s);

  expectError("a" + space + s + space + "#",
    (l, p) => l(p(slen + 1, 0, slen + 1), p(llen + 1, 0, llen + 1)),
    "There " + message + " the right of " + s);

  expectError(space + s + space + "b",
    (l, p) => l(p(slen, 0, slen), p(llen, 0, llen)),
    "There " + message + " the left of " + s);
}


test("brackets", () => {
  testBrackets("(", ")", call);
  testBrackets("[", "]", array);
  testBrackets("{", "}", record);

  expectError("(] foo bar)",
    (l, p) => l(p(1, 0, 1), p(2, 0, 2)),
    "Missing starting [");
});


test("prefix", () => {
  testPrefix("&", quote);
  testPrefix("~", unquote);
  testPrefix("@", splice);
});


test("infix", () => {
  $parse.specials["L+"] = $parse.parseInfix(2, "left", assign);
  $parse.specials["L/"] = $parse.parseInfix(1, "left", type);

  $parse.specials["R+"] = $parse.parseInfix(2, "right", assign);
  $parse.specials["R/"] = $parse.parseInfix(1, "right", type);

  // (((1 L+ ((2 L/ 3) L/ 4)) L+ 5) L+ 6)
  expectParse("1 L+ 2 L/ 3 L/ 4 L+ 5 L+ 6", (l, p) => [
    assign(
      assign(
        assign(
          integer("1", l(p(0, 0, 0), p(1, 0, 1))),
          type(
            type(
              integer("2", l(p(5, 0, 5), p(6, 0, 6))),
              integer("3", l(p(10, 0, 10), p(11, 0, 11))),
              l(p(5, 0, 5), p(11, 0, 11))
            ),
            integer("4", l(p(15, 0, 15), p(16, 0, 16))),
            l(p(5, 0, 5), p(16, 0, 16))
          ),
          l(p(0, 0, 0), p(16, 0, 16))
        ),
        integer("5", l(p(20, 0, 20), p(21, 0, 21))),
        l(p(0, 0, 0), p(21, 0, 21))
      ),
      integer("6", l(p(25, 0, 25), p(26, 0, 26))),
      l(p(0, 0, 0), p(26, 0, 26))
    )
  ]);

  // (1 R+ ((2 R/ (3 R/ 4)) R+ (5 R+ 6)))
  expectParse("1 R+ 2 R/ 3 R/ 4 R+ 5 R+ 6", (l, p) => [
    assign(
      integer("1", l(p(0, 0, 0), p(1, 0, 1))),
      assign(
        type(
          integer("2", l(p(5, 0, 5), p(6, 0, 6))),
          type(
            integer("3", l(p(10, 0, 10), p(11, 0, 11))),
            integer("4", l(p(15, 0, 15), p(16, 0, 16))),
            l(p(10, 0, 10), p(16, 0, 16))
          ),
          l(p(5, 0, 5), p(16, 0, 16))
        ),
        assign(
          integer("5", l(p(20, 0, 20), p(21, 0, 21))),
          integer("6", l(p(25, 0, 25), p(26, 0, 26))),
          l(p(20, 0, 20), p(26, 0, 26))
        ),
        l(p(5, 0, 5), p(26, 0, 26))
      ),
      l(p(0, 0, 0), p(26, 0, 26))
    )
  ]);

  testInfix(":", " ", match, "must be an expression on");
  testInfix("<=", " ", assign, "must be an expression on");
  testInfix("::", " ", type, "must be an expression on");
  testInfix(".", "", dot, "cannot be whitespace to");
});


test("|", () => {
  expectParse("|", (l, p) => [
    bar([], l(p(0, 0, 0), p(1, 0, 1)))
  ]);

  // ((| foo) : ((| bar) : qux))
  expectParse("| foo : | bar : qux", (l, p) => [
    match(
      bar([
        symbol("foo", l(p(2, 0, 2), p(5, 0, 5)))
      ], l(p(0, 0, 0), p(5, 0, 5))),
      match(
        bar([
          symbol("bar", l(p(10, 0, 10), p(13, 0, 13)))
        ], l(p(8, 0, 8), p(13, 0, 13))),
        symbol("qux", l(p(16, 0, 16), p(19, 0, 19))),
        l(p(8, 0, 8), p(19, 0, 19))
      ),
      l(p(0, 0, 0), p(19, 0, 19))
    )
  ]);
});


test("<=", () => {
  expectParse("(foo <= bar)", (l, p) => [
    call([
      assign(
        symbol("foo", l(p(1, 0, 1), p(4, 0, 4))),
        symbol("bar", l(p(8, 0, 8), p(11, 0, 11))),
        l(p(1, 0, 1), p(11, 0, 11))
      )
    ], l(p(0, 0, 0), p(12, 0, 12)))
  ]);
});


test("&", () => {
  expectParse("(foo &bar)", (l, p) => [
    call([
      symbol("foo", l(p(1, 0, 1), p(4, 0, 4))),
      quote(
        symbol("bar", l(p(6, 0, 6), p(9, 0, 9))),
        l(p(5, 0, 5), p(9, 0, 9))
      )
    ], l(p(0, 0, 0), p(10, 0, 10)))
  ]);
});


test("^", () => {
  expectParse("&foo <= bar qux", (l, p) => [
    quote(
      assign(
        symbol("foo", l(p(1, 0, 1), p(4, 0, 4))),
        symbol("bar", l(p(8, 0, 8), p(11, 0, 11))),
        l(p(1, 0, 1), p(11, 0, 11))
      ),
      l(p(0, 0, 0), p(11, 0, 11))
    ),

    symbol("qux", l(p(12, 0, 12), p(15, 0, 15)))
  ]);

  expectParse("^(&foo) <= bar qux", (l, p) => [
    assign(
      quote(
        symbol("foo", l(p(3, 0, 3), p(6, 0, 6))),
        l(p(0, 0, 0), p(7, 0, 7))
      ),
      symbol("bar", l(p(11, 0, 11), p(14, 0, 14))),
      l(p(0, 0, 0), p(14, 0, 14))
    ),

    symbol("qux", l(p(15, 0, 15), p(18, 0, 18)))
  ]);

  expectParse("^(&foo <= bar) qux", (l, p) => [
    quote(
      assign(
        symbol("foo", l(p(3, 0, 3), p(6, 0, 6))),
        symbol("bar", l(p(10, 0, 10), p(13, 0, 13))),
        l(p(3, 0, 3), p(13, 0, 13))
      ),
      l(p(0, 0, 0), p(14, 0, 14))
    ),

    symbol("qux", l(p(15, 0, 15), p(18, 0, 18)))
  ]);
});


test("parse", () => {
  expectParse("(MATCH foo | &bar : qux <= corge)", (l, p) => [
    call([
      symbol("MATCH", l(p(1, 0, 1), p(6, 0, 6))),
      symbol("foo", l(p(7, 0, 7), p(10, 0, 10))),
      match(
        bar([
          quote(symbol("bar", l(p(14, 0, 14), p(17, 0, 17))),
                l(p(13, 0, 13), p(17, 0, 17)))
        ], l(p(11, 0, 11), p(17, 0, 17))),
        assign(
          symbol("qux", l(p(20, 0, 20), p(23, 0, 23))),
          symbol("corge", l(p(27, 0, 27), p(32, 0, 32))),
          l(p(20, 0, 20), p(32, 0, 32))
        ),
        l(p(11, 0, 11), p(32, 0, 32))
      )
    ], l(p(0, 0, 0), p(33, 0, 33))),
  ]);

  expectParse("(MATCH foo [ &bar ] : qux <= corge)", (l, p) => [
    call([
      symbol("MATCH", l(p(1, 0, 1), p(6, 0, 6))),
      symbol("foo", l(p(7, 0, 7), p(10, 0, 10))),
      match(
        array([
          quote(symbol("bar", l(p(14, 0, 14), p(17, 0, 17))),
                l(p(13, 0, 13), p(17, 0, 17)))
        ], l(p(11, 0, 11), p(19, 0, 19))),
        assign(
          symbol("qux", l(p(22, 0, 22), p(25, 0, 25))),
          symbol("corge", l(p(29, 0, 29), p(34, 0, 34))),
          l(p(22, 0, 22), p(34, 0, 34))
        ),
        l(p(11, 0, 11), p(34, 0, 34))
      )
    ], l(p(0, 0, 0), p(35, 0, 35))),
  ]);


  // ((| a (& ((~a) <= (~b)))) : c)
  expectParse("| a &~b <= ~c : d", (l, p) => [
    match(
      bar([
        symbol("a", l(p(2, 0, 2), p(3, 0, 3))),
        quote(
          assign(
            unquote(
              symbol("b", l(p(6, 0, 6), p(7, 0, 7))),
              l(p(5, 0, 5), p(7, 0, 7))
            ),
            unquote(
              symbol("c", l(p(12, 0, 12), p(13, 0, 13))),
              l(p(11, 0, 11), p(13, 0, 13))
            ),
            l(p(5, 0, 5), p(13, 0, 13))
          ),
          l(p(4, 0, 4), p(13, 0, 13))
        )
      ], l(p(0, 0, 0), p(13, 0, 13))),
      symbol("d", l(p(16, 0, 16), p(17, 0, 17))),
      l(p(0, 0, 0), p(17, 0, 17))
    )
  ]);


  // ((| 1 (~ (@1)) (& 1)) : (2 <= (1 :: 1.2)))
  /*expectParse("| 1 ~ @1 & 1 : 2 <= 1 :: 1.2", (l, p) =>
);*/


  expectParse("(| : 1)", (l, p) => [
    call([
      match(
        bar([], l(p(1, 0, 1), p(2, 0, 2))),
        integer("1", l(p(5, 0, 5), p(6, 0, 6))),
        l(p(1, 0, 1), p(6, 0, 6))
      ),
    ], l(p(0, 0, 0), p(7, 0, 7)))
  ]);


  expectParse("(| foo bar : &1)", (l, p) => [
    call([
      match(
        bar([
          symbol("foo", l(p(3, 0, 3), p(6, 0, 6))),
          symbol("bar", l(p(7, 0, 7), p(10, 0, 10)))
        ], l(p(1, 0, 1), p(10, 0, 10))),
        quote(integer("1", l(p(14, 0, 14), p(15, 0, 15))),
              l(p(13, 0, 13), p(15, 0, 15))),
        l(p(1, 0, 1), p(15, 0, 15))
      )
    ], l(p(0, 0, 0), p(16, 0, 16)))
  ]);
});
