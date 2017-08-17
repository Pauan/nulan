import * as $test from "../util/test";
import * as $loc from "../util/loc";
import { tokenize } from "../parse/tokenize";
import { parse } from "../parse/parse";
import { compile } from "./compile";
import { Program, define, variable, number } from "./ir";


function expectCompile(
  s: string,
  fn: (
    loc: (left: $loc.Position, right: $loc.Position) => $loc.Loc,
    position: (index: number, line: number, column: number) => $loc.Position
  ) => Array<Program>
): void {
  const filename = "test.nul";

  expect(compile(parse(tokenize(s, filename)))).toEqual(fn(
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

  $test.expectError(() => compile(parse(tokenize(s, filename))),
    fn(
      (l, r): $loc.Loc => $loc.loc(filename, l, r),
      $loc.position
    ),
    message);
}


test("LET", () => {
  expectCompile("(LET foo :: Int32 42)", (l, p) => [
    define(
      variable(0, "foo", l(p(5, 0, 5), p(8, 0, 8))),
      number(42, l(p(18, 0, 18), p(20, 0, 20))),
      l(p(0, 0, 0), p(21, 0, 21))
    )
  ]);
});
