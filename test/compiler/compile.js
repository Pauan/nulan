import { expect, expect_crash } from "../assert";
import { reply } from "../../ffi/task";
import { tokenize } from "../../src/parser/tokenize";
import { parse } from "../../src/parser/parse";
import { compile } from "../../src/compiler/compile";
import { lines, repeat } from "../../util/string";


const file = "compile.test";

const test = (input, f) => {
  const x = lines(input);

  const _loc = (line1, column1, line2, column2) =>
    loc(file, x,
        { line: line1, column: column1 },
        { line: line2, column: column2 });

  return expect(f(_loc),
           reply(compile(parse(tokenize(x, file)))));
};


export default [
  test("{ hi }", (loc) => [
    record([symbol("hi", loc(0, 2,
                             0, 4))],
           loc(0, 0,
               0, 6))
  ]),
];
