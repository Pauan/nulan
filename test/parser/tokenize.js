import { assert_equal } from "../assert";
import { string } from "../../src/parser/types.js";
import { tokenize } from "../../src/parser/tokenize.js";
import { lines } from "../../src/parser/string.js";


const file = "tokenize.test";

const test = (input, f) => {
  const x = lines(input);
  assert_equal(tokenize(x, file), f(file, x), input);
};


test("\"\\u{21}\\u{1D306}\"", (file, lines) =>
  [string("!\uD834\uDF06", file, lines,
          { line: 0, column: 0 },
          { line: 0, column: 17 })]);
