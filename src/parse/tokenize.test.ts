import { tokenize } from "./tokenize";
import { loc, position } from "../util/loc";


test("tokenize", () => {
	expect(tokenize("foo", "test.nul")).toEqual([{
    type: "symbol",
    value: "foo",
    loc: loc("test.nul", position(0, 0, 0), position(3, 0, 3))
  }]);
});
