import * as $ast from "./ast";
import { loc, position } from "../util/loc";


var l = loc("test.nul", position(0, 0, 0), position(0, 0, 0));


test("pretty", () => {
  expect($ast.pretty($ast.variable(1, null, l))).toBe("#(var 1)");
  expect($ast.pretty($ast.variable(1, "foo", l))).toBe("#(var 1 foo)");
  expect($ast.pretty($ast.integer("1", l))).toBe("1");
  expect($ast.pretty($ast.symbol("foo", l))).toBe("foo");
  expect($ast.pretty($ast.float("1.2", l))).toBe("1.2");
  expect($ast.pretty($ast.string("foo\"\\", l))).toBe("\"foo\\\"\\\\\"");
  expect($ast.pretty($ast.tag("foo", l))).toBe("*foo");
  expect($ast.pretty($ast.wildcard(l))).toBe("_");

  expect($ast.pretty($ast.lambda([], $ast.symbol("foo", l), l))).toBe("(-> foo)");
  expect($ast.pretty($ast.lambda([$ast.symbol("foo", l)], $ast.symbol("bar", l), l))).toBe("(-> foo bar)");
  expect($ast.pretty($ast.lambda([$ast.symbol("foo", l), $ast.symbol("bar", l)], $ast.symbol("qux", l), l))).toBe("(-> foo bar qux)");

  expect($ast.pretty($ast.call([], l))).toBe("()");
  expect($ast.pretty($ast.call([$ast.symbol("foo", l)], l))).toBe("( foo )");
  expect($ast.pretty($ast.call([$ast.symbol("foo", l), $ast.symbol("bar", l)], l))).toBe("( foo bar )");

  expect($ast.pretty($ast.array([], l))).toBe("[]");
  expect($ast.pretty($ast.array([$ast.symbol("foo", l)], l))).toBe("[ foo ]");
  expect($ast.pretty($ast.array([$ast.symbol("foo", l), $ast.symbol("bar", l)], l))).toBe("[ foo bar ]");

  expect($ast.pretty($ast.record([], l))).toBe("{}");
  expect($ast.pretty($ast.record([$ast.symbol("foo", l)], l))).toBe("{ foo }");
  expect($ast.pretty($ast.record([$ast.symbol("foo", l), $ast.symbol("bar", l)], l))).toBe("{ foo bar }");

  expect($ast.pretty($ast.bar($ast.symbol("foo", l), l))).toBe("(| foo)");
  expect($ast.pretty($ast.quote($ast.symbol("foo", l), l))).toBe("(& foo)");
  expect($ast.pretty($ast.unquote($ast.symbol("foo", l), l))).toBe("(~ foo)");
  expect($ast.pretty($ast.splice($ast.symbol("foo", l), l))).toBe("(@ foo)");

  expect($ast.pretty($ast.dot($ast.symbol("foo", l), $ast.symbol("bar", l), l))).toBe("(foo . bar)");
  expect($ast.pretty($ast.match($ast.symbol("foo", l), $ast.symbol("bar", l), l))).toBe("(foo : bar)");
  expect($ast.pretty($ast.assign($ast.symbol("foo", l), $ast.symbol("bar", l), l))).toBe("(foo <= bar)");
  expect($ast.pretty($ast.type($ast.symbol("foo", l), $ast.symbol("bar", l), l))).toBe("(foo :: bar)");
});
