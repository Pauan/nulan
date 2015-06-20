import { Record, Dict, skip } from "../node_modules/Immutable/src/Immutable";
import { parse } from "./parse";
import { builtins } from "./builtins";
import { macex, macex_rest } from "./macex";
import { Scope } from "./scope";

import { pretty } from "./pretty";
/*import { pretty, Call } from "./types";

console.log(pretty(List([1, 2, 3])));
console.log(pretty(Record([["foo", 1], ["bar", 2]])));
console.log(pretty(Dict([["foo", 1], ["bar", 2]])));
console.log(pretty(Tuple([1, 2, 3])));
console.log(pretty(Call(Tuple([1, 2, 3]), null)));*/

//var x = parse("(foo bar qux 10 5.5 10foo) () (`1 2 ,3 @(4 5)\n) {10 20}10", "TEST.nu");
//var x = parse("{ 1 = (self -> a b) 2 = (self -> a b) }", "TEST.nu");
var x = parse("(+ 1 2) (js/get 1 2 3 4 5) (if (if 1 2 3) (if 1 2 3) (if 1 2 3)) (var foo = 5)", "TEST.nu");

var info1 = macex(Record([
  ["scope", Scope(builtins)],
  ["modules", Dict()],
  ["value", x.get(0)],
  ["phase", "runtime"]
]));

var info2 = macex_rest(info1, skip(x, 1));

console.log(pretty(info2));
