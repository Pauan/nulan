foo();
$array["bar"]();
$array.kill_thread();
$array[foo]();
//$array[kill_thread]();
uhh.kill_thread();
uhh[kill_thread]();
noop();
noop2();
noop3();
qux12();

const yes = (foo, foo1, noop, noop3) => {
  return foo + foo1 + noop + noop3;
};

import * as $array from "./bar";
import foo from "./bar";
import uhh from "./bar.js";

import { noop as noop3, qux12 } from "./bar";
export { kill_thread };
export { noop as noop2 } from "./bar";
export * from "./bar";
export default 5;

export const foo1 = 10;
