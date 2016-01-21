import { test_group, run_tests } from "./assert";

import $task from "./ffi/task";
//import $node from "./ffi/node";
import $tokenize from "./parser/tokenize";
import $parse from "./parser/parse";
//import $compile from "./compiler/compile";


run_tests([
  test_group("ffi/task.js", $task),
  //test_group("ffi/node.js", test2),

  test_group("src/tokenize.js", $tokenize),
  test_group("src/parse.js", $parse),
  //test_group("src/compile.js", $compile)
]);
