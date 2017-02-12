import { test_group, run_tests } from "./assert";

import $task from "./js/task";
//import $node from "./js/node";
//import $tokenize from "./parser/tokenize";
//import $parse from "./parser/parse";
//import $compile from "./compiler/compile";


run_tests([
  test_group("js/task.js", $task),
  //test_group("js/node.js", test2),

  //test_group("src/tokenize.js", $tokenize),
  //test_group("src/parse.js", $parse),
  //test_group("src/compile.js", $compile)
]);
