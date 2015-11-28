import { test_group, perform_tests } from "./assert";

import test1 from "./ffi/task";
//import test2 from "./parser/tokenize";
//import test3 from "./parser/parse";
//import test4 from "./parser/pretty";

perform_tests([
  test_group("task.js", test1)
]);
