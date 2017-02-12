import { expect, expect_crash } from "../assert";
import { make_directories, remove_directory } from "../../builtin/ffi/node";
import { chain } from "../../builtin/ffi/task";


export default [
  expect(0,
    chain(make_directories("test/foo/bar/qux"), (_) =>
      remove_directory("test/foo"))),
];
