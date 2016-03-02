import { expect, expect_crash } from "../assert";
import { make_directories, remove_directory } from "../../ffi/node";
import { chain } from "../../ffi/task";


export default [
  expect(0,
    chain(make_directories("test/foo/bar/qux"), (_) =>
      remove_directory("test/foo"))),
];
