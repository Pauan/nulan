import "babel-polyfill";
import { run } from "../util/promise";
import { compile } from "./compiler/compile";

run(async () => {
  await compile({
    root: "src",
    input: "src/parse.nul",
    output: "build/parse.js"
  });
});
