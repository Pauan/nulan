import { tokenize } from "../parse/tokenize";
import { parse } from "../parse/parse";
import { compile } from "../compile/compile";
import { NulanError, warning } from "../util/error";

declare var process: any;

try {
  console.log(compile(parse(tokenize("(+ foo bar qux) (MATCH foo | qux : 1)", "TE\\ST\".NUL"))));

} catch (e) {
  if (e instanceof NulanError) {
    console.error(e.pretty());
    process.exit(1);
  }
}
