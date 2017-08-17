import { tokenize } from "../parse/tokenize";
import { NulanError, warning } from "../util/error";

declare var process: any;

try {
  console.log(tokenize("\"foobar \\1\"\nfoobar qux corge#/hiya/#testing 10 50.5000.test\n#testing\ntesting", "TE\\ST\".NUL"));

} catch (e) {
  if (e instanceof NulanError) {
    console.error(e.pretty());
    process.exit(1);
  }
}
