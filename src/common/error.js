import { join, repeat } from "../node_modules/Immutable/src/Immutable";

function error_source(source1, filename, start, end) {
  var source2 = source1.split(/\n/g);

  var start_line = start.get("line");
  var start_column = start.get("column");

  var end_line = end.get("line");
  var end_column = end.get("column");

  var source_line = source2[start_line - 1];

  var squigglies = (start_line === end_line
                     ? end_column - start_column
                     : source_line.length - (start_column - 1));

  return "\n  " + source_line + "\n  " +
         join(repeat(" ", start_column - 1)) +
         join(repeat("^", squigglies)) +
         "  [line " + start_line + ", column " + start_column + ", file " + filename + "]";
}

export function error(x, message) {
  var loc = x.get("loc");
  var source = loc.get("source");
  var filename = loc.get("filename");
  var start = loc.get("start");
  var end = loc.get("end");
  return new Error(message + error_source(source, filename, start, end));
}
