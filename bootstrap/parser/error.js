import { crash } from "../../builtin/ffi/crash";
import { eol } from "../../util/node";
import { repeat } from "../../util/string";


const format_line = (x) => {
  if (x.start.column === x.end.column) {
    return repeat(" ", x.start.column) + "^";

  } else if (x.start.column < x.end.column) {
    return repeat(" ", x.start.column) + "^" +
           repeat("-", x.end.column - x.start.column - 1);

  } else {
    return repeat(" ", x.end.column) +
           repeat("-", x.start.column - x.end.column) + "^";
  }
};

const format_message = (header, x, message) => {
  const loc = x.loc;

  const lines = loc.lines["slice"](loc.start.line, loc.end.line + 1);

  lines["push"](format_line(loc));

  return header + ": " + message +
         "  (" + loc.filename +
         " " + (loc.start.line + 1) +
         ":" + (loc.start.column + 1) +
         ")" + eol +
         "  " + lines["join"](eol + "  ");
};


export const error = (x, message) => {
  crash(new Error(format_message("Error", x, message)));
};

export const warning = (x, message) => {
  console["warn"](format_message("Warning", x, message));
};
