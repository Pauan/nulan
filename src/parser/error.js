const repeat = (s, i) =>
  new Array(i + 1)["join"](s);

const format_line = (x) => {
  if (x.start.column === x.end.column) {
    return "";

  } else if (x.start.column < x.end.column) {
    return repeat(" ", x.start.column) + "^" +
           repeat("-", x.end.column - x.start.column - 1);

  } else {
    return repeat(" ", x.end.column) +
           repeat("-", x.start.column - x.end.column - 1) + "^";
  }
};

const format_message = (header, x, message) => {
  const lines = x.lines["slice"](x.start.line, x.end.line + 1);

  lines["push"](format_line(x));

  return header + ": " + message +
         "  (" + x.filename +
         " " + (x.start.line + 1) +
         ":" + (x.start.column + 1) +
         ")\n  " + lines["join"]("\n  ");
};

export const format_error = (x, message) =>
  format_message("Error", x, message);

export const format_warning = (x, message) =>
  format_message("Warning", x, message);

export const crash = (x) => {
  throw x;
};
