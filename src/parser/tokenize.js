import { symbol, integer, number } from "./types";


const tokenize_syntax = (output, file, lines, line, column) => {
  const char = lines[line][column];

  const start = { line, column };

  column += 1;

  const end = { line, column };

  output.push(symbol(char, file, lines, start, end));

  return tokenize1(output, file, lines, line, column);
};


const tokenize_symbol1 = (value, file, lines, start, end) => {
  // TODO a tiny bit hacky
  if (/^[0-9]+$/["test"](value)) {
    return integer(+value, file, lines, start, end);

  // TODO a tiny bit hacky
  } else if (/^[0-9]+\.[0-9]+$/["test"](value)) {
    return number(+value, file, lines, start, end);

  } else {
    return symbol(value, file, lines, start, end);
  }
};

const tokenize_symbol = (output, file, lines, line, column) => {
  const start = { line, column };

  const chars = lines[line];

  let value = chars[column];

  column += 1;

  for (;;) {
    if (column < chars["length"]) {
      const char = chars[column];

      if (specials[char] != null) {
        const end = { line, column };
        output.push(tokenize_symbol1(value, file, lines, start, end));
        return specials[char](output, file, lines, line, column);

      } else {
        value += char;
        column += 1;
      }

    } else {
      const end = { line, column };
      output.push(tokenize_symbol1(value, file, lines, start, end));
      return tokenize1(output, file, lines, line, column);
    }
  }
};


const specials = {
  // TODO error for \t ?
  " ": (output, file, lines, line, column) =>
    tokenize1(output, file, lines, line, column + 1),

  /*"#": (car, cdr, file, loc) => {

  },

  "\"": (car, cdr, file, loc) => {
  },*/

  "~": (output, file, lines, line, column) => {
    const chars = lines[line];
    const next  = column + 1;

    if (next < chars["length"]) {
      const char = chars[next];

      if (char === "@") {
        const start = { line, column };

        column = next + 1;

        const end = { line, column };

        output.push(symbol("~@", file, lines, start, end));

        return tokenize1(output, file, lines, line, column);

      } else {
        return tokenize_syntax(output, file, lines, line, column);
      }

    } else {
      return tokenize_syntax(output, file, lines, line, column);
    }
  },

  "(": tokenize_syntax,
  ")": tokenize_syntax,
  "[": tokenize_syntax,
  "]": tokenize_syntax,
  "&": tokenize_syntax,
  "@": tokenize_syntax
};


const tokenize1 = (output, file, lines, line, column) => {
  for (;;) {
    if (line < lines["length"]) {
      const chars = lines[line];

      if (column < chars["length"]) {
        const char = chars[column];

        if (specials[char] != null) {
          return specials[char](output, file, lines, line, column);

        } else {
          return tokenize_symbol(output, file, lines, line, column);
        }

      } else {
        line = line + 1;
        column = 0;
      }

    } else {
      return output;
    }
  }
};

export const tokenize = (string, file) => {
  // TODO handle \r
  const lines = string["split"](/\n/);

  return tokenize1([], file, lines, 0, 0);
};


const x = tokenize("1 1.5 (foo bar qux) u@q\nqux\n(nou)\n~\n~foo\n~@foo", "NUL");


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

  lines.push(format_line(x));

  return header + ": " + message +
         "  (" + x.filename +
         " " + (x.start.line + 1) +
         ":" + (x.start.column + 1) +
         ")\n  " + lines["join"]("\n  ");
};

const format_error = (x, message) =>
  format_message("Error", x, message);

const format_warning = (x, message) =>
  format_message("Warning", x, message);


console.log(format_error(tokenize("\n  a\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  ab\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  abc\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n    abcd\n\n", "maybe.nul")[0], "undefined variable foo"));
