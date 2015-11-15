import { symbol, integer } from "./types";
import { format_error } from "./error";
import { crash } from "../node-js";


const peek = (a, i) => {
  if (i < a["length"]) {
    return a[i];
  } else {
    return null;
  }
};


const tokenize_syntax = (output, file, lines, line, column) => {
  const char = lines[line][column];

  const start = { line, column };

  column += 1;

  const end = { line, column };

  output["push"](symbol(char, file, lines, start, end));

  return tokenize1(output, file, lines, line, column);
};


const tokenize_symbol1 = (value, file, lines, start, end) => {
  // TODO a tiny bit hacky
  if (/^[0-9]+$/["test"](value)) {
    return integer(+value, file, lines, start, end);

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
    const char = peek(chars, column);

    if (char === null || specials[char] != null) {
      const end = { line, column };
      output["push"](tokenize_symbol1(value, file, lines, start, end));
      return tokenize1(output, file, lines, line, column);

    } else {
      value += char;
      column += 1;
    }
  }
};


const tokenize_block_comment = (output, file, lines, line, column) => {
  const pending = [];

  const start = { line, column };

  column += 2;

  const end = { line, column };

  pending["push"](symbol("#/", file, lines, start, end));

  for (;;) {
    const chars = peek(lines, line);

    if (chars !== null) {
      const next1 = peek(chars, column);
      const next2 = peek(chars, column + 1);

      // The comment block has ended
      if (next1 === "/" && next2 === "#") {
        column += 2;
        pending["pop"]();

        if (pending["length"] === 0) {
          return tokenize1(output, file, lines, line, column);
        }

      // Allow for nested comment blocks
      } else if (next1 === "#" && next2 === "/") {
        const start = { line, column };

        column += 2;

        const end = { line, column };

        pending["push"](symbol("#/", file, lines, start, end));

      } else if (next1 !== null) {
        column += 1;

      } else {
        line += 1;
        column = 0;
      }

    } else {
      crash(format_error(pending[pending["length"] - 1], "missing ending /#"));
    }
  }
};


const specials = {
  "\t": (output, file, lines, line, column) => {
    const start = { line, column };

    column += 1;

    const end = { line, column };

    crash(format_error(symbol("\t", file, lines, start, end), "tabs (U+0009) are not allowed"));
  },

  " ": (output, file, lines, line, column) =>
    tokenize1(output, file, lines, line, column + 1),

  "#": (output, file, lines, line, column) => {
    const chars = lines[line];

    const next = peek(chars, column + 1);

    if (next === "/") {
      return tokenize_block_comment(output, file, lines, line, column);

    } else {
      // Ignore the rest of the current line
      return tokenize1(output, file, lines, line + 1, 0);
    }
  },

  "\"": (output, file, lines, line, column) => {
  },

  "(": tokenize_syntax,
  ")": tokenize_syntax,
  "[": tokenize_syntax,
  "]": tokenize_syntax,
  "{": tokenize_syntax,
  "}": tokenize_syntax,
  "&": tokenize_syntax,
  "~": tokenize_syntax,
  "@": tokenize_syntax,
  ".": tokenize_syntax
};


const tokenize1 = (output, file, lines, line, column) => {
  for (;;) {
    const chars = peek(lines, line);

    if (chars !== null) {
      const char = peek(chars, column);

      if (char !== null) {
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


const x = tokenize("1 1.5 1,5 (foo bar qux) u@q\nqux\n(nou)\n~\n~foo\n~@foo", "NUL");

console.log(x);


/*console.log(format_error(tokenize("\n  a\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  ab\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  abc\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n    abcd\n\n", "maybe.nul")[0], "undefined variable foo"));
*/

console.log(tokenize("\n\n    \t  \n", "foo.nul"));
console.log(tokenize("#/hiya#/#/#/#/1", "foo.nul"));
