import { symbol, integer, string } from "./types";
import { crash } from "./error";
import { repeat } from "./string";


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


const consume_spaces = (file, lines, line, column) => {
  const chars = lines[line];

  const start = { line, column };

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === " ") {
      column += 1;

    } else if (char === null) {
      const end = { line, column };

      crash(symbol(" ", file, lines, start, end),
            "spaces (U+0020) are not allowed at the end of the line");

    } else {
      return column;
    }
  }
};


const tokenize_escape = (value, file, lines, line, column) => {
  const chars = lines[line];

  const start = { line, column };

  column += 1;

  const char = peek(chars, column);

  if (char === "\\" || char === "\"") {
    column += 1;
    value["push"](char);

  } else if (char === "s") {
    column += 1;
    value["push"](" ");

  } else if (char === "n") {
    column += 1;
    value["push"]("\n");

  } else if (char === "u") {
    column += 1;

    const char = peek(chars, column);

    if (char === "{") {
      column += 1;

      let hex = "";

      for (;;) {
        const start = { line, column };

        const char = peek(chars, column);

        if (char === "}") {
          column += 1;

          if (hex === "") {
            const end = { line, column };

            crash(symbol("}", file, lines, start, end),
                  "expected [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got }");

          } else {
            value["push"](String["fromCodePoint"](parseInt(hex, 16)));
            break;
          }

        // TODO a tiny bit hacky
        } else if (/^[0-9A-F]$/["test"](char)) {
          column += 1;
          hex += char;

        } else {
          column += 1;

          const end = { line, column };

          const char2 = (char === null
                          ? ""
                          : char);

          crash(symbol(char2, file, lines, start, end),
                "expected [0 1 2 3 4 5 6 7 8 9 A B C D E F }] but got " + char2);
        }
      }

    } else {
      column += 1;

      const end = { line, column };

      const char2 = (char === null
                      ? ""
                      : char);

      crash(symbol("\\u" + char2, file, lines, start, end),
            "expected \\u{ but got \\u" + char2);
    }

  } else {
    column += 1;

    const end = { line, column };

    const char2 = (char === null
                    ? ""
                    : char);

    crash(symbol("\\" + char2, file, lines, start, end),
          "expected [\\\\ \\\" \\s \\n \\u] but got \\" + char2);
  }

  return column;
};

const tokenize_string = (output, file, lines, line, column) => {
  const start = { line, column };

  column += 1;

  const end = { line, column };

  const indent = end.column;

  const value = [];

  for (;;) {
    const chars = peek(lines, line);

    if (chars !== null) {
      const char = peek(chars, column);

      if (char === "\"") {
        column += 1;

        const end = { line, column };

        output["push"](string(value["join"](""), file, lines, start, end));

        return tokenize1(output, file, lines, line, column);

      } else if (char === "\\") {
        column = tokenize_escape(value, file, lines, line, column);

      } else if (char === " ") {
        const start_column = column;

        column = consume_spaces(file, lines, line, column);

        value["push"](repeat(" ", column - start_column));

      } else if (char === null) {
        line += 1;
        column = 0;

        value["push"]("\n"); // TODO eol ?

        const chars = peek(lines, line);

        // TODO is this correct ?
        if (chars !== null && peek(chars, column) !== null) {
          column = consume_spaces(file, lines, line, column);

          if (column < indent) {
            const start = { line, column: 0 };
            const end   = { line, column };

            crash(symbol(" ", file, lines, start, end),
                  "there must be " + indent + " or more spaces (U+0020)");

          } else {
            value["push"](repeat(" ", column - indent));
          }
        }

      } else {
        column += 1;

        value["push"](char);
      }

    } else {
      crash(symbol("\"", file, lines, start, end),
            "missing ending \"");
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
      crash(pending[pending["length"] - 1],
            "missing ending /#");
    }
  }
};

// TODO handle spaces at the end of the line
const tokenize_comment = (output, file, lines, line, column) => {
  const chars = lines[line];

  const next = peek(chars, column + 1);

  if (next === "/") {
    return tokenize_block_comment(output, file, lines, line, column);

  } else {
    // Ignore the rest of the current line
    return tokenize1(output, file, lines, line + 1, 0);
  }
};


const tokenize_tab = (output, file, lines, line, column) => {
  const start = { line, column };

  column += 1;

  const end = { line, column };

  crash(symbol("\t", file, lines, start, end),
        "tabs (U+0009) are not allowed");
};


const tokenize_space = (output, file, lines, line, column) => {
  column = consume_spaces(file, lines, line, column);
  return tokenize1(output, file, lines, line, column);
};


const specials = {
  "\t": tokenize_tab,
  " ":  tokenize_space,
  "#":  tokenize_comment,
  "\"": tokenize_string,

  "(":  tokenize_syntax,
  ")":  tokenize_syntax,
  "[":  tokenize_syntax,
  "]":  tokenize_syntax,
  "{":  tokenize_syntax,
  "}":  tokenize_syntax,
  "&":  tokenize_syntax,
  "~":  tokenize_syntax,
  "@":  tokenize_syntax,
  ".":  tokenize_syntax
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

export const tokenize = (lines, file) =>
  tokenize1([], file, lines, 0, 0);


//const x = tokenize("1 1.5 1,5 (foo bar qux) u@q\nqux\n(nou)\n~\n~foo\n~@foo", "NUL");

//console.log(x);


/*console.log(format_error(tokenize("\n  a\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  ab\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n  abc\n\n", "maybe.nul")[0], "undefined variable foo"));
console.log(format_error(tokenize("\n    abcd\n\n", "maybe.nul")[0], "undefined variable foo"));
crash(tokenize("\uD834\uDF06", "foo.nul")[0], "Hi");
*/

//console.log(tokenize("\"\\u{}\"", "foo.nul"));
//console.log(tokenize("\"\\u{a\"", "foo.nul"));
//console.log(tokenize("\"\\u{a}\"", "foo.nul"));
//console.log(tokenize("\"\\u{21}\\u{1D306}\"", "foo.nul"));
//console.log(tokenize("  \"foobar     \"", "foo.nul"));
//console.log(tokenize("  \"foobar\n\n     \"", "foo.nul"));
//console.log(tokenize("  \"foobar\n    \n\"", "foo.nul"));
//console.log(tokenize("   \"foobar\n    \n\"", "foo.nul"));
//console.log(tokenize("   \"foobar\n    qux\"a", "foo.nul"));
//console.log(tokenize("a\rb\nc\r\nd", "foo.nul"));
//console.log(tokenize("   a\n \n", "foo.nul"));
//console.log(tokenize("   a     ", "foo.nul"));
//console.log(tokenize("\n\n    a\ta  \n", "foo.nul"));
//console.log(tokenize("  #/hiya\n\n\n\n  1", "foo.nul"));
