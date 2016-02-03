import { error } from "./error";
import { repeat } from "../../util/string";
import { peek } from "../../util/array";
import * as $ast from "./ast";


const tokenize_delimiter = (output, file, lines, line, column) => {
  const char = lines[line][column];

  const start = { line, column };

  column += 1;

  const end = { line, column };

  output["push"]($ast.symbol(char, $ast.loc(file, lines, start, end)));

  return tokenize1(output, file, lines, line, column);
};


const tokenize_symbol1 = (value, loc) => {
  // TODO a tiny bit hacky
  if (/^[0-9]+$/["test"](value)) {
    return $ast.integer(+value, loc);

  } else {
    return $ast.symbol(value, loc);
  }
};

const consume_symbol = (output, file, lines, line, column, f) => {
  const start = { line, column };

  const chars = lines[line];

  let value = chars[column];

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === null || specials[char] != null) {
      const end = { line, column };
      output["push"](f(value, $ast.loc(file, lines, start, end)));
      return tokenize1(output, file, lines, line, column);

    } else {
      value += char;
      column += 1;
    }
  }
};

const tokenize_symbol = (output, file, lines, line, column) =>
  consume_symbol(output, file, lines, line, column, tokenize_symbol1);


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

      error($ast.symbol(" ", $ast.loc(file, lines, start, end)),
            "spaces (U+0020) are not allowed at the end of the line");

    } else {
      return column;
    }
  }
};


const consume_hex = (output, file, lines, line, column) => {
  const chars = lines[line];

  let hex = "";

  for (;;) {
    const char = peek(chars, column);

    if (/^[0-9A-F]$/["test"](char)) {
      column += 1;
      hex += char;

    } else if (hex === "") {
      const start = { line, column };

      if (char !== null) {
        column += 1;
      }

      const end = { line, column };

      const char2 = (char === null
                      ? "#<EOL>"
                      : char);

      error($ast.symbol(char2, $ast.loc(file, lines, start, end)),
            "expected one of [0 1 2 3 4 5 6 7 8 9 A B C D E F] but got " + char2);

    } else {
      output["push"](hex);
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

    if (char === "[") {
      column += 1;

      const hexes = [];

      for (;;) {
        column = consume_hex(hexes, file, lines, line, column);

        const char = peek(chars, column);

        if (char === "]") {
          column += 1;

          for (let i = 0; i < hexes["length"]; ++i) {
            value["push"](String["fromCodePoint"](parseInt(hexes[i], 16)));
          }

          break;

        } else if (char === " ") {
          column += 1;
        }
      }

    } else if (char === null) {
      const end = { line, column };

      error($ast.symbol("\\u", $ast.loc(file, lines, start, end)),
            "expected \\u[ but got \\u");

    } else {
      column += 1;

      const end = { line, column };

      error($ast.symbol("\\u" + char, $ast.loc(file, lines, start, end)),
            "expected \\u[ but got \\u" + char);
    }

  } else if (char === null) {
    const end = { line, column };

    error($ast.symbol("\\", $ast.loc(file, lines, start, end)),
          "expected one of [\\\\ \\\" \\s \\n \\u] but got \\");

  } else {
    column += 1;

    const end = { line, column };

    error($ast.symbol("\\" + char, $ast.loc(file, lines, start, end)),
          "expected one of [\\\\ \\\" \\s \\n \\u] but got \\" + char);
  }

  return column;
};

const indent_error = (indent, file, lines, line, column1, column2) => {
  const start = { line, column: column1 };
  const end   = { line, column: column2 };

  error($ast.symbol(" ", $ast.loc(file, lines, start, end)),
        "there must be " + indent + " or more spaces (U+0020)");
};

const tokenize_text = (output, file, lines, line, column) => {
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

        output["push"]($ast.text(value["join"](""), $ast.loc(file, lines, start, end)));

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

        if (chars !== null) {
          const char = peek(chars, column);

          if (char === " ") {
            column = consume_spaces(file, lines, line, column);

            if (column < indent) {
              indent_error(indent, file, lines, line, 0, column);

            } else {
              value["push"](repeat(" ", column - indent));
            }

          } else if (char !== null) {
            indent_error(indent, file, lines, line, 0, 1);
          }
        }

      } else {
        column += 1;

        value["push"](char);
      }

    } else {
      error($ast.symbol("\"", $ast.loc(file, lines, start, end)),
            "missing ending \"");
    }
  }
};


const tokenize_block_comment = (output, file, lines, line, column) => {
  const pending = [];

  const start = { line, column };

  column += 2;

  const end = { line, column };

  pending["push"]($ast.symbol("#/", $ast.loc(file, lines, start, end)));

  for (;;) {
    const chars = peek(lines, line);

    if (chars !== null) {
      const next1 = peek(chars, column);

      if (next1 === " ") {
        column = consume_spaces(file, lines, line, column);

      } else if (next1 === null) {
        line += 1;
        column = 0;

      } else {
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

          pending["push"]($ast.symbol("#/", $ast.loc(file, lines, start, end)));

        } else {
          column += 1;
        }
      }

    } else {
      error(pending[pending["length"] - 1],
            "missing ending /#");
    }
  }
};

const tokenize_line_comment = (output, file, lines, line, column) => {
  const chars = lines[line];

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === " ") {
      column = consume_spaces(file, lines, line, column);

    } else if (char === null) {
      return tokenize1(output, file, lines, line, column);

    } else {
      column += 1;
    }
  }
};

const tokenize_comment = (output, file, lines, line, column) => {
  const chars = lines[line];

  const next = peek(chars, column + 1);

  if (next === "/") {
    return tokenize_block_comment(output, file, lines, line, column);

  } else {
    return tokenize_line_comment(output, file, lines, line, column);
  }
};


const tokenize_tab = (output, file, lines, line, column) => {
  const chars = lines[line];

  const start = { line, column };

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === "\t") {
      column += 1;

    } else {
      const end = { line, column };

      error($ast.symbol("\t", $ast.loc(file, lines, start, end)),
            "tabs (U+0009) are not allowed");
    }
  }
};


const tokenize_space = (output, file, lines, line, column) => {
  column = consume_spaces(file, lines, line, column);
  return tokenize1(output, file, lines, line, column);
};


const specials = {
  "\t": tokenize_tab,
  " ":  tokenize_space,
  "#":  tokenize_comment,
  "\"": tokenize_text,

  "(":  tokenize_delimiter,
  ")":  tokenize_delimiter,

  "[":  tokenize_delimiter,
  "]":  tokenize_delimiter,

  "{":  tokenize_delimiter,
  "}":  tokenize_delimiter,

  "&":  tokenize_delimiter,
  "~":  tokenize_delimiter,
  "@":  tokenize_delimiter,
  ".":  tokenize_delimiter
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
        line += 1;
        column = 0;
      }

    } else {
      return output;
    }
  }
};

export const tokenize = (lines, file) =>
  tokenize1([], file, lines, 0, 0);
