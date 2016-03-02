import { error } from "./error";
import { repeat } from "../../util/string";
import { peek } from "../../util/array";
import * as $ast from "./type";


const PREFIX = 0;
const INFIX  = 1;
const SUFFIX = 2;


const tokenize_delimiter1 = (output, file, lines, line, column, indents, whitespace, space) => {
  const chars = lines[line];

  const prev = peek(chars, column - 1);
  const next = peek(chars, column + 1);

  const char = chars[column];

  const start = { line, column };

  column += 1;

  const end = { line, column };

  const token = $ast.symbol(char, $ast.loc(file, lines, start, end));

  // TODO hacky
  if (whitespace === INFIX || whitespace === SUFFIX) {
    if (prev === null) {
      error(token, "missing expression on the left side of " + char);

    } else if (space === null) {
      if (prev === " ") {
        error(token, "spaces (U+0020) are not allowed before " + char);
      }

    } else if (prev !== " " && prev !== space) {
      error(token, "must have a space or " + space + " on the left side of " + char);
    }
  }

  // TODO hacky
  if (whitespace === INFIX || whitespace === PREFIX) {
    if (next === null) {
      error(token, "missing expression on the right side of " + char);

    } else if (space === null) {
      if (next === " ") {
        error(token, "spaces (U+0020) are not allowed after " + char);
      }

    } else if (next !== " " && next !== space) {
      error(token, "must have a space or " + space + " on the right side of " + char);
    }
  }

  // TODO hacky
  if (whitespace === PREFIX && prev !== null && specials[prev] == null) {
    error(token, "cannot have an expression on the left side of " + char);
  }

  // TODO hacky
  if (whitespace === SUFFIX && next !== null && specials[next] == null) {
    error(token, "cannot have an expression on the right side of " + char);
  }

  output["push"](token);

  return tokenize1(output, file, lines, line, column, indents);
};

const tokenize_delimiter = (whitespace) =>
  (output, file, lines, line, column, indents) =>
    tokenize_delimiter1(output, file, lines, line, column, indents, whitespace, null);


const tokenize_start = (whitespace, right, space) =>
  (output, file, lines, line, column, indents) => {
    indents["push"]({
      end: right,
      column: column + 2
    });

    // TODO hacky
    return tokenize_delimiter1(output, file, lines, line, column, indents, whitespace, (space ? right : null));
  };


const tokenize_end = (whitespace, left, space) =>
  (output, file, lines, line, column, indents) => {
    const last = indents[indents["length"] - 1];

    const char = lines[line][column];

    if (last.end !== null && last.end === char) {
      indents["pop"]();

      // TODO hacky
      return tokenize_delimiter1(output, file, lines, line, column, indents, whitespace, (space ? left : null));

    // TODO code duplication
    } else {
      const start = { line, column };

      column += 1;

      const end = { line, column };

      const token = $ast.symbol(char, $ast.loc(file, lines, start, end));

      error(token, "missing starting " + left);
    }
  };


const tokenize_symbol1 = (value, loc) => {
  // TODO a tiny bit hacky
  if (/^[0-9]+$/["test"](value)) {
    return $ast.integer(+value, loc);

  } else {
    return $ast.symbol(value, loc);
  }
};

const consume_symbol = (output, file, lines, line, column, indents, f) => {
  const start = { line, column };

  const chars = lines[line];

  let value = chars[column];

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === null || specials[char] != null) {
      const end = { line, column };
      output["push"](f(value, $ast.loc(file, lines, start, end)));
      return tokenize1(output, file, lines, line, column, indents);

    } else {
      value += char;
      column += 1;
    }
  }
};

const tokenize_symbol = (output, file, lines, line, column, indents) =>
  consume_symbol(output, file, lines, line, column, indents, tokenize_symbol1);


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

const text_indent_error = (indent, file, lines, line, column1, column2) => {
  const start = { line, column: column1 };
  const end   = { line, column: column2 };

  error($ast.symbol(" ", $ast.loc(file, lines, start, end)),
        "there must be " + indent + " or more spaces (U+0020)");
};

const tokenize_text = (output, file, lines, line, column, indents) => {
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

        return tokenize1(output, file, lines, line, column, indents);

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
              text_indent_error(indent, file, lines, line, 0, column);

            } else {
              value["push"](repeat(" ", column - indent));
            }

          } else if (char !== null) {
            text_indent_error(indent, file, lines, line, 0, 1);
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


const tokenize_block_comment = (output, file, lines, line, column, indents) => {
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
        check_indent(file, lines, line, column, indents);

      } else {
        const next2 = peek(chars, column + 1);

        // The comment block has ended
        if (next1 === "/" && next2 === "#") {
          column += 2;
          pending["pop"]();

          if (pending["length"] === 0) {
            return tokenize1(output, file, lines, line, column, indents);
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

const tokenize_line_comment = (output, file, lines, line, column, indents) => {
  const chars = lines[line];

  column += 1;

  for (;;) {
    const char = peek(chars, column);

    if (char === " ") {
      column = consume_spaces(file, lines, line, column);

    } else if (char === null) {
      return tokenize1(output, file, lines, line, column, indents);

    } else {
      column += 1;
    }
  }
};

const tokenize_comment = (output, file, lines, line, column, indents) => {
  const chars = lines[line];

  const next = peek(chars, column + 1);

  if (next === "/") {
    return tokenize_block_comment(output, file, lines, line, column, indents);

  } else {
    return tokenize_line_comment(output, file, lines, line, column, indents);
  }
};


const tokenize_tab = (output, file, lines, line, column, indents) => {
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


const tokenize_space = (output, file, lines, line, column, indents) => {
  column = consume_spaces(file, lines, line, column);
  return tokenize1(output, file, lines, line, column, indents);
};


const specials = {
  "\t": tokenize_tab,
  " ":  tokenize_space,
  "#":  tokenize_comment, // TODO require " " or null to the left ?
  "\"": tokenize_text,

  "(":  tokenize_start(PREFIX, ")", false),
  ")":  tokenize_end(SUFFIX, "(", false),

  "[":  tokenize_start(PREFIX, "]", true),
  "]":  tokenize_end(SUFFIX, "[", true),

  "{":  tokenize_start(PREFIX, "}", true),
  "}":  tokenize_end(SUFFIX, "{", true),

  "&":  tokenize_delimiter(PREFIX),
  ",":  tokenize_delimiter(PREFIX),
  "@":  tokenize_delimiter(PREFIX),
  ".":  tokenize_delimiter(INFIX)
};


const check_indent = (file, lines, line, column, indents) => {
  const chars = peek(lines, line);

  if (chars !== null) {
    const char = peek(chars, column);

    if (char === " ") {
      column = consume_spaces(file, lines, line, column);
    }

    const last = indents[indents["length"] - 1];

    if (last.column !== column) {
      indent_error(file, lines, line, column, last.column);
    }
  }
};

const indent_error = (file, lines, line, column, indent) => {
  error($ast.symbol(" ", $ast.loc(file, lines,
                                  { line, column: 0 },
                                  { line, column })),
        "expected " + indent + " spaces but got " + column);
};

const tokenize1 = (output, file, lines, line, column, indents) => {
  for (;;) {
    const chars = peek(lines, line);

    if (chars !== null) {
      const char = peek(chars, column);

      if (char !== null) {
        if (specials[char] != null) {
          return specials[char](output, file, lines, line, column, indents);

        } else {
          return tokenize_symbol(output, file, lines, line, column, indents);
        }

      } else {
        line += 1;
        column = 0;
        check_indent(file, lines, line, column, indents);
      }

    } else {
      return output;
    }
  }
};

// TODO a tiny bit hacky
const tokenize_top = (output, file, lines, line, column, indents) => {
  check_indent(file, lines, line, column, indents);
  return tokenize1(output, file, lines, line, column, indents);
};

export const tokenize = (lines, file) =>
  tokenize_top([], file, lines, 0, 0, [{ end: null, column: 0 }]);
