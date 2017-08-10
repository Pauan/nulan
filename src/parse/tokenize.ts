import { NulanError } from "../util/error";
import { Loc, Position } from "../util/loc";
import * as $string from "../util/string";
import { Token } from "./ast";


function errorBlockComment(state: TokenState, pos: Position, start: string): NulanError {
  return new NulanError(loc(state, pos, position(state)),
    "Missing ending /" + start);
}

function errorStringMissing(state: TokenState, pos: Position, delimiter: string): NulanError {
  return new NulanError(loc(state, pos, position(state)),
    "Missing ending " + delimiter);
}

function errorStringIndentation(state: TokenState, pos: Position, expected: number, actual: number): NulanError {
  return new NulanError(loc(state, pos, position(state)),
    (expected === 1
      ? "At least 1 space is required, but there "
      : "At least " + expected + " spaces are required, but there ") +
    (actual === 1
      ? "is 1"
      : "are " + actual));
}

function errorExtraSpaces(state: TokenState, pos: Position, actual: number): NulanError {
  return new NulanError(loc(state, pos, position(state)),
    "Spaces are not allowed at the end of the line, but there " +
    (actual === 1
      ? "is 1"
      : "are " + actual));
}

function errorStringUnknown(state: TokenState, pos: Position, char: string): NulanError {
  return new NulanError(loc(state, pos, position(state)),
    "Invalid \\" + $string.prettyCharacter(char) + ", it must be one of the following: " +
    Object.keys(specialStringEscapes)
      .filter((x) => (x !== "\r")) // TODO a tiny bit hacky
      .sort($string.order)
      .map((x) => "\\" + $string.prettyCharacter(x))
      .join(" "));
}

function errorTab(state: TokenState, pos: Position): NulanError {
  return new NulanError(loc(state, pos, position(state)), "Invalid tab (U+0009)");
}


type Specials = {
  [key: string]: (state: TokenState, output: Array<Token>, char: string) => void
};

type Escapes = {
  [key: string]: (state: TokenState, pos: Position, chars: Array<string>, delimiter: string, start: Position, char: string) => void
};

type TokenState = {
  input: string,
  filename: string,
  index: number,
  line: number,
  column: number
};


function peek(state: TokenState): string | null {
  const input = state.input;
  const index = state.index;

  if (index < input.length) {
    return input[index];

  } else {
    return null;
  }
}


function position(state: TokenState): Position {
  return {
    index: state.index,
    line: state.line,
    column: state.column
  };
}

function loc(state: TokenState, start: Position, end: Position): Loc {
  return {
    filename: state.filename,
    start: start,
    end: end
  };
}


function incrementLine(state: TokenState): void {
  ++state.index;
  ++state.line;
  state.column = 0;
}

function incrementColumn(state: TokenState): void {
  ++state.index;
  ++state.column;
}

function incrementCharacter(state: TokenState, char: string): void {
  if (char === "\n") {
    incrementLine(state);

    if (peek(state) === "\r") {
      ++state.index;
    }

  } else if (char === "\r") {
    incrementLine(state);

    if (peek(state) === "\n") {
      ++state.index;
    }

  } else if (char === " ") {
    consumeSpacesError(state);

  } else if (char === "\t") {
    const start = position(state);
    incrementColumn(state);
    throw errorTab(state, start);

  } else {
    incrementColumn(state);
  }
}

function consumeSpaces(state: TokenState): { spaces: number, next: string | null } {
  let i = 0;

  for (;;) {
    const char = peek(state);

    if (char === " ") {
      incrementColumn(state);
      ++i;

    } else {
      return {
        spaces: i,
        next: char
      };
    }
  }
}

function consumeSpacesError(state: TokenState) {
  const pos = position(state);

  incrementColumn(state);

  const x = consumeSpaces(state);

  if (x.next == null || x.next === "\n" || x.next === "\r") {
    throw errorExtraSpaces(state, pos, x.spaces + 1);
  }
}


function tokenizeIdentifier(state: TokenState, output: Array<Token>, char: string): void {
  const chars = [char];

  const start = position(state);

  incrementColumn(state);

  for (;;) {
    const char = peek(state);

    if (char == null) {
      break;

    // TODO make this more efficient, so tokenize1 doesn't need to loop again
    } else if (specials[char] != null) {
      break;

    } else {
      incrementColumn(state);
      chars.push(char);
    }
  }

  const end = position(state);

  const str = chars.join("");

  // TODO more efficient check
  if (/^[0-9]+$/.test(str)) {
    output.push({
      type: "integer",
      value: str,
      loc: loc(state, start, end)
    });

  } else {
    output.push({
      type: "symbol",
      value: str,
      loc: loc(state, start, end)
    });
  }
}


function tokenize1(state: TokenState, output: Array<Token>): void {
  for (;;) {
    const char = peek(state);

    if (char == null) {
      return;

    } else {
      const special = specials[char];

      if (special == null) {
        tokenizeIdentifier(state, output, char);

      } else {
        special(state, output, char);
      }
    }
  }
}


function specialLineComment(state: TokenState, char: string | null): void {
  for (;;) {
    if (char == null) {
      break;

    } else if (char === "\n" || char === "\r") {
      incrementCharacter(state, char);
      break;

    } else {
      incrementCharacter(state, char);
      char = peek(state);
    }
  }
}

function incrementBlockCharacter(state: TokenState, pos: Position, start: string, char: string): void {
  // TODO code duplication with consumeSpacesError
  if (char === " ") {
    const pos = position(state);

    incrementColumn(state);

    const x = consumeSpaces(state);

    if (x.next == null) {
      throw errorBlockComment(state, pos, start);

    } else if (x.next === "\n" || x.next === "\r") {
      throw errorExtraSpaces(state, pos, x.spaces + 1);
    }

  } else {
    incrementCharacter(state, char);
  }
}

function specialBlockComment(state: TokenState, pos: Position, start: string): void {
  incrementColumn(state);

  for (;;) {
    const char = peek(state);

    if (char == null) {
      throw errorBlockComment(state, pos, start);

    } else if (char === "/") {
      incrementColumn(state);

      const char = peek(state);

      if (char == null) {
        throw errorBlockComment(state, pos, start);

      } else if (char === start) {
        incrementColumn(state);
        break;

      } else {
        incrementBlockCharacter(state, pos, start, char);
      }

    } else if (char === start) {
      const newPos = position(state);

      incrementColumn(state);

      const char = peek(state);

      if (char == null) {
        throw errorBlockComment(state, pos, start);

      } else if (char === "/") {
        specialBlockComment(state, newPos, start);

      } else {
        incrementBlockCharacter(state, pos, start, char);
      }

    } else {
      incrementBlockCharacter(state, pos, start, char);
    }
  }
}

function specialComment(state: TokenState, output: Array<Token>, start: string): void {
  const pos = position(state);

  incrementColumn(state);

  const char = peek(state);

  if (char === "/") {
    specialBlockComment(state, pos, start);

  } else {
    specialLineComment(state, char);
  }
}

function specialIncrement(state: TokenState, output: Array<Token>, char: string): void {
  incrementCharacter(state, char);
}

function specialCharacter(state: TokenState, output: Array<Token>, char: string): void {
  const start = position(state);

  incrementColumn(state);

  const end = position(state);

  output.push({
    type: "symbol",
    value: char,
    loc: loc(state, start, end)
  });
}


function specialStringEscape(state: TokenState, pos: Position, chars: Array<string>, delimiter: string): void {
  const start = position(state);

  incrementColumn(state);

  const char = peek(state);

  if (char == null) {
    throw errorStringMissing(state, pos, delimiter);

  } else {
    const escape = specialStringEscapes[char];

    if (escape == null) {
      incrementCharacter(state, char);
      throw errorStringUnknown(state, start, char);

    } else {
      escape(state, pos, chars, delimiter, start, char);
    }
  }
}

function incrementStringSpaces(state: TokenState, start: Position, delimiter: string, pos: Position): number | null {
  const x = consumeSpaces(state);

  if (x.next == null) {
    throw errorStringMissing(state, start, delimiter);

  } else if (x.next === "\n" || x.next === "\r") {
    if (x.spaces === 0) {
      return null;

    } else {
      throw errorExtraSpaces(state, pos, x.spaces);
    }

  } else {
    return x.spaces;
  }
}

function pushStringSpaces(state: TokenState, start: Position, delimiter: string, chars: Array<string>): void {
  const amount = start.column + 1;

  const pos = position(state);

  const spaces = incrementStringSpaces(state, start, delimiter, pos);

  if (spaces != null) {
    if (spaces < amount) {
      throw errorStringIndentation(state, pos, amount, spaces);

    } else if (spaces > amount) {
      chars.push($string.repeat(" ", spaces - amount));
    }
  }
}

function specialString(state: TokenState, output: Array<Token>, delimiter: string): void {
  const chars = [];

  const start = position(state);

  incrementColumn(state);

  for (;;) {
    const char = peek(state);

    if (char == null) {
      throw errorStringMissing(state, start, delimiter);

    } else if (char === delimiter) {
      incrementColumn(state);

      output.push({
        type: "string",
        value: chars.join(""),
        loc: loc(state, start, position(state))
      });

      break;

    } else if (char === "\\") {
      specialStringEscape(state, start, chars, delimiter);

    } else if (char === "\n" || char === "\r") {
      chars.push("\n");

      incrementCharacter(state, char);

      pushStringSpaces(state, start, delimiter, chars);

    } else if (char === " ") {
      const pos = position(state);

      const spaces = incrementStringSpaces(state, start, delimiter, pos);

      // TODO hacky cast needed to avoid a code coverage warning
      chars.push($string.repeat(" ", spaces as number));

    } else {
      chars.push(char);
      incrementCharacter(state, char);
    }
  }
}


function specialStringEscapeChar(output: string) {
  return function (state: TokenState, pos: Position, chars: Array<string>, delimiter: string, start: Position, char: string): void {
    incrementColumn(state);
    chars.push(output);
  };
}

function specialStringEscapeNewline(state: TokenState, pos: Position, chars: Array<string>, delimiter: string, start: Position, char: string): void {
  incrementCharacter(state, char);
  pushStringSpaces(state, pos, delimiter, chars);
}

function specialStringEscapeUnicode(state: TokenState, pos: Position, chars: Array<string>, delimiter: string, start: Position, char: string): void {
}

const specialStringEscapes: Escapes = Object.create(null);
specialStringEscapes["\""] = specialStringEscapeChar("\"");
specialStringEscapes["\\"] = specialStringEscapeChar("\\");
specialStringEscapes["t"] = specialStringEscapeChar("\t");
specialStringEscapes["n"] = specialStringEscapeChar("\n");
specialStringEscapes["r"] = specialStringEscapeChar("\r");
specialStringEscapes["u"] = specialStringEscapeUnicode;
specialStringEscapes["\n"] = specialStringEscapeNewline;
specialStringEscapes["\r"] = specialStringEscapeNewline;


const specials: Specials = Object.create(null);
specials[" "] = specialIncrement;
specials["\t"] = specialIncrement;
specials["\r"] = specialIncrement;
specials["\n"] = specialIncrement;
specials["#"] = specialComment;
specials["\""] = specialString;
specials["."] = specialCharacter;
specials["("] = specialCharacter;
specials[")"] = specialCharacter;
specials["["] = specialCharacter;
specials["]"] = specialCharacter;
specials["{"] = specialCharacter;
specials["}"] = specialCharacter;
specials["@"] = specialCharacter;
specials["&"] = specialCharacter;
specials["~"] = specialCharacter;


export function tokenize(input: string, filename: string): Array<Token> {
  const output: Array<Token> = [];

  tokenize1({
    input: input,
    filename: filename,
    index: 0,
    line: 0,
    column: 0
  }, output);

  return output;
}
