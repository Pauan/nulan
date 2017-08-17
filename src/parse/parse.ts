import { Loc, concat } from "../util/loc";
import { NulanError, assertExists } from "../util/error";
import { errorMissing } from "./error";
import * as $ast from "./ast";


type Associativity = "left" | "right";

type Priority = number;

type TokenInfo = {
  priority: Priority,
  parse: (state: ParserState, output: Array<$ast.AST>, token: $ast.Token) => void
};

type ParserState = {
  index: number,
  input: Array<$ast.Token>
};


function peek(state: ParserState): $ast.Token | null {
  const index = state.index;
  const input = state.input;

  if (index < input.length) {
    return input[index];

  } else {
    return null;
  }
}

function next(state: ParserState): void {
  ++state.index;
}


function lookup(token: $ast.Token): TokenInfo {
  if (token.type === "symbol") {
    const special = specials[token.value];

    if (special != null) {
      return special;
    }
  }

  return defaultParser;
}


function parse1(state: ParserState, output: Array<$ast.AST>, priority: Priority): void {
  for (;;) {
    const token = peek(state);

    if (token == null) {
      break;

    } else {
      const info = lookup(token);

      if (priority >= info.priority) {
        next(state);
        info.parse(state, output, token);

      } else {
        break;
      }
    }
  }
}


export function parse(tokens: Array<$ast.Token>): Array<$ast.AST> {
  const state = {
    index: 0,
    input: tokens
  };

  const output: Array<$ast.AST> = [];

  for (;;) {
    const token = peek(state);

    if (token == null) {
      return output;

    } else {
      next(state);
      lookup(token).parse(state, output, token);
    }
  }
}


function parseBracket(state: ParserState, start: $ast.Token, end: string, fn: (args: Array<$ast.AST>, token: $ast.Token) => void): void {
  const args: Array<$ast.AST> = [];

  for (;;) {
    const token = peek(state);

    if (token == null) {
      throw errorMissing(start.loc, "ending " + end);

    } else {
      next(state);

      if (token.type === "symbol" && token.value === end) {
        fn(args, token);
        break;

      } else {
        lookup(token).parse(state, args, token);
      }
    }
  }
}


function parsePrefix(state: ParserState, priority: Priority, middle: $ast.Token, output: Array<$ast.AST>, make: (ast: $ast.AST, loc: Loc) => $ast.AST): void {
  const a: Array<$ast.AST> = [];

  parse1(state, a, priority);

  const length = a.length;

  if (length === 0) {
    throw new NulanError(middle.loc, "There must be an expression on the right of " + middle.value);

  } else {
    const right: $ast.AST = a[0];

    output.push(make(right, concat(middle.loc, right.loc)));

    for (let i = 1; i < length; ++i) {
      output.push(a[i]);
    }
  }
}


function match(priority: Priority, end: string, make: (args: Array<$ast.AST>, body: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, start: $ast.Token): void => {
      parseBracket(state, start, end, (args, middle) => {
        parsePrefix(state, priority, middle, output, (right, loc) => {
          return make(args, right, concat(start.loc, loc));
        });
      });
    }
  };
}


function startBracket(priority: Priority, end: string, make: (args: Array<$ast.AST>, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, start: $ast.Token): void => {
      parseBracket(state, start, end, (args, token) => {
        output.push(make(args, concat(start.loc, token.loc)));
      });
    }
  };
}


function endBracket(priority: Priority, start: string): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, token: $ast.Token): void => {
      throw errorMissing(token.loc, "starting " + start);
    }
  };
}


function single(priority: Priority, fn: (loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, token: $ast.Token): void => {
      output.push(fn(token.loc));
    }
  };
}


function group(priority: Priority): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, middle: $ast.Token): void => {
      parsePrefix(state, priority, middle, output, (ast, loc) => {
        if (ast.type === "call") {
          if (ast.args.length === 1) {
            const x = ast.args[0];
            // TODO don't use mutation ?
            x.loc = loc;
            return x;

          } else {
            throw new NulanError(loc, "There must be exactly one expression inside ^()");
          }

        } else {
          throw new NulanError(middle.loc, "There must be a ( after ^");
        }
      });
    }
  };
}


function prefix(priority: Priority, make: (ast: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, middle: $ast.Token): void => {
      parsePrefix(state, priority, middle, output, make);
    }
  };
}


export function infix(priority: Priority, assoc: Associativity, make: (left: $ast.AST, right: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, output: Array<$ast.AST>, middle: $ast.Token): void => {
      if (output.length === 0) {
        throw new NulanError(middle.loc, "There must be an expression on the left of " + middle.value);

      } else {
        const left: $ast.AST = <$ast.AST>output.pop();

        parsePrefix(state, (assoc === "right" ? priority : priority - 1), middle, output, (right, loc) => {
          return make(left, right, concat(left.loc, loc));
        });
      }
    }
  };
}


export const specials: { [key: string]: TokenInfo } = Object.create(null);

specials[")"] = endBracket(3, "(");
specials["]"] = endBracket(3, "[");
specials["}"] = endBracket(3, "{");
specials[":"] = endBracket(3, "|");

specials["&"] = prefix(2, $ast.quote);

specials["|"] = match(2, ":", $ast.match);

specials["<="] = infix(2, "right", $ast.assign);
specials["::"] = infix(2, "right", $ast.type);

specials["."] = infix(2, "left", (left, right, loc) => {
  if (left.type === "integer" && right.type === "integer") {
    return $ast.float(left.value + "." + right.value, loc);

  } else {
    return $ast.dot(left, right, loc);
  }
});

specials["^"] = group(1);

specials["~"] = prefix(1, $ast.unquote);
specials["@"] = prefix(1, $ast.splice);
specials["%"] = prefix(1, $ast.implicit);

specials["_"] = single(0, $ast.wildcard);

specials["("] = startBracket(0, ")", $ast.call);
specials["["] = startBracket(0, "]", $ast.array);
specials["{"] = startBracket(0, "}", $ast.record);

const defaultParser: TokenInfo = {
  priority: 0,
  parse: (state: ParserState, output: Array<$ast.AST>, token: $ast.Token): void => {
    output.push(token);
  }
};
