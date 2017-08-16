import { Loc, concat } from "../util/loc";
import { NulanError, assertExists } from "../util/error";
import { errorMissing } from "./error";
import * as $ast from "./ast";


type Associativity = "left" | "right";

type Priority = number;

type TokenInfo = {
  priority: Priority,
  prefix: (state: ParserState, token: $ast.Token) => $ast.AST,
  infix?: (state: ParserState, left: $ast.AST, token: $ast.Token) => $ast.AST
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


function unexpected(state: ParserState, left: $ast.AST, token: $ast.Token): $ast.AST {
  throw new NulanError(token.loc, "Cannot have an expression on the left of " + token.value);
}


const defaultParser: TokenInfo = {
  priority: 0,
  prefix: (state: ParserState, token: $ast.Token): $ast.AST => {
    return token;
  },
  infix: unexpected
};


function lookup(token: $ast.Token): TokenInfo {
  if (token.type === "symbol") {
    const special = specials[token.value];

    if (special != null) {
      return special;
    }
  }

  return defaultParser;
}


function parse1(state: ParserState, priority: Priority, token: $ast.Token): $ast.AST {
  let info = lookup(token);

  let ast: $ast.AST = info.prefix(state, token);

  for (;;) {
    const token = peek(state);

    if (token == null) {
      return ast;

    } else {
      info = lookup(token);

      if (info.infix != null && priority < info.priority) {
        next(state);
        ast = info.infix(state, ast, token);

      } else {
        return ast;
      }
    }
  }
}


function parseAll(state: ParserState, priority: Priority): Array<$ast.AST> {
  const args: Array<$ast.AST> = [];

  for (;;) {
    const token = peek(state);

    if (token == null) {
      return args;

    } else {
      next(state);
      args.push(parse1(state, priority, token));
    }
  }
}


export function parse(tokens: Array<$ast.Token>): Array<$ast.AST> {
  return parseAll({
    index: 0,
    input: tokens
  }, 0);
}


function parseStartBracket(end: string, make: (args: Array<$ast.AST>, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: Infinity,
    prefix: (state: ParserState, start: $ast.Token): $ast.AST => {
      const args: Array<$ast.AST> = [];

      for (;;) {
        const token = peek(state);

        if (token == null) {
          throw errorMissing(start.loc, "ending " + end);

        } else {
          next(state);

          if (token.type === "symbol" && token.value === end) {
            return make(args, concat(start.loc, token.loc));

          } else {
            args.push(parse1(state, 0, token));
          }
        }
      }
    }
  };
}


function parseEndBracket(start: string): TokenInfo {
  return {
    priority: 0,
    prefix: (state: ParserState, token: $ast.Token): $ast.AST => {
      throw errorMissing(token.loc, "starting " + start);
    }
  };
}


function parseSingle(fn: (loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: 0,
    prefix: (state: ParserState, token: $ast.Token): $ast.AST => {
      return fn(token.loc);
    }
  };
}


function parseRight(state: ParserState, priority: Priority, assoc: Associativity, middle: $ast.Token): $ast.AST {
  const token = peek(state);

  if (token == null) {
    throw new NulanError(middle.loc, "There must be an expression on the right of " + middle.value);

  } else {
    next(state);

    return parse1(state, (assoc === "left" ? priority : priority - 1), token);
  }
}


function parsePrefix(priority: Priority, make: (ast: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    prefix: (state: ParserState, middle: $ast.Token): $ast.AST => {
      const right = parseRight(state, priority, "right", middle);

      return make(right, concat(middle.loc, right.loc));
    }
  };
}


function parseInfix(priority: Priority, assoc: Associativity, make: (left: $ast.AST, right: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    prefix: (state: ParserState, middle: $ast.Token): $ast.AST => {
      throw new NulanError(middle.loc, "There must be an expression on the left of " + middle.value);
    },
    infix: (state: ParserState, left: $ast.AST, middle: $ast.Token): $ast.AST => {
      const right = parseRight(state, priority, assoc, middle);

      return make(left, right, concat(left.loc, right.loc));
    }
  };
}


function parseRest(priority: Priority, make: (args: Array<$ast.AST>, loc: Loc) => $ast.AST): TokenInfo {
  return {
    priority: priority,
    prefix: (state: ParserState, start: $ast.Token): $ast.AST => {
      const args = parseAll(state, priority);

      if (args.length === 0) {
        return make(args, start.loc);

      } else {
        return make(args, concat(start.loc, args[args.length - 1].loc));
      }
    }
  };
}


const specials: { [key: string]: TokenInfo } = Object.create(null);

specials["^"] = parsePrefix(Infinity, (ast: $ast.AST, loc: Loc) => {
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
    throw new NulanError(loc, "There must be a ( after ^");
  }
});

specials["("] = parseStartBracket(")", $ast.call);
specials["["] = parseStartBracket("]", $ast.array);
specials["{"] = parseStartBracket("}", $ast.record);

specials[")"] = parseEndBracket("(");
specials["]"] = parseEndBracket("[");
specials["}"] = parseEndBracket("{");

specials["_"] = parseSingle($ast.wildcard);

specials["|"] = parseRest(6, $ast.bar);

specials[":"] = parseInfix(7, "right", $ast.match);

specials["&"] = parsePrefix(3, $ast.quote);

specials["<="] = parseInfix(4, "right", $ast.assign);
specials["::"] = parseInfix(4, "right", $ast.type);

specials["."] = parseInfix(4, "left", (left, right, loc) => {
  if (left.type === "integer" && right.type === "integer") {
    return $ast.float(left.value + "." + right.value, loc);

  } else {
    return $ast.dot(left, right, loc);
  }
});

specials["~"] = parsePrefix(5, $ast.unquote);
specials["@"] = parsePrefix(5, $ast.splice);
