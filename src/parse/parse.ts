import { Loc, concat } from "../util/loc";
import { NulanError, assertExists } from "../util/error";
import { errorMissing } from "./error";
import * as $ast from "./ast";


type Associativity = "left" | "right";

type Priority = number;

type TokenInfo = {
  priority: Priority,
  parse: (state: ParserState, token: $ast.Token, output: Array<$ast.AST>) => Array<$ast.AST>
};


type ParserState = {
  index: number,
  input: Array<$ast.Token>,
  brackets: Array<string>
};

function peek(state: ParserState): $ast.Token | null {
  if (state.index < state.input.length) {
    return state.input[state.index];

  } else {
    return null;
  }
}

function next(state: ParserState): void {
  ++state.index;
}


function parse1(state: ParserState, priority: Priority | null, output: Array<$ast.AST>): Array<$ast.AST> {
  for (;;) {
    const token = peek(state);

    if (token == null) {
      return output;

    } else if (token.type === "symbol") {
      const special = specials[token.value];

      if (special == null) {
        next(state);
        output.push(token);

      } else if (priority == null || priority < special.priority) {
        next(state);
        output = special.parse(state, token, output);

      } else {
        return output;
      }

    } else {
      next(state);
      output.push(token);
    }
  }
}

export function parse(tokens: Array<$ast.Token>): Array<$ast.AST> {
  return parse1({
    index: 0,
    input: tokens,
    brackets: []
  }, null, []);
}


function parseStartBracket(end: string, make: (args: Array<$ast.AST>, loc: Loc) => $ast.AST): TokenInfo {
  return {
    // TODO is this the correct priority ?
    priority: Infinity,
    parse: (state: ParserState, start: $ast.Token, output: Array<$ast.AST>): Array<$ast.AST> => {
      let args: Array<$ast.AST> = [];

      for (;;) {
        const token = peek(state);

        if (token == null) {
          throw errorMissing(start.loc, "ending " + end);

        } else if (token.type === "symbol" && token.value === end) {
          next(state);
          output.push(make(args, concat(start.loc, token.loc)));
          return output;

        } else {
          args = parse1(state, null, args);
        }
      }
    }
  };
}

function parseEndBracket(start: string): TokenInfo {
  return {
    // TODO is this the correct priority ?
    priority: -Infinity,
    parse: (state: ParserState, token: $ast.Token, output: Array<$ast.AST>): Array<$ast.AST> => {
      throw errorMissing(token.loc, "starting " + start);
    }
  };
}


function parseSingle(fn: (loc: Loc) => $ast.AST): TokenInfo {
  return {
    // TODO is this the correct priority ?
    priority: Infinity,
    parse: (state: ParserState, token: $ast.Token, output: Array<$ast.AST>): Array<$ast.AST> => {
      output.push(fn(token.loc));
      return output;
    }
  };
}


function parseTransform(priority: Priority, assoc: Associativity, fn: (left: Array<$ast.AST>, token: $ast.Token, right: Array<$ast.AST>) => Array<$ast.AST>): TokenInfo {
  return {
    priority: priority,
    parse: (state: ParserState, token: $ast.Token, output: Array<$ast.AST>): Array<$ast.AST> => {
      const right = parse1(state, (assoc === "left" ? priority : priority - 1), []);

      return fn(output, token, right);
    }
  };
}


function parsePrefix(priority: Priority, make: (ast: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return parseTransform(priority, "right", (left: Array<$ast.AST>, middle: $ast.Token, right: Array<$ast.AST>): Array<$ast.AST> => {
    if (right.length === 0) {
      throw new NulanError(middle.loc, "There must be an expression on the right of " + middle.value);

    } else {
      const r = right.shift() as $ast.AST;

      return left.concat([make(r, concat(middle.loc, r.loc))], right);
    }
  });
}


function parseInfix(priority: Priority, assoc: Associativity, make: (left: $ast.AST, right: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return parseTransform(priority, assoc, (left: Array<$ast.AST>, middle: $ast.Token, right: Array<$ast.AST>): Array<$ast.AST> => {
    if (left.length === 0) {
      throw new NulanError(middle.loc, "There must be an expression on the left of " + middle.value);

    } else if (right.length === 0) {
      throw new NulanError(middle.loc, "There must be an expression on the right of " + middle.value);

    } else {
      const l = left.pop() as $ast.AST;
      const r = right.shift() as $ast.AST;

      return left.concat([make(l, r, concat(l.loc, r.loc))], right);
    }
  });
}


function parseLambda(priority: Priority, make: (args: Array<$ast.AST>, body: $ast.AST, loc: Loc) => $ast.AST): TokenInfo {
  return parseTransform(priority, "right", (left: Array<$ast.AST>, middle: $ast.Token, right: Array<$ast.AST>): Array<$ast.AST> => {
    if (right.length === 0) {
      throw new NulanError(middle.loc, "There must be an expression on the right of " + middle.value);

    } else {
      const args = right.slice(0, -1);
      const body = right[right.length - 1];

      left.push(make(args, body, concat(middle.loc, body.loc)));

      return left;
    }
  });
}


const specials: { [key: string]: TokenInfo } = Object.create(null);

specials["["] = parseStartBracket("]", $ast.array);
specials["{"] = parseStartBracket("}", $ast.record);

specials["("] = parseStartBracket(")", (args: Array<$ast.AST>, loc: Loc) => {
  if (args.length === 1) {
    // TODO don't use mutation ?
    args[0].loc = loc;
    return args[0];

  } else {
    return $ast.call(args, loc);
  }
});

specials[")"] = parseEndBracket("(");
specials["]"] = parseEndBracket("[");
specials["}"] = parseEndBracket("{");

specials["_"] = parseSingle($ast.wildcard);

specials[":"] = parseInfix(1, "right", $ast.match);

specials["|"] = parsePrefix(2, $ast.bar);
specials["&"] = parsePrefix(2, $ast.quote);
specials["<="] = parseInfix(2, "right", $ast.assign);
specials["::"] = parseInfix(2, "right", $ast.type);

specials["->"] = parseLambda(2, $ast.lambda);

specials["."] = parseInfix(2, "left", (left, right, loc) => {
  if (left.type === "integer" && right.type === "integer") {
    return $ast.float(left.value + "." + right.value, loc);

  } else {
    return $ast.dot(left, right, loc);
  }
});

specials["~"] = parsePrefix(3, $ast.unquote);
specials["@"] = parsePrefix(3, $ast.splice);
