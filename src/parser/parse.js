import { error } from "./error";
import { peek } from "../../util/array";
import * as $ast from "./ast";


const is_symbol = (x, s) =>
  x.type === $ast.SYMBOL && x.value === s;

const lookup = (token) => {
  if (token.type === $ast.SYMBOL && specials[token.value] != null) {
    return specials[token.value];

  } else {
    return null;
  }
};


// Modified Pratt parser
const parse1 = (state, tokens, priority) => {
  for (;;) {
    const token = peek(tokens, state.index);

    if (token === null) {
      return state;

    } else {
      const x = lookup(token);

      if (x === null) {
        ++state.index;
        state.output["push"](token);

      } else if (priority === null || x.priority > priority) {
        ++state.index;
        state = x.parse(state, tokens, token);

      } else {
        return state;
      }
    }
  }
};

export const parse = (tokens) => {
  const state = {
    index: 0,
    output: []
  };

  const x = parse1(state, tokens, null);

  return x.output;
};


const parsed = (info) => {
  const parse = info.parse;

  info.parse = (state, tokens, token) => {
    const state2 = {
      index: state.index,
      output: []
    };

    const right = (info.right_associative
                    ? parse1(state2, tokens, info.priority - 1)
                    : parse1(state2, tokens, info.priority));

    state.index = right.index;
    state.output = parse(state.output, token, right.output);
    return state;
  };

  return info;
};

const end_at = (end, make) => {
  return {
    // TODO is this the correct priority ?
    priority: Infinity,
    parse: (state, tokens, first) => {
      let right = {
        index: state.index,
        output: []
      };

      for (;;) {
        const token = peek(tokens, right.index);

        if (token === null) {
          error(first, "missing ending " + end);

        } else if (is_symbol(token, end)) {
          const values = right.output;

          state.index = right.index + 1;

          state.output["push"](make(values, $ast.concat_loc(first.loc, token.loc)));

          return state;

        } else {
          // TODO is this the correct priority ?
          right = parse1(right, tokens, -1);
        }
      }
    }
  };
};

const start_at = (start) => {
  return {
    // TODO is this the correct priority ?
    priority: -1,
    parse: (state, tokens, token) => {
      error(token, "missing starting " + start);
    }
  };
};


const parse_prefix = (priority, make) =>
  parsed({
    priority: priority,
    right_associative: true,
    parse: (left, middle, right) => {
      if (right["length"] === 0) {
        error(middle, "missing expression on the right side");

      } else {
        const r = right["shift"]();
        const x = make(r, $ast.concat_loc(middle.loc, r.loc));
        return left["concat"]([x], right);
      }
    }
  });

const parse_infix = (priority, make, right_associative) =>
  parsed({
    priority: priority,
    right_associative: right_associative,
    parse: (left, middle, right) => {
      if (left["length"] === 0) {
        error(middle, "missing expression on the left side");

      } else if (right["length"] === 0) {
        error(middle, "missing expression on the right side");

      } else {
        const l = left["pop"]();
        const r = right["shift"]();
        const x = make(l, r, $ast.concat_loc(l.loc, r.loc));
        return left["concat"]([x], right);
      }
    }
  });

const parse_lambda = (priority, make) =>
  parsed({
    priority: priority,
    right_associative: true,
    parse: (left, middle, right) => {
      if (right["length"] < 2) {
        error(middle, "functions must have at least 1 parameter");

      } else {
        const parameters = right["slice"](0, -1);
        const body = right[right["length"] - 1];

        const x = make(parameters, body, $ast.concat_loc(middle.loc, body.loc));

        left["push"](x);
        return left;
      }
    }
  });


const specials = {
  "(": end_at(")", $ast.call),
  ")": start_at("("),

  "[": end_at("]", $ast.list),
  "]": start_at("["),

  "{": end_at("}", $ast.record),
  "}": start_at("{"),

  "->": parse_lambda(10, $ast.lambda),

  "|": parse_prefix(10, $ast.bar),
  "&": parse_prefix(10, $ast.quote),
  ",": parse_prefix(20, $ast.unquote),
  "@": parse_prefix(20, $ast.splice),

  ".":  parse_infix(10, $ast.dot, false),
  "<=": parse_infix(10, $ast.assign, true),
  "::": parse_infix(10, $ast.type, false),
};
