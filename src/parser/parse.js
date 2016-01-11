import { error } from "../../util/error";
import { peek } from "../../util/array";
//import { pretty } from "./pretty";
import { SYMBOL, symbol, integer, string,
         call, list, record, lambda, bar,
         assign, dot } from "./ast";


const is_symbol = (x, s) =>
  x.type === SYMBOL && x.value === s;

const lookup = (token) => {
  if (token.type === SYMBOL && specials[token.value] != null) {
    return specials[token.value];

  } else {
    return null;
  }
};


// Heavily modified Pratt parser
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

const end_at = (end, make, unwrap) => {
  return {
    // TODO is this the correct priority ?
    priority: 0,
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

          if (unwrap && values["length"] === 1) {
            state.output["push"](values[0]);

          } else {
            state.output["push"](make(values, first.filename, first.lines, first.start, token.end));
          }

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
        const x = make(r, middle.filename, middle.lines, middle.start, r.end);
        return left["concat"]([x], right);
      }
    }
  });

const parse_infix = (priority, make) =>
  parsed({
    priority: priority,
    parse: (left, middle, right) => {
      if (left["length"] === 0) {
        error(middle, "missing expression on the left side");

      } else if (right["length"] === 0) {
        error(middle, "missing expression on the right side");

      } else {
        const l = left["pop"]();
        const r = right["shift"]();
        const x = make(l, r, middle.filename, middle.lines, l.start, r.end);
        return left["concat"]([x], right);
      }
    }
  });

const parse_lambda = parsed({
  priority: 20,
  right_associative: true,
  parse: (left, middle, right) => {
    if (right["length"] < 2) {
      error(middle, "functions must have at least 1 parameter");

    } else {
      const parameters = right["slice"](0, -1);
      const body = right[right["length"] - 1];

      const x = lambda(parameters, body,
                       middle.filename, middle.lines,
                       middle.start, body.end);

      left["push"](x);
      return left;
    }
  }
});


const specials = {
  "(": end_at(")", call, true),
  ")": start_at("("),

  "[": end_at("]", list, false),
  "]": start_at("["),

  "{": end_at("}", record, false),
  "}": start_at("{"),

  "->": parse_lambda,

  "|": parse_prefix(10, bar),

  "<=": parse_infix(10, assign),
  ".": parse_infix(10, dot),

  //"&": parse_prefix
};
