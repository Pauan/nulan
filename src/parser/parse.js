import { error } from "../../util/error";
import { peek } from "../../util/array";
//import { pretty } from "./pretty";
import { SYMBOL, symbol, integer, string,
         call, list, record, lambda } from "./ast";


const is_symbol = (x, s) =>
  x.type === SYMBOL && x.value === s;

const lookup = (token) => {
  if (token.type === SYMBOL && specials[token.value] != null) {
    return specials[token.value];

  } else {
    return null;
  }
};


const end_at = (end, make) => {
  return {
    parse: (output, tokens, index) => {
      const first = tokens[index];

      index += 1;

      const values = [];

      for (;;) {
        const token = peek(tokens, index);

        if (token === null) {
          error(first, "missing ending " + end);

        } else if (is_symbol(token, end)) {
          index += 1;
          output["push"](make(values, first.filename, first.lines,
                                      first.start, token.end));
          return index;

        } else {
          index = parse1(values, tokens, index);
        }
      }
    }
  };
};

const start_at = (start, stop_lambda, unwrap) => {
  return {
    stop_lambda: stop_lambda,
    unwrap: unwrap,
    parse: (output, tokens, index) => {
      const token = tokens[index];
      error(token, "missing starting " + start);
    }
  };
};


const parse_lambda = {
  parse: (output, tokens, index) => {
    const first = tokens[index];

    index += 1;

    const values = [];

    for (;;) {
      const token = peek(tokens, index);

      if (token === null) {
        if (values["length"] === 0) {
        } else {
          const parameters = values["slice"](0, -1);
          const body = values[values["length"] - 1];
          output["push"](lambda(parameters, body,
                                first.filename, first.lines,
                                first.start, body.end));
          return index;
        }

      } else {
        const x = lookup(token);

        if (x !== null && x.stop_lambda) {
          if (output["length"] === 0 && x.unwrap) {
          } else {
          }

          const parameters = values["slice"](0, -1);
          const body = values[values["length"] - 1];
          output["push"](lambda(parameters, body,
                                first.filename, first.lines,
                                first.start, body.end));
          return index;

        } else {
          index = parse1(values, tokens, index);
        }
      }
    }
  }
};


const specials = {
  "(": end_at(")", call),
  ")": start_at("(", true, true),

  "[": end_at("]", list),
  "]": start_at("[", true, false),

  "{": end_at("}", record),
  "}": start_at("{", true, false),

  "->": parse_lambda,

  //"&": parse_prefix
};


const parse1 = (output, tokens, index) => {
  const token = tokens[index];

  const x = lookup(token);

  if (x !== null) {
    return x.parse(output, tokens, index);

  } else {
    output["push"](token);
    return index + 1;
  }
};

export const parse = (tokens) => {
  const output = [];

  let index = 0;

  for (;;) {
    const token = peek(tokens, index);

    if (token === null) {
      return output;

    } else {
      index = parse1(output, tokens, index);
    }
  }
};
