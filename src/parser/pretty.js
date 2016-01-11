import * as $ast from "./ast";
import { repeat, indent, lines } from "../../util/string";
import { map, join, length } from "../../util/array";


const is_simple = (x) =>
  x.type === $ast.SYMBOL ||
  x.type === $ast.INTEGER ||
  x.type === $ast.NUMBER ||
  x.type === $ast.STRING ||
  (x.type === $ast.DOT &&
    is_simple(x.left) &&
    is_simple(x.right)) ||
  (x.type === $ast.QUOTE &&
    is_simple(x.value)) ||
  (x.type === $ast.UNQUOTE &&
    is_simple(x.value)) ||
  (x.type === $ast.SPLICE &&
    is_simple(x.value));


const last = (s) => {
  const x = lines(s);
  return length(x[x["length"] - 1]);
};

const pretty_lambda = (left, parameters, body, right) => {
  let output = left;

  for (let i = 0; i < parameters["length"]; ++i) {
    output += " ";
    output += indent(pretty(parameters[i]), repeat(" ", last(output)));
  }

  return output + "\n  " + indent(pretty(body), "  ") + right;
};

const pretty_brackets = (left, value, right, space, indented) => {
  if (value["length"] === 0) {
    return left + right;

  } else {
    let simple = is_simple(value[0]);

    let output = left + space;

    output += indent(pretty(value[0]), repeat(" ", last(output)));

    for (let i = 1; i < value["length"]; ++i) {
      const x = value[i];

      if (simple && is_simple(x)) {
        output += " ";
        output += indent(pretty(x), repeat(" ", last(output)));

      } else {
        simple = false;
        output = output + "\n" + indented + indent(pretty(x), indented);
      }
    }

    return output + space + right;
  }
};

// TODO handle other escapes, such as Unicode
// TODO handle spaces at the end of the line
const pretty_string = (s) =>
  indent("\"" + s["replace"](/[\\\"]/g, "\\$&") + "\"", " ");

const pretty_prefix = (name, value) =>
  name + indent(pretty(value), repeat(" ", length(name)));

const pretty_infix = (left, name, right) => {
  const s = pretty(left) + name;
  return s + indent(pretty(right), repeat(" ", last(s)));
};


export const pretty = (x) => {
  if (x.type === $ast.SYMBOL) {
    return x.value;

  } else if (x.type === $ast.INTEGER || x.type === $ast.NUMBER) {
    return "" + x.value;

  } else if (x.type === $ast.STRING) {
    return pretty_string(x.value);

  } else if (x.type === $ast.CALL) {
    return pretty_brackets("(", x.value, ")", "", "  ");

  } else if (x.type === $ast.LIST) {
    return pretty_brackets("[", x.value, "]", " ", "");

  } else if (x.type === $ast.RECORD) {
    return pretty_brackets("{", x.value, "}", " ", "");

  } else if (x.type === $ast.LAMBDA) {
    return pretty_lambda("(->", x.parameters, x.body, ")");

  } else if (x.type === $ast.ASSIGN) {
    return pretty_infix(x.left, " <= ", x.right);

  } else if (x.type === $ast.DOT) {
    return pretty_infix(x.left, ".", x.right);

  } else if (x.type === $ast.TYPE) {
    return pretty_infix(x.left, " :: ", x.right);

  } else if (x.type === $ast.BAR) {
    return pretty_prefix("| ", x.value);

  } else if (x.type === $ast.QUOTE) {
    return pretty_prefix("&", x.value);

  } else if (x.type === $ast.UNQUOTE) {
    return pretty_prefix("~", x.value);

  } else if (x.type === $ast.SPLICE) {
    return pretty_prefix("@", x.value);
  }
};
