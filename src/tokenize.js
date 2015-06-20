import { Queue, List, Record, join } from "../node_modules/Immutable/src/Immutable";
import { Symbol, Number } from "./types";
import { assert } from "./assert";

function add1(x) {
  return x + 1;
}

function ignore(line) {
  return function (info) {
    if (line) {
      info = info.modify("line", add1);
      info = info.set("column", 1);
    } else {
      info = info.modify("column", add1);
    }

    return info;
  }
}

function loc(start, end) {
  return Record({
    "filename": start.get("filename"),
    "source": start.get("source"),
    "start": Record({
      "line": start.get("line"),
      "column": start.get("column")
    }),
    "end": Record({
      "line": end.get("line"),
      "column": end.get("column")
    })
  });
}

function push1(info, char) {
  var start = info;

  info = info.modify("column", add1);

  info = info.modify("output", function (output) {
    return output.push(Symbol(char, loc(start, info)));
  });

  return info;
}

function tokenize_word(info, char) {
  var start = info;
  var input = info.get("input");
  var temp  = List([char]);

  info = info.modify("column", add1);

  for (;;) {
    if (input.isEmpty()) {
      break;

    } else {
      var char = input.peek();

      if (special.has(char)) {
        break;

      } else {
        temp = temp.push(char);
        input = input.pop();

        info = info.modify("column", add1);
      }
    }
  }

  assert(!temp.isEmpty());

  return info.set("input", input).modify("output", function (output) {
    var string = join(temp);
    if (/^[0-9]+(?:\.[0-9]+)?$/.test(string)) {
      return output.push(Number(+string, loc(start, info)));
    } else {
      return output.push(Symbol(string, loc(start, info)));
    }
  });
}

function tokenize_string(info, char) {
  var start = info;
  var input = info.get("input");
  var temp  = List([char]);
}

var special = Record({
  " ":  ignore(false),
  "\n": ignore(true),
  "(": push1,
  ")": push1,
  "{": push1,
  "}": push1,
  "[": push1,
  "]": push1,
  "`": push1,
  ",": push1,
  "@": push1,
  "\"": tokenize_string
});

function tokenize1(info) {
  for (;;) {
    var input = info.get("input");
    if (input.isEmpty()) {
      break;

    } else {
      var char = input.peek();

      info = info.modify("input", function (input) {
        return input.pop();
      });

      if (special.has(char)) {
        info = special.get(char)(info, char);

      } else {
        info = tokenize_word(info, char);
      }
    }
  }

  return info;
}

export function tokenize(input, filename) {
  var info = Record({
    "input": Queue(input),
    "output": Queue(),
    "filename": filename,
    "source": input,
    "column": 1,
    "line": 1
  });

  info = tokenize1(info);

  return info.get("output");
}
