import { List, Tuple, Record } from "../node_modules/Immutable/src/Immutable";
import { Call, isType, isSymbol, loc } from "./types";
import { error } from "./error";
import { assert } from "./assert";
import { tokenize } from "./tokenize";

function start_at(start) {
  return Record({
    "start-at": start
  });
}

function end_at(str_end, f) {
  return Record({
    "end-at": str_end,
    "priority": Infinity, // TODO hacky
    "parse": function (left, start, right) {
      var end = right.get("input").peek();
      right = pop(right);

      left = left.set("input", right.get("input"));
      left = push(left, f(start, right.get("output"), loc(start, end)));

      return left;
    }
  });
}

function unary(priority) {
  return Record({
    "associativity": "right",
    "priority": priority,
    "parse": function (left, start, right) {
      var output = right.get("output");

      if (output.isEmpty()) {
        throw error(start, "Expected an expression to the right of " + start.get("value") + " but got nothing");

      } else {
        var value = output.get(0);

        left = left.set("input", right.get("input"));

        return left.modify("output", function (left) {
          return left.push(Call(Tuple([start, value]), loc(start, value))).concat(output.slice(1));
        });
      }
    }
  });
}

function binary(priority) {
  return Record({
    "priority": priority,
    "parse": function (left, middle, right) {
      var left_output  = left.get("output");
      var right_output = right.get("output");

      if (left_output.isEmpty()) {
        throw error(middle, "Expected an expression to the left of " + middle.get("value") + " but got nothing");

      } else if (right_output.isEmpty()) {
        throw error(middle, "Expected an expression to the right of " + middle.get("value") + " but got nothing");

      } else {
        left = left.set("input", right.get("input"));

        var start = left_output.get(-1);
        var end   = right_output.get(0);

        var output = left_output.slice(0, -1)
          .push(Call(Tuple([middle, start, end]), loc(start, end)))
          .concat(right_output.slice(1));

        left = left.set("output", output);

        return left;
      }
    }
  });
}

var parsers = Record({
  "(": end_at(")", function (start, output, loc) {
    return Call(Tuple(output), loc);
  }),
  ")": start_at("("),

  "{": end_at("}", function (start, output, loc) {
    return Call(Tuple(output.insert(0, start)), loc);
  }),
  "}": start_at("{"),

  "[": end_at("]", function (start, output, loc) {
    return Call(Tuple(output.insert(0, start)), loc);
  }),
  "]": start_at("["),

  "`": unary(80), // TODO 10
  ",": unary(80),
  "@": unary(80),

  "=": binary(0),

  "->": Record({
    "associativity": "right",
    "priority": 10,
    "parse": function (left, middle, right) {
      var output = right.get("output");

      left = left.set("input", right.get("input"));

      var args, body;
      if (output.isEmpty()) {
        args = output;
        // TODO is the loc correct ?
        body = Call(Tuple(), middle.get("loc"));
      } else {
        args = output.slice(0, -1);
        body = output.get(-1);
      }

      if (args.isEmpty()) {
        // TODO is the loc correct ?
        args = Call(Tuple(), middle.get("loc"));
      } else {
        args = Call(Tuple(args), loc(args.get(0), args.get(-1)));
      }

      return push(left, Call(Tuple([middle, args, body]), loc(middle, body)));
    }
  })
});



function getParser(x) {
  if (isType(x, "Symbol")) {
    var name = x.get("value");
    return parsers.get(name, null);
  } else {
    return null;
  }
}

function pop(info) {
  return info.modify("input", function (input) {
    return input.pop();
  });
}

function push(info, x) {
  return info.modify("output", function (output) {
    return output.push(x);
  });
}

// Heavily modified Pratt parser, designed for lists of symbols rather than expressions of tokens
function parse1(info, pri1, end) {
  for (;;) {
    var input = info.get("input");
    if (input.isEmpty()) {
      break;

    } else {
      var parser, x = input.peek();

      // TODO this is a bit hacky, is there a better way ?
      if (end !== null && isSymbol(x, end)) {
        break;

      } else if ((parser = getParser(x)) !== null) {
        if (parser.has("start-at")) {
          var start_at = parser.get("start-at");

          if (end === null) {
            throw error(x, "Missing starting " + start_at);
          } else {
            throw error(x, "Expected " + end + " but got " + x.get("value"));
          }

        } else {
          var pri2 = parser.get("priority", 0);

          if (pri1 === null || pri2 > pri1) {
            info = pop(info);

            // TODO this is a bit hacky, is there a better way ?
            if (parser.has("end-at")) {
              var end_at = parser.get("end-at");

              var info2 = parse1(info.set("output", List()), null, end_at);

              for (;;) {
                var input = info2.get("input");
                if (input.isEmpty()) {
                  throw error(x, "Missing ending " + end_at);

                } else if (isSymbol(input.peek(), end_at)) {
                  break;

                } else {
                  info2 = parse1(info2, null, end_at);
                }
              }

            } else {
              if (parser.get("associativity", "left") === "right") {
                --pri2;
              }

              var info2 = parse1(info.set("output", List()), pri2, end);
            }

            info = parser.get("parse")(info, x, info2);

          } else {
            break;
          }
        }

      } else {
        info = pop(info);
        info = push(info, x);
      }
    }
  }

  return info;
}

export function parse(x, filename) {
  var info = Record({
    "input": tokenize(x, filename),
    "output": List()
  });

  info = parse1(info, null, null);

  return Tuple(info.get("output"));
}