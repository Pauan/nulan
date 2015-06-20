import { Record, Set, Tuple, foldl } from "../node_modules/Immutable/src/Immutable";
import { Box, lookup_var } from "./scope";
import { Something, tag_type, tag_simple, Symbol, Call, wrap_loc, isType } from "./types";
import { error } from "./error";
import { macex, macex_rest } from "./macex";
import { tag_pretty, pretty, pretty_newline } from "./pretty";
import { plural } from "./core/compile";

function If_pretty(x) {
  return "#(If " + pretty_newline(pretty(x.get("test")), 5) +
         "\n   " + pretty_newline(pretty(x.get("yes")), 3) +
         "\n   " + pretty_newline(pretty(x.get("no")), 3) + ")";
}

function If(test, yes, no, loc) {
  return Record([
    ["test", test],
    ["yes", yes],
    ["no", no],
    ["loc", loc],
    [tag_type, "If"],
    [tag_pretty, If_pretty]
  ]);
}

function Assign_pretty(x) {
  return "#(Assign " + pretty(x.get("box")) + " " + pretty(x.get("value")) + ")";
}

function Assign(left, right, loc) {
  return Record([
    ["left", left],
    ["right", right],
    ["loc", loc],
    [tag_type, "Assign"],
    [tag_pretty, Assign_pretty]
  ]);
}

function Var_pretty(x) {
  return "#(Var " + pretty(x.get("box")) + " " + pretty(x.get("value")) + ")";
}

function Var(values, loc) {
  return Record([
    ["values", values],
    ["loc", loc],
    [tag_type, "Var"],
    [tag_pretty, If_pretty]
  ]);
}

/*function Binary_pretty(x) {
  return "#(Binary " + x.get("op") + " " + pretty(x.get("left")) + " " + pretty(x.get("right")) + ")";
}

export function Binary(op, left, right, loc) {
  return Record([
    ["op", op],
    ["left", left],
    ["right", right],
    ["loc", loc],
    [tag_type, "Binary"],
    [tag_pretty, Binary_pretty]
  ]);
}

function binary(op) {
  return function (info, args, loc) {
    if (args.size() === 2) {
      var arg1 = macex(info.set("value", args.get(0)));
      var arg2 = macex(arg1.set("value", args.get(1)));
      return arg2.set("value", Binary(op, arg1.get("value"), arg2.get("value"), loc));

    } else {
      throw error(loc, "Expected 2 arguments but got " + args.size());
    }
  }
}

function binary_foldl(op) {

}*/

function macro(name, f) {
  return [name, Box({
    "name": Something(name),
    "macro": Something(f),
    "phases": Set(["runtime", "compile"])
  }, null)];
}

/*function Core_pretty(x) {
  return "#(Core " + x.get("name") + ")";
}

export function Core(name, loc) {
  return Record([
    ["name", name],
    ["loc", loc],
    [tag_type, "Core"],
    [tag_pretty, Core_pretty],
    [tag_simple, true]
  ]);
}*/

function core(inner, outer) {
  return [inner, Box([
    ["name", Something(inner)],
    ["module", Tuple(["nulan", "core"])],
    ["phases", Set(["runtime", "compile"])]
  ], null)];

  /*return function (info, args, loc) {
    var info2 = info.modify("value", function (value) {
      return Core(name, value.get("loc"));
    });

    return macex_rest(info2, args).modify("value", function (value) {
      return Call(Tuple(value), loc);
    });
  };*/
}


function getBuiltin(info, name) {
  return info.get("scope").get("builtin").get(name);
}

function isBoxEqual(x, y) {
  return x.get("id") === y.get("id");
}


export var builtins = Record([
  macro("=", function (info, args, loc) {
    throw error(wrap_loc(loc), "Cannot use = here");

    return info.set("value", Assign(args.get(0), args.get(1), loc));
  }),

  macro("if", function (info, args, loc) {
    if (args.size() !== 3) {
      throw error(wrap_loc(loc), "Expected 3 arguments but got " + args.size());
    }

    var arg1 = macex(info.set("value", args.get(0)));
    var arg2 = macex(arg1.set("value", args.get(1)));
    var arg3 = macex(arg2.set("value", args.get(2)));

    return arg3.set("value", If(arg1.get("value"), arg2.get("value"), arg3.get("value"), loc));
  }),

  (match value
    `(= name value2)
      (Assign name value2 value:loc)
    (symbol? value2)
      value2)

  macro("var", function (info, args, loc) {
    foldl(args, info, function (info, value) {
      if (isType(value, "Call")) {
        destructure_call(value, 2, function (name, value) {
          console.log(name, value);
        });

        var args = value.get("value");

        var first = lookup_var(info.set("value", args.get(0)));

        var builtin = getBuiltin(first, "=");

        if (isBoxEqual(first.get("value"), builtin)) {

          console.log("HURRAY");
        } else {
          throw error(first.get("value"), "Expected " + pretty(builtin) + " but got " + pretty(first.get("value")));
        }

        if (args.size() !== 3) {

        }

        return Assign(args.get(1), args.get(2), value.get("loc"));

      } else if (isType(value, "Symbol")) {
        return value;

      } else {
        throw error(value, "Invalid match: " + pretty(value));
      }
    });

    /*var arg1 = macex(info.set("value", args.get(0)));
    var arg2 = macex(arg1.set("value", args.get(1)));
    var arg3 = macex(arg2.set("value", args.get(2)));

    return arg3.set("value", If(arg1.get("value"), arg2.get("value"), arg3.get("value"), loc));*/
  }),

  core("js/get", "get"),

  core("Something", "Something"),
  core("Something", "Nothing"),

  core("+", "add"),
  core("-", "subtract"),
  core("/", "divide"),
  core("*", "multiply"),
  core("not", "not"),
  core("null", "_void"),
  core("modulo", "modulo"),
  core("do", "_do"),
  core("self", "self"),

  core("string?", "isString"),
  core("number?", "isNumber"),
  core("boolean?", "isBoolean"),
  core("null?", "isVoid"),
  core("lt?", "lt"),
  core("gt?", "gt"),
  core("lt-is?", "lt_is"),
  core("gt-is?", "gt_is"),
  core("is?", "is"),
  core("isnt?", "isnt"),
  //macro("js/new", ),
]);
