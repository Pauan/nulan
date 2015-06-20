import { foldl, skip, Tuple, List } from "../node_modules/Immutable/src/Immutable";
import { error } from "./error";
import { pretty } from "./pretty";
import { isType, Call } from "./types";
import { lookup_var } from "./scope";

export function macex_rest(info, rest) {
  var info2 = info.modify("value", function (value) {
    return List([value]);
  });

  return foldl(rest, info2, function (info, x) {
    var list = info.get("value");

    var info2 = macex(info.set("value", x));

    return info2.modify("value", function (value) {
      return list.push(value);
    });
  });
}

function macex_Call(info, tuple, loc) {
  var first = tuple.get(0);
  var rest  = skip(tuple, 1);
  var info2 = macex(info.set("value", first));

  var box = info2.get("value");

  if (isType(box, "Box")) {
    var macro = box.get("macro");
    if (isType(macro, "Something")) {
      return macro.get("value")(info2, Tuple(rest), loc);

    } else {
      return macex_rest(info2, rest).modify("value", function (value) {
        return Call(Tuple(value), loc);
      });
    }

  } else {
    // TODO code duplication
    return macex_rest(info2, rest).modify("value", function (value) {
      return Call(Tuple(value), loc);
    });
  }
}

export function macex(info) {
  var value = info.get("value");

  if (isType(value, "Call")) {
    var tuple = value.get("value");
    var loc = value.get("loc");

    if (tuple.isEmpty()) {
      throw error(value, "Invalid call: " + pretty(value));

    } else {
      return macex_Call(info, tuple, loc);
    }

  } else if (isType(value, "Symbol")) {
    return lookup_var(info);

  } else if (isType(value, "Number") || isType(value, "String")) {
    return info;

  } else {
    throw error(value, "Cannot macex type: " + pretty(value));
  }
}
