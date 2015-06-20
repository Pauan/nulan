import { isRecord, isTuple, UUIDTag, isList, isDict,
         isSet, isType, join, repeat, map } from "../node_modules/Immutable/src/Immutable";

export var tag_pretty = UUIDTag("321522eb-bfa5-4a3a-93dc-66f6ec6c774a");

export function pretty_newline(x, spaces) {
  return x.replace(/\n/g, "\n" + join(repeat(" ", spaces)));
}

// TODO handle newlines
// TODO expose the toString algorithm from the Immutable library
export function pretty_record(x) {
  if (x.isEmpty()) {
    return "{}";

  } else {
    return "{ " + join(map(x, function (a) {
      var key   = a.get(0);
      var value = a.get(1);

      var key2 = pretty(key);
      return pretty_newline(key2 + " = " + pretty_newline(pretty(value), key2.length + 3), 2);
    }), "\n  ") + " }";
  }
}

export function pretty_tuple(x) {
  return "[" + pretty_newline(join(map(x, pretty), "\n"), 1) + "]";
}

export function pretty_string(x) {
  return pretty_newline("\"" + x.replace(/[\\\"]/g, "\\$&") + "\"", 1);
}

export function pretty(x) {
  if (isType(x)) {
    var type = x.type();
    if (type.has(tag_pretty)) {
      return type.get(tag_pretty)(x);
    } else {
      return "#(Type " + pretty(x.type()) + " " + join(map(x, pretty)) + ")";
    }

  } else if (isTuple(x)) {
    return pretty_tuple(x);

  } else if (isRecord(x)) {
    return pretty_record(x);

  } else if (isDict(x)) {
    return "#(Dict " + pretty_newline(pretty_record(x), 7) + ")";

  } else if (isList(x)) {
    return "#(List " + pretty_newline(pretty_tuple(x), 7) + ")";

  } else if (isSet(x)) {
    return "#(Set " + pretty_newline(pretty_tuple(x), 6) + ")";

  } else if (typeof x === "function") {
    return "#(Function)";

  } else if (typeof x === "string") {
    return pretty_string(x);

  } else {
    return "" + x;
  }
}
