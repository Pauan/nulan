import { Record, isRecord, isTuple, UUIDTag, join, map } from "../node_modules/Immutable/src/Immutable";
import { assert } from "./assert";
import { tag_pretty, pretty, pretty_newline, pretty_string } from "./pretty";

export var tag_simple = UUIDTag("da5e7255-3e01-492a-9b89-4aa6c7a3c0d6");

//export var empty = UUIDTag("8aef2108-d1db-47b7-8581-48827782bd53");

function isSimpleType(x) {
  //var type = (typeof x);
  /*type === "number" ||
         type === "string" ||
         type === "function" ||*/
  return isRecord(x) && x.get(tag_simple, false);
}

function Call_pretty(x) {
  var x2 = map(x.get("value"), function (x) {
    if (isSimpleType(x)) {
      return pretty_newline(pretty(x), 1);
    } else {
      return "\n  " + pretty_newline(pretty(x), 2);
    }
  });
  return "(" + join(x2, " ") + ")";
}

export function Call(x, loc) {
  assert(isTuple(x));

  return Record([
    ["value", x],
    ["loc", loc],
    [tag_type, "Call"],
    [tag_pretty, Call_pretty]
  ]);
}

function Symbol_pretty(x) {
  return "#(Symbol " + pretty_newline(x.get("value").replace(/[\\\(\)]/g, "\\$&"), 9) + ")";
}

export function Symbol(x, loc) {
  assert(typeof x === "string");
  assert(x !== "");

  return Record([
    ["value", x],
    ["loc", loc],
    [tag_type, "Symbol"],
    [tag_pretty, Symbol_pretty],
    [tag_simple, true]
  ]);
}

function Number_pretty(x) {
  return "" + x.get("value");
}

export function Number(x, loc) {
  assert(typeof x === "number");

  return Record([
    ["value", x],
    ["loc", loc],
    [tag_type, "Number"],
    [tag_pretty, Number_pretty],
    [tag_simple, true]
  ]);
}

function String_pretty(x) {
  return pretty_string(x.get("value"));
}

export function String(x, loc) {
  assert(typeof x === "string");

  return Record([
    ["value", x],
    ["loc", loc],
    [tag_type, "String"],
    [tag_pretty, String_pretty],
    [tag_simple, true]
  ]);
}

export function loc(start, end) {
  return start.get("loc").set("end", end.get("loc").get("end"));
}

export function wrap_loc(loc) {
  return Record([["loc", loc]]);
}

export function isType(x, type) {
  return isRecord(x) && x.has(tag_type) && x.get(tag_type) === type;
}

export function isSymbol(x, name) {
  return isType(x, "Symbol") && x.get("value") === name;
}

/*export function toNumber(x) {
  if (typeof x === "number") {
    return Number(x, null);
  } else if (isType(x, "number")) {
    return x;
  } else {
    throw error(x, "Cannot convert to number: " + pretty(x));
  }
}

export function toString(x) {
  if (typeof x === "string") {
    return String(x, null);
  } else if (isType(x, "string")) {
    return x;
  } else {
    throw error(x, "Cannot convert to string: " + pretty(x));
  }
}*/
