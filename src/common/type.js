import { Dict, equal, isType, Type, UUIDTag } from "../../node_modules/Immutable/src/Immutable";
import { check_arguments_min } from "./check";

export var tag_type = UUIDTag("84d743fb-56e5-4574-89b8-10037b969907");

/*export var tag_values = UUIDTag("d32e07d9-d6a4-438b-aea0-c8694fd72768");

function Type_pretty(x) {
  return "#(Type " + pretty(x.get(tag_type)) + join(map(x.get(tag_values), pretty), " ") + ")";
}*/

var type_id = 0;

export function Type(x) {
  return Dict([
    [tag_type, ++type_id]
  ]).merge(x);
}

export function Subtype(type, x) {
  return type.merge(x);
}

export function isa() {
  check_arguments_min(arguments.length, 1);

  var a = [];
  for (var i = 1; i < arguments.length; ++i) {
    a.push(arguments[i]);
  }

  return Type(arguments[0], a);
}

export function isIsa(x, type) {
  return isType(x) && equal(x.type(), type);
}
