import { isSet, Record, Dict, Set, UUIDTag, Tag, join, Tuple } from "../node_modules/Immutable/src/Immutable";
import { Something, Nothing, tag_type, tag_simple, isType } from "./types";
import { error } from "./error";
import { tag_pretty, pretty } from "./pretty";
import { assert } from "./assert";

/*var tag_value = UUIDTag("2a83ad38-5f7f-4eb3-8b9e-68c41d5374d5");
var tag_macro = UUIDTag("fb5d3741-15fc-4653-86d7-a7aa68eeeff3");
var tag_phases = UUIDTag("8edd68e2-54a5-4bd2-9eb7-ed6785f5d869");*/

var Box_id = 0;

function Box_pretty(x) {
  var name = x.get("name");
  if (isType(name, "Something")) {
    return "#(Box " + x.get("id") + " " + name.get("value") + ")";
  } else {
    return "#(Box " + x.get("id") + ")";
  }
}

export function Box(info, loc) {
  return Record([
    ["id", ++Box_id],
    ["name", Nothing],
    ["loc", loc],
    ["phases", Set()], // TODO
    ["value", Nothing],
    ["macro", Nothing],
    ["module", Tuple()], // TODO
    [tag_type, "Box"],
    [tag_pretty, Box_pretty],
    [tag_simple, true]
  ]).update(info);
}

var Box_defaults = Record([
  ["id", null],
  ["loc", isa(Nothing)],
  ["name", isa(Nothing)],
  ["value", isa(Nothing)],
  ["macro", isa(Nothing)],
  ["phases", Set()]
]);

export function Box(loc, info) {
  return isa(Code, "Box", Box_defaults.set("id", ++Box_id).update(info));
}

export function Scope(builtin) {
  return Record([
    ["builtin", builtin],
    ["builtin-used", Set()],
    ["defined", Dict()],
    ["deferred", Dict()]
  ]);
}

function check_phase(box, phase) {
  var phases = box.get("phases");
  if (phases.has(phase)) {
    return box;
  } else {
    throw error(box, "Variable was defined in phases [" + join(phases, ", ") + "] but accessed in phase \"" + phase + "\"");
  }
}

// TODO generic function for this
function transfer_loc(box, symbol) {
  return box.set("loc", symbol.get("loc"));
}

function return_box(info, box, symbol, phase) {
  return info.set("value", check_phase(transfer_loc(box, symbol), phase));
}

export function lookup_var(info) {
  var scope  = info.get("scope");
  var symbol = info.get("value");
  var phase  = info.get("phase");

  if (!isType(symbol, "Symbol")) {
    throw error(symbol, "Expected a symbol but got " + pretty(symbol));
  }

  var name = symbol.get("value");
  var defined = scope.get("defined");
  var deferred = scope.get("deferred");
  var builtin = scope.get("builtin");

  // Local vars have priority over builtin vars
  if (defined.has(name)) {
    return return_box(info, defined.get(name), symbol, phase);

  } else if (deferred.has(name)) {
    return return_box(info, deferred.get(name), symbol, phase);

  } else if (builtin.has(name)) {
    scope = scope.modify("builtin-used", function (used) {
      return used.add(name);
    });

    info = info.set("scope", scope);

    return return_box(info, builtin.get(name), symbol, phase);

  } else {
    throw error(symbol, "Variable " + name + " is not defined");
  }
}

export function lookup_deferred(info) {
  var scope = info.get("scope");
  var value = info.get("value");

  assert(isType(value, "Symbol"));

  var name = value.get("value");
  var deferred = scope.get("deferred");

  if (deferred.has(name)) {
    return Something(deferred.get(name));
  } else {
    return Nothing;
  }
}

function define_box(info, name, set) {
  var value = info.get("value");
  var scope = info.get("scope");

  assert(typeof name === "string");
  assert(isType(value, "Box"));

  var used = scope.get("builtin-used");
  var deferred = scope.get("deferred");
  var defined = scope.get("defined");

  if (defined.has(name) || deferred.has(name)) {
    throw error(value, "Variable " + name + " cannot be defined twice");

  // It's okay for a local var to shadow a built-in var, if the built-in var is not used
  } else if (used.has(name)) {
    throw error(value, "Variable " + name + " cannot have two different meanings");

  } else {
    scope = scope.modify(set, function (dict) {
      return dict.set(name, value);
    });

    return info.set("scope", scope);
  }
}

export function define_var(info, name) {
  return define_box(info, name, "defined");
}

export function define_deferred(info, name) {
  return define_box(info, name, "deferred");
}

export function deferred_to_defined(info, name) {
  return info.modify("scope", function (scope) {
    var deferred = scope.get("deferred");
    var box = deferred.get(name);

    scope = scope.set("deferred", deferred.remove(name));

    scope = scope.modify("defined", function (defined) {
      assert(!defined.has(name));
      return defined.set(name, box);
    });

    return scope;
  });
}
