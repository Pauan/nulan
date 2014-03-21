"use strict";

var a = require("../lib/util/symbol")
  , b = require("../lib/util/object")
  , c = require("../lib/util/uuid")
  , d = require("../lib/scope")
  , e = require("./1 tokenize")
  , f = require("./2 parse")

var Symbol   = a.Symbol
  , isObject = b.isObject
  , v4       = c.v4
  , Scope    = d.Scope
  , Error    = e.Error
  , proxy    = f.proxy


var vars    = exports.vars    = new Scope()
var isGet   = exports.isGet   = Symbol("isGet")
var isSet   = exports.isSet   = Symbol("isSet")
var isMacro = exports.isMacro = Symbol("isMacro")

var boxId = 0

var boxes = {}

function Box(x, loc) {
  var i     = "" + (++boxId)
  this.type = "x-nulan-box" // TODO kind of gross...
  // TODO should be a part of util/uuid
  this.id   = v4().slice(0, -i.length) + i // TODO use something other than uuid v4 ?
  this.name = x
  this.loc  = loc
  boxes[this.id] = this
}
Box.prototype.toString = function () {
  if (this.name != null) {
    return "#(box " + this.id + " " + this.name + ")"
  } else {
    return "#(box " + this.id + ")"
  }
}
exports.Box = Box

function getBox(i) {
  console.assert(i in boxes)
  return boxes[i]
}
exports.getBox = getBox

function toBox(x) {
  if (x.type === "x-nulan-box") {
    return x
  } else if (x.type === "Identifier") {
    if (vars.has(x.name)) {
      // TODO: not sure if this should enrich or not...
      //       it mostly affects macros:
      //
      //         $mac foo ->
      //           'sym "5"
      //         foo;
      return proxy(vars.get(x.name), x.loc)
    } else {
      throw new Error(x, "undefined symbol: " + x)
    }
  } else {
    return x
  }
}
exports.toBox = toBox

/*function checkMode(x, y) {
  if (x.mode !== mode.get()) {
    throw new Error(y, "undefined symbol: " + y + " (but it exists at " + x.mode + " time)")
  }
}*/

function macex1(a) {
  if (a.length === 0) {
    return {
      loc: a.loc,
      type: "UnaryExpression",
      operator: "void",
      prefix: true,
      argument: {
        loc: a.loc,
        type: "Literal",
        value: 0
      }
    }
  } else {
    var x = toBox(a[0])
    if (isObject(x) && isMacro in x) {
      return x[isMacro](a)
    } else {
      return {
        loc: a.loc,
        type: "CallExpression",
        callee: macex(a[0]),
        arguments: a.slice(1).map(macex)
      }
    }
  }
}

function macex(x) {
  if (x == null) {
    return macex([])
  } else if (typeof x === "number" || typeof x === "string" || typeof x === "boolean") {
    return {
      type: "Literal",
      value: x
    }
  } else if (Array.isArray(x)) {
    return macex1(x)
  } else if (x.type === "x-nulan-box") {
    if (isGet in x) {
      return x[isGet]([x])
    } else {
      return x
      /*checkMode(x, y)
      if (mode.get() === "run" || x.local) {
        return x
      } else {
        return compileBoxValue(x)
      }*/
    }
  } else if (x.type === "Identifier") {
    return macex(toBox(x))
  } else if (x.type === "Literal") {
    return x
  } else {
    throw new Error(x, "<macex> unexpected datatype: " + x)
  }
}
exports.macex = macex