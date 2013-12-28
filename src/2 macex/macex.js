define(["../util/print", "../util/data"], function (a, b) {
  "use strict";

  var error   = a.error
    , toBox   = b.toBox
    , isMacro = b.isMacro
    , isMacex = b.isMacex
    , op      = b.op
    , opApply = b.opApply
    , Number  = b.Number
    , String  = b.String

  function macex1(a) {
    if (a.length === 0) {
      return op("empty", null)
    } else {
      var x = toBox(a[0])
      if (isMacro in x) {
        return x[isMacro](a)
      } else {
        return opApply("call", null, a.map(macex))
      }
    }
  }

  function macex(x) {
    /*if (x == null) {
      return macex([])
    } else */if (Array.isArray(x)) {
      return macex1(x)
    } else if (typeof x === "number") {
      return new Number(x)
    } else if (typeof x === "string") {
      return new String(x)
    } else if (isMacex in x) {
      return x[isMacex](x)
    } else {
      error(x, "[macex.js] unexpected datatype: ", [x])
    }
  }

  return {
    macex: macex,
  }
})