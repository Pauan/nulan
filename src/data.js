define(["../lib/util/buffer", "../lib/util/name"], function (buffer, name) {
  "use strict";

  var macex   = new name.Name(),
      get     = new name.Name(),
      set     = new name.Name(),
      pattern = new name.Name(),
      syntax  = new name.Name()

  var Error = buffer.Error
  
  function Module() {}
  
  function Op(s, a) {
    this.name = s
    this.args = a
  }
  
  function Box() {}
  
  function isSym(x, y) {
    return x instanceof Symbol && x.value === y
  }
  
  function boxOrSym(x) {
    return x instanceof Box || x instanceof Symbol
  }
  
  // TODO generic
  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }
  
  function loc(x, y) {
    return {
      source: x.source,
      start: x.start,
      end: y.end
    }
  }

  function ParseStart() {}
  function ParseEnd() {}
  function ParseBypass(x) {
    this.value = x
  }
  function MacexBypass(x) {
    this.value = x
  }
  function Symbol(x) {
    this.value = x
  }
  function Number(x) {
    this.value = x
  }
  function String(x) {
    this.value = x
  }
  
  return {
    macex: macex,
    get: get,
    set: set,
    pattern: pattern,
    syntax: syntax,

    loc: loc,
    unwrap: unwrap,
    isSym: isSym,
    boxOrSym: boxOrSym,

    Module: Module,
    Box: Box,
    Error: Error,
    Op: Op,
    MacexBypass: MacexBypass,
    ParseStart: ParseStart,
    ParseEnd: ParseEnd,
    ParseBypass: ParseBypass,
    Symbol: Symbol,
    Number: Number,
    String: String,
  }
})
