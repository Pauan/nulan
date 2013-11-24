define(["../lib/util/buffer"], function (buffer) {
  "use strict";
  
  var Error = buffer.Error
  
  function Module() {
    this.imports = []
    this.exports = {}
  }
  
  function Op(s, a) {
    this.name = s
    this.args = a
  }
  
  function Box() {}
  
  function isSym(x, y) {
    return x instanceof Symbol && x.value === y
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
    loc: loc,
    unwrap: unwrap,
    isSym: isSym,

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
