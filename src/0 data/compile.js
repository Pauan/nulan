define(["../../lib/util/key", "../print", "./symbol"], function (a, b, c) {
  "use strict";

  var Key       = a.Key
    , isPrint   = b.isPrint
    , isComplex = b.isComplex
    , print     = b.print
    , Symbol    = c.Symbol
  
  var isCompile = new Key()
  
  function Op(s, a) {
    this.name = s
    this.args = a
  }
  Op.prototype[isComplex] = true
  Op.prototype[isPrint] = function (x, i) {
    return print([new Symbol(x.name)].concat(x.args), i)
    //var s = "#(op " + x.name + " "
    //return s + printNormal(x.args, s.length) + ")"
  }

  return {
    isCompile: isCompile,
  }
})