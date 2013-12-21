define(["../print"], function (a) {
  "use strict";
  
  var isPrint = a.isPrint

  function Symbol(x) {
    this.value = x
  }
  Symbol.prototype[isPrint] = function (x) {
    var s = x.value
    if (/^[0-9]+$|^[0-9]+\.[0-9]+$|[\(\)\{\}\[\]\.\\\'" \n]/.test(s)) {
      return "'" + s.replace(/['\\]/g, "\\$&").replace(/\n/g, "\\n") + "'"
    } else {
      return s
    }
  }
  
  function isSymbol(x, y) {
    return x instanceof Symbol && x.value === y
  }
  
  return {
    Symbol: Symbol,
    isSymbol: isSymbol,
  }
})