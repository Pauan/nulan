define(["../print"], function (a) {
  "use strict";
  
  var isPrint = a.isPrint
    , print   = a.print

  function Number(x) {
    this.value = x
  }
  String.prototype[isPrint] = function (x, i) {
    return print(x.value, i)
  }
  
  return {
    Number: Number,
  }
})