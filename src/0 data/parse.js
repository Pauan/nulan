define(["../print"], function (a) {
  "use strict";

  var isPrint = a.isPrint
    , print   = a.print

  function ParseStart() {}
  ParseStart.prototype[isPrint] = function () {
    return "["
  }

  function ParseEnd() {}
  ParseEnd.prototype[isPrint] = function () {
    return "]"
  }

  function ParseBypass(x) {
    this.value = x
  }
  ParseBypass.prototype[isPrint] = function (x, i) {
    return print(x.value, i)
  }

  return {
    ParseStart:  ParseStart,
    ParseEnd:    ParseEnd,
    ParseBypass: ParseBypass,
  }
})