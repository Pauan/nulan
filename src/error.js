define(["./data", "./print"], function (data, print) {
  "use strict";

  function error(loc) {
    var r = []
    for (var i = 1; i < arguments.length; ++i) {
      var x = arguments[i]
      if (Array.isArray(x)) {
        r.push(print.normal(x[0], r.join("").length))
      } else {
        r.push(x)
      }
    }
    throw new data.Error(loc, r.join(""))
  }
  
  return error
})
