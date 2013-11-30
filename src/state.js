define(["./scope"], function (scope) {
  "use strict";
  
  var boxes = {}
    , vars  = scope.make()

  var mode = (function () {
    var mode = "run"
    return {
      get: function () {
        return mode
      },
      set: function (x, f) {
        var old = mode
        mode = x
        try {
          return f()
        } finally {
          mode = old
        }
      }
    }
  })()
  
  return {
    boxes: boxes,
    vars: vars,
    mode: mode,
  }
})
