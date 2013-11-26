define([], function () {
  "use strict";

  var mode = "run"
  
  return {
    mode: {
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
  }
})
