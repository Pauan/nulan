define(["./scope"], function (scope) {
  "use strict";
  
  var boxes = {}

  var vars = scope.make()
  
  var module = (function () {
    var a = []
    return {
      has: function () {
        return a.length !== 0
      },
      get: function () {
        return a[a.length - 1]
      },
      set: function (x, f) {
        a.push(x)
        try {
          return f()
        } finally {
          a.pop()
        }
      }/*,
      reset: function (f) {
        var old = a
        a = []
        try {
          return f()
        } finally {
          a = old
        }
      }*/
    }
  })()

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
    module: module,
  }
})
