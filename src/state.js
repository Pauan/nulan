define(["./scope"], function (scope) {
  "use strict";
  
  var boxes = {}
  var modules = {}

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
  
  function makeSetter(value) {
    return {
      get: function () {
        return value
      },
      set: function (x, f) {
        var old = value
        value = x
        try {
          return f()
        } finally {
          value = old
        }
      }
    }
  }
  
  var local  = makeSetter(false)
    , mode   = makeSetter("compile")
    , assign = makeSetter(false)
  
  return {
    modules: modules,
    boxes: boxes,
    vars: vars,
    mode: mode,
    local: local,
    module: module,
    assign: assign,
  }
})
