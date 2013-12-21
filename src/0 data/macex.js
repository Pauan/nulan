define(["../../lib/util/key"], function (a) {
  "use strict";

  var Key = a.Key

  var isGet     = new Key()
    , isSet     = new Key()
    , isMacro   = new Key()
    , isPattern = new Key()
    , isSyntax  = new Key()

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

  var local = makeSetter(false)
    , mode  = makeSetter("run")
    , path  = makeSetter(null)

  return {
    isGet: isGet,
    isSet: isSet,
    isMacro: isMacro,
    isPattern: isPattern,
    isSyntax: isSyntax,

    mode: mode,
    local: local,
    path: path,
  }
})