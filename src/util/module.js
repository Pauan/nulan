define(["../options", "../../lib/util/iter", "../1 parse/tokenize", "../1 parse/indent", "../1 parse/parse", "../2 macex/macex", "./data", "../filesystem"], function (options, iter, a, b, c, d, e, f) {
  "use strict";

  var tokenize = a.tokenize
    , indent   = b.indent
    , parse    = c.parse
    , macex    = d.macex
    , vars     = e.vars
    , local    = e.local
    , path     = e.path
    , relToAbs = f.relToAbs
    , readFile = f.readFile

  /*function relToAbs(abs, rel) {
    var a1 = (rel[0] === "/"
               ? []
               : abs.split(/\//))
      , a2 = rel.split(/\//)
    a2.forEach(function (x) {
      if (x === "..") {
        if (a1.length === 1) {
          throw new Error("invalid module path: " + rel)
        } else {
          a1.pop()
        }
      } else if (x !== ".") {
        a1.push(x)
      }
    })
    return a1.join("/")
  }*/

  var moduleCache = {}

  function getModulePath(s) {
    return relToAbs(s)
  }

  function getModule(s) {
    s = getModulePath(s)
    var o = moduleCache[s]
    if (o == null) {
      o = moduleCache[s] = {}
      // TODO should reset mode to "run" ?
      // TODO better resetting
      local.set(false, function () {
        path.set(s, function () {
          vars.push(o, function () {
            macexString(readFile(s))
          })
        })
      })
    }
    return o
  }

  function macexString(s) {
    var o = tokenize(s)
      , r = []

    iter.each(o, function (x) {
      x = indent(x)
      x = parse(x)
      x = macex(x)
      r.push(x)
    })

    return r
  }

  return {
    moduleCache: moduleCache,
    getModule: getModule,
    macexString: macexString,
  }
})
