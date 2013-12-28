define(["../options", "../../lib/scope", "../util/data", "../util/print"], function (options, a, b, c) {
  "use strict";

  var Scope  = a.Scope
    , Symbol = b.Symbol
    , Box    = b.Box
    , Op     = b.Op
    , error  = c.error

  var reserved = {}

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
  ;("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with " +
    "class enum export extends import super " +
    "implements interface let package private protected public static yield " +
    "null true false").split(" ").forEach(function (s) {
    reserved[s] = true
  })
    
  function mangle(s) {
    if (reserved[s]) {
      return "_" + s
    } else {
      return options.mangle(s)
    }
  }
  
  /*function unmangle(s) {
    var x = /^_(.*)$/.exec(s)
    console.log(x)
    if (x && reserved[x[1]]) {
      return x[1]
    } else {
      return options.unmangle(s)
    }
  }*/
  
  var boxes = new Scope()
  
  function getNextUniq(s) {
    var r = s.split("")
      , i = r.length
    while (i--) {
      if (r[i] === "z") {
        r[i] = "a"
      } else {
        r[i] = String.fromCharCode(r[i].charCodeAt(0) + 1)
        return r.join("")
      }
    }
    return r.join("") + "a"
  }

  function boxToSym(x, symbols) {
    var s
    if (x.value == null || options.minified) {
      //return x
      s = "a"
      while (symbols[s]) {
        s = getNextUniq(s)
      }
    } else {
      var orig = mangle(x.value)
        , i    = 2
      s = orig
      while (symbols[s]) {
        s = orig + i
        ++i
      }
    }
    return new Symbol(s)
  }
  
  function set(x, symbols) {
    if (x instanceof Op) {
      if (x.name === "var-function") {
        setBox(x.args[0], symbols)
        // TODO this is probably unnecessary
        set(x.args[1], symbols)
      } else if (x.name === "var") {
        x.args.forEach(function (x) {
          if (x instanceof Op && x.name === "=") {
            x = x.args[0]
          }
          setBox(x, symbols)
        })
      } else if (x.name !== "function") {
        x.args.forEach(function (x) {
          set(x, symbols)
        })
      }
    }
  }
  
  function setBox(x, symbols) {
    if (x instanceof Box) {
      var y = boxToSym(x, symbols)
      boxes.set(x.id, y)
      setSym(y, symbols)
    }
  }
  
  function setSym(x, symbols) {
    symbols[x.value] = true
  }
  
  function findSymbols(x, symbols) {
    if (x instanceof Op) {
      x.args.forEach(function (x) {
        findSymbols(x, symbols)
      })
    } else if (x instanceof Symbol) {
      symbols[mangle(x.value)] = true
    } else if (x instanceof Box) {
      if (boxes.has(x.id)) {
        setSym(boxes.get(x.id), symbols)
      }
    }
  }

  function loop(x, symbols) {
    if (x instanceof Op) {
      if (x.name === "function") {
        return boxes.push({}, function () {
          symbols = {}
          x.args.forEach(function (x) {
            findSymbols(x, symbols)
          })
          x.args[0].args.forEach(function (x) {
            setBox(x, symbols)
          })
          set(x.args[1], symbols)
          return new Op(x.name, x.args.map(function (x) {
            return loop(x, symbols)
          }))
        })
      } else {
        return new Op(x.name, x.args.map(function (x) {
          return loop(x, symbols)
        }))
      }
    } else if (x instanceof Symbol) {
      // TODO loc
      return new Symbol(mangle(x.value))
    } else if (x instanceof Box) {
      if (boxes.has(x.id)) {
        return boxes.get(x.id)
      } else {
        error(x, [x])
      }
    } else {
      return x
    }
  }

  function replace(x) {
    var symbols = {}
    return boxes.reset({}, function () {
      findSymbols(x, symbols)
      set(x, symbols)
      return loop(x, symbols)
    })
  }

  return {
    replace: replace,
  }
})
