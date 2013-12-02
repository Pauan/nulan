define(["./data", "./options", "./scope"], function (data, options, scope) {
  "use strict";
  
  var boxes = scope.make()
  
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
    if (x.value == null || options.minified) {
      var s = "a"
      while (symbols[s]) {
        s = getNextUniq(s)
      }
    } else {
      var s = x.value
        , i = 2
      while (symbols[s]) {
        s = x.value + i
        ++i
      }
    }
    return new data.Symbol(s)
  }
  
  function set(x, symbols) {
    if (x instanceof data.Op) {
      if (x.name === "var") {
        x.args.forEach(function (x) {
          if (x instanceof data.Op && x.name === "=") {
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
    if (x instanceof data.Box) {
      var y = boxToSym(x, symbols)
      boxes.set(x.id, y)
      findSymbols(y, symbols)
    }
  }
  
  function findSymbols(x, symbols) {
    if (x instanceof data.Op) {
      x.args.forEach(function (x) {
        findSymbols(x, symbols)
      })
    } else if (x instanceof data.Symbol) {
      symbols[x.value] = true
    } else if (x instanceof data.Box) {
      if (boxes.has(x.id)) {
        findSymbols(boxes.get(x.id), symbols)
      }
    }
  }

  function loop(x, symbols) {
    if (x instanceof data.Op) {
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
          return new data.Op(x.name, x.args.map(function (x) {
            return loop(x, symbols)
          }))
        })
      } else {
        return new data.Op(x.name, x.args.map(function (x) {
          return loop(x, symbols)
        }))
      }
    } else if (x instanceof data.Box) {
      if (boxes.has(x.id)) {
        return boxes.get(x.id)
      } else {
        throw new Error()
      }
    } else {
      return x
    }
  }

  function replace(x, symbols) {
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
