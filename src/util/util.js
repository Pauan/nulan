define(["../options", "../../lib/util/object", "./data", "./print", "../2 macex/macex", "../filesystem", "../3 compile/compile"], function (options, a, b, c, d, e, f) {
  "use strict";

  var is         = a.is
    , Symbol     = b.Symbol
    , String     = b.String
    , Box        = b.Box
    , isMacex    = b.isMacex
    , isSyntax   = b.isSyntax
    , toBox      = b.toBox
    , vars       = b.vars
    , mode       = b.mode
    , context    = b.context
    , opApply    = b.opApply
    , error      = c.error
    , macex      = d.macex
    , evalString = e.evalString
    , compile    = f.compile
  
  
  // symbol
  function isSymbol(x, y) {
    return x instanceof Symbol && x.value === y
  }
  
  
  // macex
  function bypass(x) {
    var o = {}
    o[isMacex] = function () {
      return x
    }
    return o
  }

  
  // array
  // TODO generic
  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }
  
  // TODO generic
  function arrayToIter(x) {
    var i = 0
    return {
      has: function () {
        return i < x.length
      },
      peek: function () {
        return x[i]
      },
      read: function () {
        return x[i++]
      }
    }
  }


  // box
  function isBoxOrSym(x) {
    return x instanceof Box || x instanceof Symbol
  }

  // TODO replace with generic equals check
  function isBox(x, y) {
    console.assert(y instanceof Box)
    /*if (x instanceof data.Symbol) {

    } else if (!(x instanceof Box)) {
      error(x, "expected box or symbol but got ", [x])
    }*/
    return is(toBox(x), y)
  }

  function checkBox(x, y) {
    // TODO: use isSym ?
    if (!isBox(x, y)) {
      // TODO is this good ?
      if (y instanceof Box) {
        y = new Symbol(y.value)
      }
      error(x, "expected ", [y], " but got ", [x])
    }
  }

  function setBox(x) {
    if (x instanceof Box) {
      return x
    } else if (x instanceof Symbol) {
      var o = new Box(x.value)
      o.loc = x.loc
      vars.set(x.value, o)
      return o
    } else {
      error(x, "expected symbol but got: ", [x])
    }
  }

  // TODO not sure if this should be in here or not...
  function getSyntax(x) {
    // TODO ew
    if (typeof x === "string" && vars.has(x)) {
      x = vars.get(x)
    } else if (x instanceof Symbol && vars.has(x.value)) {
      x = vars.get(x.value)
    }
    if (x instanceof Box && isSyntax in x) {
      return x[isSyntax]
    } else {
      return null
    }
  }
  
  function withNewScope(f) {
    return vars.push({}, f)
  }
  
  
  function compileOnlyError(x) {
    var s = mode.get()
    if (s !== "compile") {
      error(x, "cannot use ", [x], " at " + s + " time")
    }
  }

  // TODO syntax stuff, should probably be in a separate module ?
  function toString(x) {
    if (x instanceof Symbol) {
      var o = new String(x.value)
      o.loc = x.loc // data.loc(x, x)
      return o
    } else {
      return x
    }
  }
  
  function opargs(s, min, max) {
    return function (a) {
      checkArguments(a, min, max)
      return opApply(s, a[0], a.slice(1).map(macex))
    }
  }

  function compileEval(x) {
    return mode.set("compile", function () {
      x = compile(macex(x))
      options.$eval(x)
      if (context.get() == null) {
        throw new Error("Must set context before calling eval")
        //throw new Error("Nulan requires `options::eval` to be set to a function")
      }
      return evalString(x, context.get())
    })
  }

  function makeBoxSetter(s) {
    return function (a) {
      checkArguments(a, 2, 2)
      // TODO destructuring ?
      var x = toBox(a[1])
        , f = compileEval(a[2])
      x[s] = function (a) {
        a = a.slice(1)
        return macex(f.apply(this, a))
      }
      return macex([])
    }
  }

  return {
    loc: loc,
    isSymbol: isSymbol,
    bypass: bypass,
    unwrap: unwrap,
    arrayToIter: arrayToIter,
    isBoxOrSym: isBoxOrSym,
    isBox: isBox,
    checkBox: checkBox,
    setBox: setBox,
    getSyntax: getSyntax,
    withNewScope: withNewScope,
    compileOnlyError: compileOnlyError,
    toString: toString,
    checkArguments: checkArguments,
    opargs: opargs,
    compileEval: compileEval,
    makeBoxSetter: makeBoxSetter,
  }
})