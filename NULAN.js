var NULAN = (function (n) {
  "use strict";

  Object.prototype.tap = function () {
    console.log(require("util").inspect(n.toJSON(this), false, null, false))
    return this
  }


  var mode = "compile" // Whether code is evaluated at run-time or compile-time

  function withMode(s, f) {
    var old = mode
    mode = s
    try {
      var x = f()
    } finally {
      mode = old
    }
    return x
  }


  function Macro(x) { this.value = x }

  function macro(f) {
    return new Macro(function (a) {
      return f.apply(this, a.slice(1))
    })
  }

  function compileOnlyError(x) {
    if (mode === "run") {
      throw new n.Error(x, "cannot use " + x + " at run time")
    }
  }

  function compileOnly(f) {
    return new Macro(function (a) {
      compileOnlyError(a[0])
      return f.apply(this, a.slice(1))
    })
  }


  function complex(x) {
    return Array.isArray(x)
  }

  function boxOrSym(x) {
    return x instanceof n.Box || x instanceof n.Symbol
  }

  function wildcard(x) {
    return x instanceof n.Symbol && x.value === "_"
  }


  function concater(l, r) {
    return l.length === 0
             ? r.length === 1
                 ? r[0]
                 : ["call", [".", r[0], "concat"], r.slice(1)]
             : ["call", [".", ["array", l], "concat"], r]
  }

  function arraySplitter(a, when, each, end) {
    var x, x2, l = [], r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      x = a[i]
      x2 = when(x)
      if (x2) {
        r.push(each(x2, true))
        ++i

        while (i < iLen) {
          x = a[i]
          x2 = when(x)
          if (x2) {
            r.push(each(x2, true))
          } else {
            r.push(["array", [each(x, false)]])
          }
          ++i
        }

        return end(concater(l, r), true)
      } else {
        l.push(each(x, false))
      }
    }
    return end(l, false)
  }

  function splicingArgs(f, a) {
    return arraySplitter(a, function (x) {
      if (Array.isArray(x) && isBoxM(x[0], "@")) {
        return x[1]
      }
    }, function (x) {
      return mac(x)
    }, function (x, b) {
      if (b) {
        return ["call", [".", f, "apply"], [["null"], x]]
      } else {
        return ["call", f, x]
      }
    })
/*
    var x, r2, r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      x = a[i]
      if (Array.isArray(x) && isValue(x[0], "@")) {
        r2 = splicingArgsRest(mac(x[1]), i + 1, iLen, a)
        return ["call", [".", f, "apply"],
                        [["null"],
                         (r.length === 0 && r2.length === 1
                           ? r2[0]
                           : ["call", [".", ["array", r], "concat"], r2])]]
      } else {
        r.push(mac(x))
      }
    }
    return ["call", f, r]*/
  }

  // TODO: ugh I wish I could do this natively in JS
  function pair(a) {
    var r = []
    for (var i = 0, iLen = a.length - 1; i < iLen; i += 2) {
      r.push([a[i], a[i + 1]])
    }
    return r
  }

  function binr(s, a, i) {
    return a.reduce(function (x, y) {
      return [s, x, y]
    })
  }

  function binreduce0(s, i) {
    return macro(function () {
      switch (arguments.length) {
      case 0:
        return i
      case 1:
        return mac(arguments[0])
      default:
        return binr(s, [].map.call(arguments, mac))
      }
    })
  }

  function binreduce1(s, i) {
    return new Macro(function (a) {
      switch (a.length) {
      case 1:
        throw new n.Error(a[0], a[0] + " cannot be called with 0 arguments")
      case 2:
        if (typeof i === "function") {
          return i(mac(a[1]))
        } else {
          return mac(a[1])
        }
      default:
        return binr(s, a.slice(1).map(mac))
      }
    })
  }
/*
  (is 1 2 3 4)

  (and (is 1 2) (is 2 3) (is 3 4))

  (is (foo 1) (bar 2) 3 4)

  (let u (foo 1)
    (let v (bar 2)
      (and (is u v) (is v 3) (is 3 4))))
*/

  function binand(s, i) {
    return macro(function () {
      switch (arguments.length) {
      case 0:
        return i
      case 1:
        return i
      case 2:
        return [s, mac(arguments[0]), mac(arguments[1])]
      default:
        var r = []
        ;[].slice.call(arguments).map(mac).reduce(function (x, y) {
          r.push([x, y])
          return y
        })
        return r.map(function (x) {
          return [s].concat(x)
        }).reduce(function (x, y) {
          return ["&&", x, y]
        })
      }
    })
  }

  function unary(s) {
    return macro(function (x) {
      return [s, mac(x)]
    })
  }

  function binary(s) {
    return macro(function (x, y) {
      return [s, mac(x), mac(y)]
    })
  }

  /*function withPartialScope(args, f) {
    var saved = args.map(function (x) {
      if (Array.isArray(x)) {
        x = x[1]
      }
      var s = symToString(x)
                  // TODO: better detection if vars is undefined
        , r = [s, vars[s], locals[s]]
      locals[s] = true // TODO
      return r
    })

    var r = f()

    saved.forEach(function (a) {
      var s = a[0]
      if (a[1]) {
        vars[s] = a[1]
      } else {
        delete vars[s]
      }
      // TODO
      if (a[2]) {
        locals[s] = true
      } else {
        delete locals[s]
      }
    })

    return r
  }*/

  function slicer(v, i, iLen) {
    var r = [[boxM("."),
              [boxM("."), [boxM("list")], "slice"],
               "call"],
             v]
    var i2 = i - iLen + 1
    if (i2 !== 0 || i !== 0) {
      r.push(i)
    }
    if (i2 !== 0) {
      r.push(i2)
    }
    return r
  }


  var mangle, unmangle, validJS

  ;(function () {
    // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
    var reserved = ("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with" +
                    " class enum export extends import super" +
                    " implements interface let package private protected public static yield" +
                    " arguments null true false undefined").split(" ").join("|")

    var to = new RegExp("^(" + reserved + ")$|(^[0-9])|([a-z])\-([a-z])|[^$a-z0-9]", "g")

    mangle = function (s) {
      return s.replace(to, function (s, s1, s2, s3, s4) {
        // TODO: a teensy bit hacky
        if (s1 || s2) {
          return "_" + (s1 || s2)
        } else if (s3) {
          return s3 + s4.toLocaleUpperCase()
        } else {
          return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
        }
      })
    }

    // mangle("50fooBar-qux")

    var from = new RegExp("_([0-9]*)_|^_(" + reserved + "|[0-9])|([a-z])([A-Z])", "g")

    // Not actually used, but still nice to have
    unmangle = function (s) {
      return s.replace(from, function (_, s, s1, s2, s3) {
        if (s1) {
          return s1
        } else if (s2) {
          return s2 + "-" + s3.toLocaleLowerCase()
        } else {
          return s === "" ? "_" : String.fromCharCode(s)
        }
      })
    }

    validJS = function (x) {
      if (typeof x === "string") {
        // TODO: code duplication with mangle
        x = x.replace(/([a-z])\-([a-z])/g, function (_, s1, s2) {
          return s1 + s2.toLocaleUpperCase()
        })
        if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)) {
          return x
        }
      }
    }
  })()


  n.vars    = {} // Nulan Variable -> JS Variable (guaranteed unique)
  var boxes = {} // JS Variable    -> Box

  var Uniq, setBuiltin, setSymToBox, withLocalScope

  ;(function () {
    var js_vars = {} // JS Variable -> true (if variable exists in JS)
      , values  = {} // JS Variable -> compile time value
      , local

    function findUniq(sOld) {
      var s = sOld
        , i = 2
      while (js_vars[s]) {
        s = sOld + i
        ++i
      }
      js_vars[s] = true
      return s
    }

    function getUniq() {
      var s, i = 1
      while (true) {
        for (var a = "a".charCodeAt(0), b = "z".charCodeAt(0); a <= b; ++a) {
          s = new Array(i + 1).join(String.fromCharCode(a))
          if (!js_vars[s]) {
            js_vars[s] = true
            return s
          }
        }
        ++i
      }
    }

    n.Box = function (s) {
      this.name = this.value = s
      this.mode = {}
    }
    /*n.Box.prototype.uniqize = function () {
      //this.local = local
      //this.mode  = mode
      this.value = findUniq(this.name)
    }
    n.Box.prototype.mangle = function () {
      this.value = mangle(this.name)
      return this
    }*/
    n.Box.prototype.toString = function () {
             // TODO: should this use unmangle?
      return unmangle(this.value)
    }

    Uniq = function () {}
    Uniq.prototype = new n.Box()
    /*Uniq.prototype.uniqize = function () {
      // TODO
      this.local = local
      //this.mode  = mode
      this.value = getUniq()
    }*/

    setBuiltin = function (sN, sJS) {
      var x = new n.Box(sJS)
      // TODO: code duplication with setSymToBox
      x.local = local
      x.mode[mode] = true // TODO
      n.vars[sN] = x.value
      return x
    }

    setSymToBox = function (x) {
      var s
      if (x instanceof n.Symbol) {
        s = x.value
        x = setSymToBox(new n.Box(mangle(x.value)))
        n.vars[s] = x.value
        return x
      } else if (x instanceof Uniq) {
        x.value = getUniq() // TODO: assign this to boxes too?
      } else if (x instanceof n.Box) {
        x.value = findUniq(x.name)
        boxes[x.value] = x
      }
      x.local = local
      x.mode[mode] = true
      return x
    }

    withLocalScope = function (f) {
      var old  = js_vars
        , old2 = boxes // TODO: does this need to save boxes too?
        , old3 = local
      js_vars = Object.create(js_vars)
      boxes   = Object.create(boxes)
      local   = true
      try {
        var x = f()
      } finally {
        js_vars = old
        boxes   = old2
        local   = old3
      }
      return x
    }
  })()


  setBuiltin("%t", "true")
  setBuiltin("%f", "false")


  // TODO
  ;(function () {
    n.Wrapper = function (value) {
      this.value = value
    }
    n.Symbol = function (value) {
      this.value = value
    }

    Uniq.prototype.toString =
    n.Wrapper.prototype.toString =
    n.Symbol.prototype.toString = function () {
      return this.value
    }
  })()


  function withNewScope(f) {
    var old = n.vars
    n.vars = Object.create(n.vars)
    try {
      var x = f()
    } finally {
      n.vars = old
    }
    return x
  }


  var isMacro, setValue, compileEval

  ;(function () {
    var values = {} // Boxes -> Compile-time values

    isMacro = function (x) {
      if ((x = getBox(x)) && (x = values[x.value]) instanceof Macro) {
        return x.value
      }
    }

    setValue = function (x, y) {
      x = setSymToBox(new n.Symbol(x))
      //b.mode = "compile" // TODO: a teensy bit hacky
      //b.uniqize() // TODO: does this need to uniqize?
      //n.vars[x] = b
      values[x.value] = y
    }

    // This needs to be in here because `eval` needs access to `values`
    compileEval = function (x) {
      return withMode("compile", function () {
        x = mac(x)
        x = NINO.compile(NINO.transform([x]))
        console.log(x)
        return ["id", eval(x)]
      })
    }
  })()


  function getBox(x) {
    if (x instanceof n.Box) {
      return x
    } else if (x instanceof n.Symbol) {
      var y = n.vars[x.value]
      if (y) {
        return boxes[y]
      } else {
        throw new n.Error(x, "undefined variable: " + x)
      }
    }
  }

  function checkBox(x, y) {
    // TODO: x.local ||
    if (mode === "quote" || y.mode[mode]) {
      return y
    } else {
      console.log(mode, y.mode)
      throw new n.Error(x, "undefined variable (but it exists at " +
                             // TODO
                             (mode === "compile" ? "run" : "compile") +
                             " time): " + x)
    }
  }

  function boxM(s) {
    // TODO mangle
    return boxes[mangle(s)]
  }

  /*function isSym(x, s) {
    if (x instanceof n.Box) {
      return x.name === s
    } else {
      return false
    }
  }*/

  function isBox(x, s) {
    x = getBox(x)
    return x instanceof n.Box && x.value === s
  }

  function isBoxM(x, s) {
    return isBox(x, mangle(s))
  }


  function mac(a) {
    var x
    if (Array.isArray(a)) {
      if ((x = isMacro(a[0]))) {
        return x(a)
      } else if (a.length === 0) { // TODO: this should probably be in splicingArgs
        return ["empty"] // TODO: is this correct?
        //return ["void", ["number", "0"]]
      } else {
        return splicingArgs(mac(a[0]), a.slice(1))
      }
    } else if (a instanceof n.Wrapper) {
      return mac(a.value) // TODO: check all the places that currently expect strings/numbers and won't work with a wrapper
    } else if (typeof a === "number") {
      return ["number", "" + a]
    } else if (typeof a === "string") {
      return ["string", a]
    /*} else if (a === void 0) { // TODO
      return ["void", ["number", "0"]]*/
    } else if (a instanceof n.Symbol) {
      return mac(checkBox(a, getBox(a)))
    } else if (a instanceof n.Box) {
      x = a.value

      if (a.local || mode === "run") {
        return ["name", x]
      } else if (mode === "compile") {
        return [".", ["name", "values"], x]
      } else if (mode === "quote") {
        return [".", ["name", "boxes"], x]
        //return ["call", [".", ["name", "n"], "getBox"], [mac(x)]]
        //return ["new", [".", ["name", "n"], "Box"], [mac(x)]]
        //return mac([box("&box"), x])
      } else {
        throw new n.Error(a, "invalid mode: " + mode) // TODO
      }
    } else {
      throw new n.Error(a, "invalid expression: " + a)
    }
  }

  // TODO
  function $mac(a) {
    var x = a[0]
    // TODO: make it work with boxes too
    if (x instanceof n.Symbol) {
      x = ["name", x.value]
    } else if (typeof x !== "string") {
      throw new n.Error(x, "invalid expression: " + x)
    }
    return (a.length === 1
             ? x
             : [x].concat([].slice.call(a, 1).map(mac)))
  }

/*  function splicingArgsRest(x, i, iLen, a) {
    var r = [x]
    while (i < iLen) {
      x = a[i]
      if (Array.isArray(x) && isBoxM(x[0], "@")) {
        r.push(mac(x[1]))
      } else {
        r.push(["array", [mac(x)]])
      }
      ++i
    }
    return r
  }*/


  var patterns = {}

  patterns[mangle("list")] = function (args, v, body) {
    args = args.slice(1)
    var index
    args.forEach(function (x, i) {
      if (Array.isArray(x) && isBoxM(x[0], "@")) {
        index = i
      }
    })
    return args.reduceRight(function (x, y, i, a) {
      if (i === index) {
        return destructure1(y[1], slicer(v, i, a.length), x)
      } else if (i > index) {
        return destructure1(y, [boxM("."), v,
                                [boxM("-"),
                                 [boxM("."), v, "length"],
                                 a.length - i]],
                               x)
      } else {
        return destructure1(y, [boxM("."), v, i], x)
      }
    }, body)
  }

  patterns[mangle("dict")] = function (args, v, body) {
    args = args.slice(1)
    return pair(args).reduceRight(function (x, y) {
      return destructure1(y[1], [boxM("."), v, y[0]], x)
    }, body)
  }

  patterns[mangle("=")] = function (args, v, body) {
    args = args.slice(1)
    return destructure1(args[0], [boxM("if"), [boxM("null?"), v], args[1], v], body)
  }

  patterns[mangle("'")] = function (args, v, body) {
    compileOnlyError(args[0])

    var x = getBox(args[1])
    return [boxM("if"), [boxM("&box=="), v, x.value],
                        body,
                        [boxM("&error"), v, [boxM("+"), "expected " + args[1] + " but got ", v]]]
  }


  function destructure1(args, v, body) {
    if (complex(args)) {
      var u = new Uniq()
      return [boxM("|"), [boxM("var"), [boxM("="), u, v]], destructure(args, u, body)]
    } else {
      return destructure(args, v, body)
    }
  }

  function destructure(args, v, body) {
    var a, f
    if (wildcard(args)) {
      return body
    } else if (boxOrSym(args)) {
      a = [boxM("var"), [boxM("="), args, v]]
              // TODO better detection for if body is empty
      return (body
               ? [boxM("|"), a, body]
               : a)
    } else if (Array.isArray(args)) {
      if ((f = getBox(args[0])) && (f = patterns[f.value])) {
        return f(args, v, body)
      } else {
        throw new n.Error(args[0], args[0] + " is not a pattern")
      }
    } else {
      return [boxM("if"),
               [boxM("=="), v, args],
               body,
               [boxM("error"), [boxM("+"), "expected " + args + " but got ", v]]]
    }
  }


  // Simple complexity
  setValue("num",         unary("u+"))
  setValue("~",           unary("!"))
  setValue("&typeof",     unary("typeof"))

  setValue("&in",         binary("in"))
  setValue("&instanceof", binary("instanceof"))
  setValue("mod",         binary("%"))

  setValue("==",          binand("===", ["boolean", "true"]))
  setValue("<",           binand("<",   ["boolean", "true"]))
  setValue("=<",          binand("<=",  ["boolean", "true"]))
  setValue(">",           binand(">",   ["boolean", "true"]))
  setValue(">=",          binand(">=",  ["boolean", "true"]))

  setValue("&&",          binreduce0("&&", ["boolean", "true"]))
  setValue("||",          binreduce0("||", ["boolean", "false"]))
  setValue("+",           binreduce0("+",  ["number", "0"]))
  setValue("*",           binreduce0("*",  ["number", "1"]))

  setValue("-",           binreduce1("-", function (x) { return ["u-", x] }))
  setValue("|",           binreduce1(","))
  setValue("/",           binreduce1("/"))

  setValue("&break", macro(function () {
    return ["break"]
  }))

  setValue("$eval", macro(function (x) {
    return compileEval(x)
  }))

  setValue("&", macro(function () {
    return $mac(arguments)
  }))

  setValue("<=", macro(function (x, y) {
    return ["=", mac(x), mac(y)]
  }))

  setValue("list", macro(function () {
    return ["array", [].map.call(arguments, mac)]
  }))

  setValue(".", macro(function (x, y) {
    var s = validJS(y)
    if (s) {
      return [".", mac(x), s]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  }))

  // TODO
  setValue(",")
  setValue("@")

/*
  setValue(",", macro(function (x) {
    return mac(x)
  }))

  setValue("@", macro(function (x) {
    // TODO
    throw new n.Error(x, "invalid use of @")
  }))
*/

  setValue("null?", macro(function (x) {
    return ["==", mac(x), ["null"]]
  }))

  // TODO: even though it's an ordinary macro, I can't move it into NULAN.macros because "var" depends upon it
  setValue("=", macro(function (x, y) {
    return mac([boxM("if"), [boxM("null?"), x], [boxM("<="), x, y]])
  }))

  setValue("++", macro(function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["++", mac(x)]
    } else {
      return ["+=", mac(x), mac(y)]
    }
  }))

  setValue("--", macro(function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["--", mac(x)]
    } else {
      return ["-=", mac(x), mac(y)]
    }
  }))

  setValue("new", macro(function (x) {
    var args = [].slice.call(arguments, 1)
    return ["new", mac(x), args.map(mac)]
  }))

  setValue("while", macro(function (test, body) {
    return ["while", mac(test), [mac(body)]]
  }))

  setValue("w/new-scope", macro(function (body) {
    return withNewScope(function () {
      return mac(body)
    })
  }))

  setValue("if", macro(function anon() {
    var a = arguments
    switch (a.length) {
    case 0:
      return ["void", ["number", "0"]]
    case 1:
      return mac(a[0])
    case 2:
      return ["if", mac(a[0]), [mac(a[1])], []]
    case 3:
      return ["if", mac(a[0]),
               [withNewScope(function () { return mac(a[1]) })],
               [withNewScope(function () { return mac(a[2]) })]]
    // TODO: maybe simplify this a bit?
    default:
      return ["if", mac(a[0]), [mac(a[1])],
                    [anon.apply(this, [].slice.call(a, 2))]]
    }
  }))

  setValue("error", macro(function (x) {
    return ["throw", ["new", ["name", "Error"], [mac(x)]]]
  }))

  setValue("finally", macro(function (x, y) {
    return ["try", [mac(x)], ["finally", [mac(y)]]]
    /*var u = new Uniq()
    return ["try", [mac([values["var"], [u, x]])], ["finally", [mac(y), mac(u)]]]*/
  }))


  // Medium complexity
  // TODO: make it work at compile-time
  // var foo = 5
  // $eval
  //   var foo = 10
  // $eval
  //   del foo
  // foo
  setValue("del", macro(function (x) {
    // TODO: make it work with boxes too?
    if (x instanceof n.Symbol) {
      var s = mac(x)
      delete n.vars[x.value]
      return s
    } else {
      // TODO: return the value that's being deleted
      return ["delete", mac(x)]
    }
  }))

  setValue("dict", macro(function () {
    var args = pair(arguments)
      , a
      , u
    // TODO: make it work with boxes too
    if (args.every(function (a) { return a[0] instanceof n.Symbol })) {
      a = args.map(function (a) {
        return [a[0].value, mac(a[1])]
      })
      return ["object", a]
    } else {
      u = new Uniq()
      a = args.map(function (a) {
        var x = a[0]
          , y = a[1]
        // TODO: make it work with boxes too?
        if (x instanceof n.Symbol) {
          x = x.value
        }
        return [boxM("<="), [boxM("."), u, x], y]
      })
      return mac([boxM("|"), [boxM("var"), [boxM("="), u, [boxM("dict")]]]].concat(a))
    }
  }))

  setValue("'", new Macro(function (a) {
    var s = a[0]

    function loop(x) {
      // TODO: a little bit hacky, but it'll do for now
      if (Array.isArray(x)) {
        if (isBoxM(x[0], ",") && (x = x[1])) {
          if (Array.isArray(x) && isBoxM(x[0], "@")) {
                                 // TODO: a teensy bit hacky
            throw new n.Error({ text:   s.text
                              , column: s.column
                              , line:   s.line
                              , length: x[0].column - s.column + 1 }, "',@ is invalid")
          } else {
            return mac(x)
          }
        /*} else if (isBoxM(x[0], "'")) {
          return loop(x[1])
          //console.log("FOO")
          // .concat()
          x = loop(x[1])
          x[1].unshift(["call", ["name", "box"], [["string", "list"]]])
          console.log(x)
          return x*/
        } else {
          return arraySplitter(x, function (x) {
            if (Array.isArray(x) && isBoxM(x[0], ",") && (x = x[1]) &&
                Array.isArray(x) && isBoxM(x[0], "@") && (x = x[1])) {
              return x
            }
          }, function (x, b) {
            if (b) {
              return mac(x)
            } else {
              return loop(x)
            }
          }, function (x, b) {
            return b ? x : ["array", x]
          })
        }
      } else if (wildcard(x)) {
        // TODO: a little hacky
        return mac([boxM("sym"), x.value])
      } else if (boxOrSym(x)) {
        return withMode("quote", function () {
          return mac(x)
        })
      } else {
        return mac(x)
      }
    }

    compileOnlyError(s)
    return loop(a[1])
  }))

  setValue("&box==", function (x, y) {
    return isBox(x, y)
  })

  setValue("&box==", compileOnly(function (x, y) {
    return ["call", ["name", "isBox"], [mac(x), mac(y)]]
  }))

  setValue("include", macro(function () {
    [].forEach.call(arguments, function (x) {
      setSymToBox(x) // TODO: is this correct?
      //setNewBox(x)
                       // (&eval '(&list 1 2 3))
                       // (include &list)
                       // (&eval '(&list 1 2 3))
    })
    return ["empty"]
  }))

  setValue("make-uniq", compileOnly(function () {
    return ["new", ["name", "Uniq"], []]
  }))

  setValue("sym", compileOnly(function (x) {
    return ["new", [".", ["name", "n"], "Symbol"], [mac(x)]]
  }))

  setValue("&error", compileOnly(function (x, y) {
    return ["throw", ["new", [".", ["name", "n"], "Error"], [mac(x), mac(y)]]]
  }))


  // Heavy complexity
  setValue("->", macro(function (args, body) {
    if (Array.isArray(args)) {
      return withLocalScope(function () {
        return withNewScope(function () {
  /*
          var slice = Function.prototype.call.bind([].slice)
          slice(arguments)
  */
          var x, u, s, r = []

          for (var i = 0, iLen = args.length; i < iLen; ++i) {
            x = args[i]

            if (boxOrSym(x) && !wildcard(x)) {
              x = setSymToBox(x)
              r.push(x.value)

            } else if (Array.isArray(x) && isBoxM(x[0], "@")) {
              s = new n.Box("arguments") // TODO: ew
              s.local = true // TODO: ew

              u = (i !== iLen - 1
                    ? new Uniq()
                    : s)

              ;(function (i2) {
                var x
                while (i2 > i) {
                  x = args[i2]
                                        // TODO: code duplication with the list pattern
                  body = destructure(x, [boxM("."), u,
                                          [boxM("-"),
                                           [boxM("."), u, "length"],
                                           args.length - i2]],
                                        body)
                  --i2
                }
              })(iLen - 1)

              body = destructure(x[1], slicer(u, i, args.length), body).tap()

              if (i !== iLen - 1) {
                body = destructure(u, s, body)
              }
              break

            } else {
              u = new Uniq()
              u = setSymToBox(u) // TODO: this is called only for uniqize, is that correct?
              //uniqize(u)
              //setVar(u) // TODO: does this need to be added to vars?
              r.push(u.value)
              body = destructure(x, u, body)
            }
          }

          return ["function", "", r, [["return", mac(body)]]]
        })
      })
    } else {
      throw new n.Error(args, "invalid function argument list: " + args)
    }
  }))

  setValue("var", macro(function () {
    var args = [].slice.call(arguments)

    /*function before(x) {
      if (Array.isArray(x)) {
        if (isSymbol(x[1])) {
          return [x[1], mac(x[2])]
        } else {
          pretty(mac(destructure(x[1], x[2])))
        }
      } else {
        return [x, void 0]
      }
    }*/

    return args.map(function (x) {
      var y
      if (Array.isArray(x)) {
        // TODO: use isSym ?
        if (!isBoxM(x[0], "=")) {
          throw new n.Error(x[0], "expected ('=) but got " + x[0])
        }
        y = x[2]
        x = x[1]
      }
      if (wildcard(x)) {
        return mac(y)
      } else if (boxOrSym(x)) {
        if (y !== void 0) {
          y = mac(y)
        }

        x = setSymToBox(x)

        if (!x.local && mode === "compile") {
          return ["=", [".", ["name", "values"], x.value], y]
        } else {
          if (y === void 0) {
            return ["var", [[x.value]]]
          } else {
            return ["var", [[x.value, y]]]
          }
        }
      } else {
        // TODO: code duplication with destructure1
        if (complex(y)) {
          var u = new Uniq()
          return mac([boxM("|"), [boxM("var"), [boxM("="), u, y]], destructure(x, u)])
        } else {
          return mac(destructure(x, y))
        }
      }
    }).reduce(function (x, y) {
      return [",", x, y]
    })
  }))


  // TODO
  //setValue("$syntax-rules", n.syntaxRules)


/*
  // Non-primitives, but it's easier to define them in here than in NULAN.macros
  setValue("$run", macro(function (x) {
    return mac([boxM("$eval"), [boxM("|"), x, []]])
  }))

  setValue("w/var", macro(function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    return mac([boxM("w/new-scope"), [boxM("|"), [boxM("var")].concat(args), body]])
  }))*/

/*
  setValue("$mac", macro(function (s, v) {
    return withMode("compile", function () {
      s = setSymToBox(s)

      v = compileEval(v)[1]

      // TODO: unmangle this?
      setBoxValue(s, macro(function () {
        return mac(v.apply(this, arguments))
      }))

      return ["empty"]
    })
  }))
*/

/*

$mac foo -> 10

foo;





def foo -> x y
  x + y

$syntax-infix foo 9001

1 foo 2


function infix(i, b, f) {
    return {
      delimiter: b,
      priority: i,
      action: function (l, s, r) {
        var x = l[l.length - 1]
          , y = r[0]

        return l.slice(0, -1).concat([f ? f(x, s, y) : [s, x, y]], r.slice(1))
      }
    }
  }
*/


  setValue("make-macro", compileOnly(function (x) {
    return ["new", ["name", "Macro"], [mac(x)]]
  }))

  setValue("&compile", compileOnly(function (x) {
    return ["call", ["name", "mac"], [mac(x)]]
  }))


  /*setValue("w/var", function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    var x = args.map(function (x) { return x[0] })
      , y = args.map(function (x) { return x[1] })

    return mac([[boxM("->"), x, body]].concat(y))
  })*/
/*
  (var + 5)

  (mac foo ->
    {+ 10 20})

  (mac something -> x
    (&eval x))

  (something var)

  -> x y; x + y

  ->; x y; foo x y


  [ "foo" -> x y
      bar
      qux
      corge
  | "bar" -> x y
      bar
      qux
      corge ]

  {
    foo: function (x, y) {
      bar
      qux
      return corge
    },
    bar: function (x, y) {
      bar
      qux
      return corge
    }
  }

  [ "foo" f "bar" g ]
  ->
    x y
    foo x y
*/

/*
  (mac foo ->
    (let u (uniq)
      '(let u 5 u)))

  (mac foo ->
    (let u (uniq)
      '("let" u 5 u)))

  {a b @c}  -> {a b @c}
  '{a b @c} -> {list a b {splice c}}

  (let n (sym "foo")
    '(n bar qux))

  (mac foo -> (let n (sym "list") '(n 1 2)))

  (mac foo -> {"list" 1 2})

  (mac foo -> (let u (uniq) (let v (uniq) '(fn {u v} (add u v)))))

  mac foo ->
    let u (uniq)
      let v (uniq)
        'fn {u v}
           add u v
*/

  /*setValue("namespace", new Macro(function (x) {
    namespace = x
    js_vars   = {}
    return ["empty"]
  })*/

  /*function reducer(a, f) {
    return [].map.call(a, mac).reduce(f)
  }*/

  /*setValue("w/namespace", new Macro(function (x, body) {
    var old1 = namespace
      , old2 = js_vars
    namespace = x
    js_vars   = {}
    var r = mac(body)
    //var r = binr(",", [].slice.call(arguments, 1).map(mac))
    namespace = old1
    js_vars   = old2
    return r
  })*/

/*
  (&eval (var mac (&mac -> n v
    (let u (uniq)
      {&eval
        {var n

        {fn u
        {apply v u}

        }}}
    )))

  (mac foo -> a b {a b})

  (&eval (var foo (&mac -> a b (&compile {a b}))))

  (&eval (var foo (&mac (&compiler -> a b {a b}))))

  (&compile )

  (mac foo -> (let u (uniq) {let u 5 u}))
*/
/*
  $mac foo -> a b c

  $eval
    | var u
    | u <= -> a b c
    | def foo -> @args
        &macex (u @args)
*/
/*
(define (pattern-match-if test expected got body)
  (list _if test
            body
            (list error (format "invalid pattern: expected ~s but got" expected) got)))

; (depends pattern-match-if is?)
(define (pattern-match-is x u body)
  (pattern-match-if (list is? u x) x u body))

; (mutual pattern-match)
; (depends _let update env)
(define (pattern-match1 x u body)
  (if (pair? x)
      (let ((v (gensym)))
        (update env v v)
        (list _let v u (pattern-match x v body)))
      (pattern-match x u body)))

; (mutual pattern-match1)
; (depends _let _quote pattern-car pattern-match-if pattern-match-is)
(define (pattern-match x u body)
  (cond ((symbol? x)
          (list _let x u body)
          #|(let ((b (box #f)))
            (update env x b)
            (displayln x)
            (displayln (lookup (cadr u)))
            (list do (list unsafe-box*-cas! b #f u)
                     body))|#
        )
        ((pair? x)
          (cond ((eq? (car x) 'list)
                  ; TODO: can probably use unsafe-car and unsafe-cdr
                  (let loop ((x (cdr x)))
                    (if (null? x)
                        body
                        (pattern-match1 (car x) (list (pattern-car x) u)
                          (pattern-match u
                            (list unsafe-cdr u)
                            (if (null? (cdr x))
                                (pattern-match-if (list null? u)
                                  null
                                  u
                                  (loop (cdr x)))
                                (loop (cdr x))))))))
                ; TODO: can probably use unsafe-car and unsafe-cdr
                ((eq? (car x) 'dict)
                  (let loop ((x (cdr x)))
                    (cond ((null? x)
                            body)
                          ((null? (cdr x))
                            (error "invalid pattern:" (car x)))
                          (else
                            (pattern-match1 (cadr x)
                              ;; TODO: replace hash-ref with get
                              (list hash-ref u (car x))
                              (loop (cddr x)))))))
                ((eq? (car x) 'quote)
                  (pattern-match-is (list _quote (cadr x)) u body)) ; TODO: a bit clunky
                (else (error "invalid pattern:" x))))
        (else
          (pattern-match-is x u body))))
*/
  // (&eval '{1 2 3})
  // (&eval (let u (sym "foo") u))

  // (&eval '{1 2 {3 4 5}})
  // (&eval '{1 ,@2 ,@{3 4 5}})
  // (&eval '(1 2 '(3 {4} 5)))

  // '(1 2 ,@{3 4} 5)
  // {1 2 @{3 4} 5}
  // [1, 2].concat([3, 4], [5])

  // '({1 2} ,@{3 4} @5)
  // {{list 1 2} @{3 4} {&splice 5}}
  // [["list", 1, 2]].concat([3, 4], [["@", 5]])

  n.include = function () {
    [].forEach.call(arguments, function (x) {
      setSymToBox(new n.Symbol(x)) // TODO is this correct?
      //setNewBox(new n.Symbol(x))
    })
  }

  n.builtin = function (o) {
    Object.keys(o).forEach(function (x) {
      setValue(x, o[x])
    })
    /*[].forEach.call(arguments, function (x) {
      x = new n.Symbol(x)
      setBox(x)
      //setNewBox(new n.Symbol(x))
    })*/
    /*var args = arguments
    // TODO: get it to allow for the variable at both compile at runtime
    withMode("compile", function () {
      //withLocalScope(function () {
      n.include.apply(null, args)
      //})
    })*/
  }

  // TODO: make it possible to have a variable at both runtime and compiletime
  //n.include("console", "Array")
  n.builtin({ "console": console, "Array": Array })

  n.import = function () {
    [].forEach.call(arguments, function (s) {
      n.parse(n.readFile(s), function (x) {
        n.compile(x)
      })
    })
  }

  n.toJSON = function anon(a) {
    if (Array.isArray(a)) {
      return a.map(anon)
    /*} else if (a instanceof n.Box) {
      return unmangle(a.value)*/
    } else if (a instanceof n.Symbol || a instanceof n.Box) {
      return "" + a
    } else if (a instanceof n.Wrapper) {
      return anon(a.value)
    } else if (typeof a === "string") {
      return "\"" + a + "\"" // TODO replace " inside the string with \"
    } else {
      return a
    }
  }

  n.compile = function (a) {
    return withMode("run", function () {
      // TODO: change NINO to accept a single argument
      return NINO.compile(NINO.transform([mac(a)]))
    })
  }

  return n
})(NULAN || {})
