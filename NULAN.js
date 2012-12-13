var NULAN = (function (nTop) {
  "use strict";

  Object.prototype.tap = function () {
    console.log(require("util").inspect(nTop.toJSON(this), false, null, false))
    return this
  }

  var n = {}

  var local // TODO


  ;(function () {
    // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
    var reserved = ("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with" +
                    " class enum export extends import super" +
                    " implements interface let package private protected public static yield" +
                    " arguments null true false undefined").split(" ").join("|")

    var to = new RegExp("^(" + reserved + ")$|(^[0-9])|([a-z])\-([a-z])|[^$a-z0-9]", "g")

    // mangle("50fooBar-qux")

    n.mangle = function (s) {
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

    var from = new RegExp("_([0-9]*)_|^_(" + reserved + "|[0-9])|([a-z])([A-Z])", "g")

    // Not actually used, but still nice to have
    n.unmangle = function (s) {
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
  })()


  ;(function () {
    var js_vars = {} // JS variables -> true (if variable exists)

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
      this.name = this.value = n.mangle(s)
    }
    n.Box.prototype.uniqize = function () {
      //this.local = local
      //this.mode  = mode
      this.value = findUniq(this.name)
    }

    n.Uniq = function () {}
    n.Uniq.prototype = new n.Box("")
    n.Uniq.prototype.uniqize = function () {
      // TODO
      this.local = local
      //this.mode  = mode
      this.value = getUniq()
    }

    n.withLocalScope = function (f) {
      var old  = js_vars
        , old2 = local
      js_vars = Object.create(js_vars)
      local   = true
      try {
        var x = f()
      } finally {
        js_vars = old
        local   = old2
      }
      return x
    }
  })()


  ;(function () {
    n.Wrapper = function (value) {
      this.value = value
    }
    n.Symbol = function (value) {
      this.value = value
    }

    n.Box.prototype.toString =
    n.Wrapper.prototype.toString =
    n.Symbol.prototype.toString = function () {
      return this.value
    }
  })()


  function box(s) {
    return new n.Box(s)
    /*var x = n.vars[s]
    if (x) {
      return x
    } else {
      // TODO
      throw new Error("!!!!!!!Undefined variable: " + s)
    }*/
  }

  function keyword(x) {
    var y = new n.Box("")
    y.value = x
    return y
  }


  ;(function () {
    n.vars = {} // Nulan variables -> Boxes

    n.vars["%t"] = keyword("true")
    n.vars["%f"] = keyword("false")

    n.getBox = function (x, b) {
      if (x instanceof n.Box) {
        return x
      } else if (x instanceof n.Symbol) {
        var y = n.vars[x.value]
        // b && y.mode !== mode
        if (!y || b && (mode === "run" && values[y.value]) ||
                  b && (mode === "compile" && !values[y.value])) {
          if (!y) {
            throw new nTop.Error(x, "undefined variable: " + x)
          } else {
            throw new nTop.Error(x, "undefined variable (but it exists at " +
                                      (mode === "compile" ? "run" : "compile") +
                                      " time): " + x)
          }
        } else {
          return y
        }
      }
    }

    n.setBox = function (x) {
      var y = new n.Box(x.value)
      y.local = local
      //y.mode  = mode
      n.vars[x.value] = y
      return y
    }

    n.withNewScope = function (f) {
      var old = n.vars
      n.vars = Object.create(n.vars)
      try {
        var x = f()
      } finally {
        n.vars = old
      }
      return x
    }


    var values = {} // Boxes -> Compile-time values

    n.isMacro = function (x) {
      if ((x = n.getBox(x)) && typeof (x = values[x.value]) === "function") {
        return x
      }
    }

    n.setBoxValue = function (x, y) {
      //n.vars[x.value] = x
      values[x.value] = y
    }

    n.setValue = function (x, y) {
      var b = new n.Box(x)
      //b.mode = "compile" // TODO: a teensy bit hacky
      b.uniqize() // TODO: does this need to uniqize?
      n.vars[x] = b
      values[b.value] = y
    }

    // This needs to be in here because `eval` needs access to `values`
    n.compileEval = function (x) {
      return withMode("compile", function () {
        x = mac(x)
        x = NINO.compile(NINO.transform([x]))
        console.log(x)
        return ["id", eval(x)]
      })
    }

    // TODO: make it work at compile-time
    // var foo = 5
    // $eval
    //   var foo = 10
    // $eval
    //   del foo
    // foo
    n.setValue("del", function (x) {
      // TODO: make it work with boxes too?
      if (x instanceof n.Symbol) {
        var s = mac(x)
        delete n.vars[x.value]
        return s
      } else {
        // TODO: return the value that's being deleted
        return ["delete", mac(x)]
      }
    })
  })()


  var mode = "run" // Whether code is evaluated at run-time or compile-time

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


  function mac(a) {
    var x
    if (Array.isArray(a)) {
      if ((x = n.isMacro(a[0]))) {
        return x.apply(null, a.slice(1))
      } else if (a.length === 0) { // TODO: this should probably be in splicingArgs
        return ["void", ["number", "0"]]
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
      return mac(n.getBox(a, true))
    } else if (a instanceof n.Box) {
      x = a.value

      if (a.local || mode === "run") {
        return ["name", x]
      } else if (mode === "compile") {
        // TODO: should this unmangle?
        return ["[]", ["name", "values"], ["string", x]]
      } else if (mode === "quote") {
        // TODO: should this unmangle?
        return ["call", ["name", "box"], [mac(x)]]
        //return ["call", [".", ["name", "n"], "getBox"], [mac(x)]]
        //return ["new", [".", ["name", "n"], "Box"], [mac(x)]]
        //return mac([box("&box"), x])
      } else {
        throw new nTop.Error(a, "invalid mode: " + mode) // TODO
      }
    } else {
      throw new nTop.Error(a, "invalid expression: " + a)
    }
  }

  // TODO
  function $mac(a) {
    var x = a[0]
    // TODO: make it work with boxes too
    if (x instanceof n.Symbol) {
      x = ["name", x.value]
    } else if (typeof x !== "string") {
      throw new nTop.Error(x, "invalid expression: " + x)
    }
    return (a.length === 1
             ? x
             : [x].concat([].slice.call(a, 1).map(mac)))
  }


  function boxOrSym(x) {
    return x instanceof n.Box || x instanceof n.Symbol
  }

  /*function isSym(x, s) {
    if (x instanceof n.Box) {
      return x.name === s
    } else {
      return false
    }
  }*/

  function isBox(x, s) {
    x = n.getBox(x)
    return x instanceof n.Box && x.value === s
  }

  function isBoxM(x, s) {
    return isBox(x, n.mangle(s))
  }

  function complex(x) {
    return Array.isArray(x)
  }


  function splicingArgsRest(x, i, iLen, a) {
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
    return function () {
      switch (arguments.length) {
      case 0:
        return i
      case 1:
        return mac(arguments[0])
      default:
        return binr(s, [].map.call(arguments, mac))
      }
    }
  }

  function binreduce1(s, i) {
    return function () {
      switch (arguments.length) {
      case 0:
        throw new nTop.Error({}, (typeof i === "string" ? i : s) +
                                 " cannot be called with 0 arguments")
      case 1:
        if (typeof i === "function") {
          return i(mac(arguments[0]))
        } else {
          return mac(arguments[0])
        }
      default:
        return binr(s, [].map.call(arguments, mac))
      }
    }
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
    return function () {
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
    }
  }

  function unary(s) {
    return function (x) {
      return [s, mac(x)]
    }
  }

  function binary(s) {
    return function (x, y) {
      return [s, mac(x), mac(y)]
    }
  }

  function validJS(x) {
    if (typeof x === "string") {
      if (/^[$_a-zA-Z](?:[a-z]\-[a-z]|[$_a-zA-Z0-9])*$/.test(x)) {
        return n.mangle(x) // TODO mangle
      }
    }
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
    var r = [[box("."),
              [box("."), [box("list")], "slice"],
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

  var patterns = {
    "list": function (args, v, body) {
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
          return destructure1(y, [box("."), v,
                                  [box("-"),
                                   [box("."), v, "length"],
                                   a.length - i]],
                                 x)
        } else {
          return destructure1(y, [box("."), v, i], x)
        }
      }, body)
    },

    "dict": function (args, v, body) {
      return pair(args).reduceRight(function (x, y) {
        return destructure1(y[1], [box("."), v, y[0]], x)
      }, body)
    },

    "=": function (args, v, body) {
      return destructure1(args[0], [box("if"), [box("null?"), v], args[1], v], body)
    },

    "'": function (args, v, body) {
      var x = n.getBox(args[0])
      return [box("if"), [box("&box=="), v, x.value],
                         body,
                         [box("&error"), v, [box("+"), "expected " + args[0] + " but got ", v]]]
    }
  }

  function destructure1(args, v, body) {
    if (complex(args)) {
      var u = new n.Uniq()
      //u.uniqize()
      return [box("|"), [box("var"), [box("="), u, v]], destructure(args, u, body)]
    } else {
      return destructure(args, v, body)
    }
  }

  function destructure(args, v, body) {
    var a, f
    if (boxOrSym(args)) {
      a = [box("var"), [box("="), args, v]]
              // TODO
      return (body
               ? [box("|"), a, body]
               : a)
    } else if (Array.isArray(args)) {
                                 // TODO: test this
      if ((f = n.getBox(args[0], true)) && (f = patterns[f.value])) {
        return f(args.slice(1), v, body)
      } else {
        throw new nTop.Error(args[0], "not a pattern")
      }
    } else {
      return [box("if"),
               [box("=="), v, args],
               body,
               [box("error"), [box("+"), "expected " + args + " but got ", v]]]
    }
  }


  // Simple complexity
  n.setValue("num",         unary("u+"))
  n.setValue("~",           unary("!"))
  n.setValue("&typeof",     unary("typeof"))

  n.setValue("&in",         binary("in"))
  n.setValue("&instanceof", binary("instanceof"))
  n.setValue("mod",         binary("%"))

  n.setValue("==",          binand("===", ["boolean", "true"]))
  n.setValue("<",           binand("<",   ["boolean", "true"]))
  n.setValue("=<",          binand("<=",  ["boolean", "true"]))
  n.setValue(">",           binand(">",   ["boolean", "true"]))
  n.setValue(">=",          binand(">=",  ["boolean", "true"]))

  n.setValue("&&",          binreduce0("&&", ["boolean", "true"]))
  n.setValue("||",          binreduce0("||", ["boolean", "false"]))
  n.setValue("+",           binreduce0("+",  ["number", "0"]))
  n.setValue("*",           binreduce0("*",  ["number", "1"]))

  n.setValue("-",           binreduce1("-", function (x) { return ["u-", x] }))
  n.setValue("|",           binreduce1(",", "|"))
  n.setValue("/",           binreduce1("/"))

  n.setValue("&break", function () {
    return ["break"]
  })

  n.setValue("$eval", function (x) {
    return n.compileEval(x)
  })

  n.setValue("&", function () {
    return $mac(arguments)
  })

  n.setValue("<=", function (x, y) {
    return ["=", mac(x), mac(y)]
  })

  n.setValue("list", function () {
    return ["array", [].map.call(arguments, mac)]
  })

  n.setValue(".", function (x, y) {
    var s = validJS(y)
    if (s) {
      return [".", mac(x), s]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  })

  n.setValue(",", function (x) {
    return mac(x)
  })

  n.setValue("@", function (x) {
    // TODO
    throw new nTop.Error(x, "invalid use of @")
  })

  n.setValue("null?", function (x) {
    return ["==", mac(x), ["null"]]
  })

  // TODO: move into NULAN.macros somehow
  n.setValue("=", function (x, y) {
    return mac([box("if"), [box("null?"), x], [box("<="), x, y]])
  })

  n.setValue("++", function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["++", mac(x)]
    } else {
      return ["+=", mac(x), mac(y)]
    }
  })

  n.setValue("--", function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["--", mac(x)]
    } else {
      return ["-=", mac(x), mac(y)]
    }
  })

  n.setValue("new", function (x) {
    var args = [].slice.call(arguments, 1)
    return ["new", mac(x), args.map(mac)]
  })

  n.setValue("while", function (test, body) {
    return ["while", mac(test), [mac(body)]]
  })

  // TODO: move this into NULAN.macros
  n.setValue("w/var", function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    return n.withNewScope(function () {
      return mac([box("|"), [box("var")].concat(args), body])
    })
  })

  n.setValue("w/new-scope", function (body) {
    return n.withNewScope(function () {
      return mac(body)
    })
  })

  n.setValue("if", function anon() {
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
               [n.withNewScope(function () { return mac(a[1]) })],
               [n.withNewScope(function () { return mac(a[2]) })]]
    // TODO: maybe simplify this a bit?
    default:
      return ["if", mac(a[0]), [mac(a[1])],
                    [anon.apply(this, [].slice.call(a, 2))]]
    }
  })

  n.setValue("error", function (x) {
    return ["throw", ["new", ["name", "Error"], [mac(x)]]]
  })

  n.setValue("finally", function (x, y) {
    return ["try", [mac(x)], ["finally", [mac(y)]]]
    /*var u = new Uniq()
    return ["try", [mac([values["var"], [u, x]])], ["finally", [mac(y), mac(u)]]]*/
  })


  // Medium complexity
  n.setValue("dict", function () {
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
      u = new n.Uniq()
      //u.uniqize()
      a = args.map(function (a) {
        var x = a[0]
          , y = a[1]
        // TODO: make it work with boxes too?
        if (x instanceof n.Symbol) {
          x = x.value
        }
        return [box("<="), [box("."), u, x], y]
      })
      return mac([box("|"), [box("var"), [box("="), u, [box("dict")]]]].concat(a))
    }
  })

  n.setValue("'", function loop(x) {
    // TODO: a little bit hacky, but it'll do for now
    if (Array.isArray(x)) {
      var s = x[0]
      if (isBox(x[0], n.mangle(",")) && (x = x[1])) {
        if (Array.isArray(x) && isBoxM(x[0], "@")) {
                            // TODO: use store somehow?
          throw new nTop.Error({ text:   s.text
                               , column: s.column - 1
                               , line:   s.line
                               , length: 3 }, "',@ is invalid")
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
    } else if (boxOrSym(x)) {
      //x = n.getBox(x)
      //console.log(x)
      //console.log("quote", n.getBox(x))
      //return n.getBox(x)
      return withMode("quote", function () {
        return mac(x)
      })
    } else {
      return mac(x)
    }
  })

  n.setValue("&box==", function (x, y) {
    return ["call", ["name", "isBox"], [mac(x), mac(y)]]
  })

  n.setValue("include", function () {
    [].forEach.call(arguments, function (x) {
      n.setBox(x)
      //n.setNewBox(x)
                       // (&eval '(&list 1 2 3))
                       // (include &list)
                       // (&eval '(&list 1 2 3))
    })
    return ["empty"]
  })

  n.setValue("uniq", function () {
    return ["new", [".", ["name", "n"], "Uniq"], []]
  })

  n.setValue("sym", function (x) {
    return ["new", [".", ["name", "n"], "Symbol"], [mac(x)]]
  })

  n.setValue("&error", function (x, y) {
    return ["throw", ["new", [".", ["name", "nTop"], "Error"], [mac(x), mac(y)]]]
  })

  //pretty(destructure(["dict", new n.Symbol("foo"), new n.Symbol("bar"), new n.Symbol("qux"), 30, new n.Symbol("corge"), ["=", new n.Symbol("uuuuuu"), new n.Symbol("bar")]], new n.Symbol("u"), "END"))

  //pretty(destructure(["list", new n.Symbol("c"), ["@", new n.Symbol("a")], 20, new n.Symbol("b")], new n.Symbol("u"), ["list", new n.Symbol("a"), new n.Symbol("b")]))

  //pretty(destructure(["list", new n.Symbol("a"), 20, ["@", new n.Symbol("HAHA")], new n.Symbol("b"), ["=", new n.Symbol("c"), new n.Symbol("b")]], new n.Symbol("u"), "END"))


  // Heavy complexity
  n.setValue("->", function (args, body) {
    if (Array.isArray(args)) {
      return n.withLocalScope(function () {
        return n.withNewScope(function () {
  /*
          var slice = Function.prototype.call.bind([].slice)
          slice(arguments)
  */
          var x, u, s, r = []

          for (var i = 0, iLen = args.length; i < iLen; ++i) {
            x = args[i]

            // TODO: code duplication with box("var")
            if (boxOrSym(x)) {
              if (x instanceof n.Symbol) {
                x = n.setBox(x)
              }
              x.uniqize()
              r.push(x.value)

            } else if (Array.isArray(x) && isBoxM(x[0], "@")) {
              s = keyword("arguments")
              s.local = true // TODO: ew

              u = (i !== iLen - 1
                    ? new n.Uniq()
                    : s)

              ;(function (i2) {
                var x
                while (i2 > i) {
                  x = args[i2]
                                        // TODO: code duplication with the list pattern
                  body = destructure(x, [box("."), u,
                                          [box("-"),
                                           [box("."), u, "length"],
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
              u = new n.Uniq()
              u.uniqize()
              //n.setVar(u) // TODO: does this need to be added to vars?
              r.push(u.value)
              body = destructure(x, u, body)
            }
          }

          return ["function", "", r, [["return", mac(body)]]]
        })
      })
    } else {
      throw new nTop.Error(args, "invalid function argument list: " + args)
    }
  })

  //n.setValue("true", new n.Symbol("true") // TODO
  n.setValue("var", function () {
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
          throw new nTop.Error(x[0], "expected ('=) but got " + x[0])
        }
        y = x[2]
        x = x[1]
      }
      if (boxOrSym(x)) {
        if (y !== void 0) {
          y = mac(y)
        }

        if (x instanceof n.Symbol) {
          x = n.setBox(x)
        }
        x.uniqize()

        if (!x.local && mode === "compile") {
                                                  // TODO: unmangle this?
          return ["=", ["[]", ["name", "values"], ["string", x.value]], y]
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
          var u = new n.Uniq()
          return mac([box("|"), [box("var"), [box("="), u, y]], destructure(x, u)])
        } else {
          return mac(destructure(x, y))
        }
      }
    }).reduce(function (x, y) {
      return [",", x, y]
    })
  })

  n.setValue("$mac", function (s, v) {
    return withMode("compile", function () {
      // TODO: code duplication with box("var")
      if (s instanceof n.Symbol) {
        s = n.setBox(s)
      }
      s.uniqize()

      v = n.compileEval(v)[1]

      // TODO: unmangle this?
      n.setBoxValue(s, function () {
        return mac(v.apply(this, arguments))
      })

      return ["empty"]
    })
  })


  /*n.setValue("w/var", function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    var x = args.map(function (x) { return x[0] })
      , y = args.map(function (x) { return x[1] })

    return mac([[box("->"), x, body]].concat(y))
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

  /*n.setValue("namespace", new Macro(function (x) {
    namespace = x
    js_vars   = {}
    return ["empty"]
  })*/

  /*function reducer(a, f) {
    return [].map.call(a, mac).reduce(f)
  }*/

  /*n.setValue("w/namespace", new Macro(function (x, body) {
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

  nTop.vars    = n.vars
  nTop.Box     = n.Box
  nTop.Wrapper = n.Wrapper
  nTop.Symbol  = n.Symbol

  nTop.include = function () {
    [].forEach.call(arguments, function (x) {
      n.setBox(new n.Symbol(x))
      //n.setNewBox(new n.Symbol(x))
    })
  }

  nTop.builtin = function (o) {
    Object.keys(o).forEach(function (x) {
      n.setValue(x, o[x])
    })
    /*[].forEach.call(arguments, function (x) {
      x = new n.Symbol(x)
      n.setBox(x)
      //n.setNewBox(new n.Symbol(x))
    })*/
    /*var args = arguments
    // TODO: get it to allow for the variable at both compile at runtime
    withMode("compile", function () {
      //n.withLocalScope(function () {
      nTop.include.apply(null, args)
      //})
    })*/
  }

  // TODO: make it possible to have a variable at both runtime and compiletime
  //nTop.include("console", "Array")
  nTop.builtin({ "console": console, "Array": Array })

  nTop.import = function () {
    [].forEach.call(arguments, function (s) {
      nTop.parse(nTop.readFile(s), function (x) {
        nTop.compile(x)
      })
    })
  }

  nTop.toJSON = function anon(a) {
    if (Array.isArray(a)) {
      return a.map(anon)
    } else if (a instanceof n.Box) {
      return n.unmangle(a.value)
    } else if (a instanceof n.Symbol) {
      return a.value
    } else if (a instanceof n.Wrapper) {
      return anon(a.value)
    } else if (typeof a === "string") {
      return "\"" + a + "\"" // TODO replace " inside the string with \"
    } else {
      return a
    }
  }

  nTop.compile = function (a) {
    // TODO: change NINO to accept a single argument
    return NINO.compile(NINO.transform([mac(a)]))
  }

  return nTop
})(NULAN || {})
