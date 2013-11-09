define(["./data", "./scope", "./parse", "./box"], function (data, scope, parse, box) {
  "use strict";

  var scope = "global"
    , mode  = "compile"

  function tap(x) {
    console.log(x)
    return x
  }

  function isFirst(x, y) {
    return Array.isArray(x) && isBox(x[0], y)
  }

  function compileOnlyError(x) {
    if (mode !== "compile") {
      throw new parse.Error(x, "cannot use " + n.print(x) + " at " + mode + " time")
    }
  }

  function isString(x) {
    if (typeof x === "string") {
      return x
    } else if (x instanceof n.String) {
      return x.value
    } else {
      return null
    }
  }

  function isNumber(x) {
    if (typeof x === "number") {
      return x
    } else if (x instanceof n.Number) {
      return x.value
    } else {
      return null
    }
  }

  // TODO should maybe be in nino/compile.js
  function one(x, y) {
    if (x === 1) {
      return x + " " + y
    } else {
      return x + " " + y + "s"
    }
  }

  function formatMode(a) {
    switch (a.length) {
    case 0:
      return "<no>"
    case 1:
      return a[0]
    case 2:
      return a[0] + " and " + a[1]
    default:
      return a.slice(0, -1).join(", ") + ", and " + a[a.length - 1]
    }
  }

  function checkBox(x) {
    if (!x.mode[mode]) {
      var s = formatMode(Object.keys(x.mode))
      throw new n.Error(x, "undefined variable (but it exists at " + s + " time)")
    }
  }

  function checkAssign(x) {
    // TODO: use isSym ?
    if (!isBox(x, "=")) {
      throw new n.Error(x, "expected = but got " + n.print(x))
    }
  }

  function boxOrSym(x) {
    return x instanceof n.Symbol || x instanceof n.Box
  }

  function withLocalScope(f) {
    var old  = scope
      , old2 = boxes
    scope = "local"
    boxes = Object.create(boxes)
    try {
      return f()
    } finally {
      scope = old
      boxes = old2
    }
  }

  function withNewScope(f) {
    var old = vars
    vars = Object.create(vars)
    try {
      return f()
    } finally {
      vars = old
    }
  }

  function withBlockScope(f) {
    return withNewScope(function () {
      return (scope === "global"
               ? withLocalScope(function () {
                   return NINO.op("call", NINO.op("function", NINO.op(","),
                                            NINO.op("return", f())))
                 })
               : f())
    })
  }

  function withMode(x, f) {
    var old = mode
    mode = x
    try {
      return f()
    } finally {
      mode = old
    }
  }

  function symBypass(s) {
    return new n.Bypass(NINO.op("variable", s))
  }

  function compileValue(x) {
    if (x.unique == null) {
      throw new n.Error(x, "cannot use variable as a global at compile time")
    } else {
      return n.enrich(NINO.op(".", NINO.op("call", NINO.op("variable", "box"),
                                                   NINO.op("string", x.unique)),
                                   NINO.op("string", "fn")),
                      x)
    }
  }

  function setBox(s) {
    var x           = new n.Box(s)
    x.unique        = NINO.getUnique(s, boxes)
    boxes[x.unique] = x
    vars[s]         = x.unique
    return x
  }

  function setExternal(x, y) {
    x = setBox(x)
    x.external = y
    return x
  }

  function symToBox(x) {
    if (x instanceof n.Box) {
      return x
    } else if (x instanceof n.Symbol) {
      return n.enrich(setBox(x.value), x)
    } else {
      throw new n.Error(x, "expected variable but got " + n.print(x))
    }
  }

  // TODO make it work with boxes too
  function symToExternal(x, y) {
    if (boxOrSym(x)) {
      if (boxOrSym(y)) {
        return n.enrich(setExternal(x.value, y.value), x)
      } else {
        throw new n.Error(y, "expected variable but got " + n.print(y))
      }
    } else {
      throw new n.Error(x, "expected variable but got " + n.print(x))
    }
  }

  function set(x, y) {
    x = setBox(x)
    Object.keys(y).forEach(function (s) {
      x[s] = y[s]
    })
    return x
  }

  function macro(s, f) {
    return set(s, { builtin: f })
  }

  function func(s, f) {
    return set(s, { fn: f })
  }

  function op(s, s2) {
    return set(s, {
      builtin: function (a) {
        /*if (len !== null && (a.length - 1) !== len) {
          throw new n.Error(a[0], s + " expected " +
                            one(len, "argument") + " but got " +
                            (a.length - 1))
        }*/
        return n.enrich(NINO.opArray(s2, a.slice(1).map(compile)), a[0])
      }
    })
  }

  function functionCall(a) {
    var r = [compile(a[0])]
    a.slice(1).forEach(function (x) {
      if (isFirst(x, "@")) {
        r.push(NINO.op("...", compile(x[1])))
      } else {
        r.push(compile(x))
      }
    })
    return n.enrich(NINO.opArray("call", r), a[0])
  }

  function patternMatch1(k, v) {
    if (boxOrSym(k)) {
      return [box("vars"), [box("="), k, v]]
    } else if (Array.isArray(k)) {
      var f = getBox(k[0])
      if (f.pattern != null) {
        return f.pattern([k[0], k.slice(1), v])
      /*} else if ((f = n.isMacro(f))) {
        return patternMatch1(f(k), v)*/
      } else {
        throw new n.Error(k[0], "is not a pattern")
      }
    } else {
      return [box("if"),
               [box("~="), v, k],
               [box("throw"),
                 [box("new"), symBypass("Error"),
                   [box("+"), "expected " + n.print(k) + " but got ", v]]]]
    }
  }

  function patternMatch(k, v) {
    if (isComplex(k) && isComplex(v)) {
      var u = new n.Box()
      return [box("|"), [box("vars"), [box("="), u, v]],
                        patternMatch1(k, u)]
    } else {
      return patternMatch1(k, v)
    }
  }

  // TODO pretty ew, but it does work...
  function eachVars(f, x, after) {
    // TODO this should work even if the pattern returns a macro that expands to | or vars
    if (isFirst(x, "|")) {
      x.slice(1).forEach(function (x) {
        eachVars(f, x, after)
      })
    } else if (isFirst(x, "vars")) {
      x.slice(1).forEach(f)
    } else {
      x = compile(x)
      after.push(function () {
        return x
      })
    }
  }

  function compile(x) {
    var f
    if (x instanceof n.Bypass) {
      return x.value
    } else if (Array.isArray(x)) {
      if (x.length === 0) {
        return NINO.op("empty")
      } else if ((f = n.isMacro(x[0]))) {
        return f(x)
      } else {
        return functionCall(x)
      }
    } else if ((f = isNumber(x)) !== null) {
      return NINO.op("number", f)
    } else if ((f = isString(x)) !== null) {
      return NINO.op("string", f)
    } else if (x instanceof n.Symbol) {
      return compile(getBox(x))
    } else if (x instanceof n.Box) {
      if (x.get != null) {
        return compile(x.get([x]))
      } else {
        checkBox(x)
        if (x.external != null) {
          return n.enrich(NINO.op("variable", x.external), x)
        } else if (mode === "run" || x.scope === "local") {
          //x._38_type = a._38_type
          return x.toNino
        } else if (mode === "compile") {
          return compileValue(x)
        } else {
          throw new n.Error(x, "invalid mode: " + mode)
        }
      }
    } else {
      throw new n.Error(x, "invalid expression: " + n.print(x))
    }
  }

  "#"

  func("&box==", function (x, y) {
    return isBox(x, y)
  })

  set("'", {
    pattern: function (a) {
      compileOnlyError(a[0])

      var x = getBox(a[1][0]) // TODO: checkBox ?
        , v = a[2]
      return [box("if"), [box("~"), [box("&box=="), v, x.unique]],
                                                           // TODO
               [box("&error"), v, [box("+"), "expected " + n.print(x) + " but got ", v]]]
    },
    macro: function (a) {
      var s = a[0]
        , i = 1

      // TODO: move these both outside of ' ?
      function findCommas(x) {
        var i2 = 0
          , y  = x
        while (isFirst(y, ",")) {
          y = y[1]
          ++i2
        }
        if (i2 !== i) {
          y = loop(y)
          var b = loop(x[0])
          while (i2 > 0) {
            y = [box("["), b, y]
            --i2
          }
        }
        return y
      }

      function withQuote(x, f) {
        if (isBox(x[0], "'")) {
          ++i
          try {
            return f()
          } finally {
            --i
          }
        } else {
          return f()
        }
      }

      function loop(x) {
        if (Array.isArray(x)) {
          if (isBox(x[0], ",")) {
            return findCommas(x)
          } else {
            return withQuote(x, function () {
              return [box("[")].concat(x.map(loop))
            })
          }
        } else if (boxOrSym(x)) {
          x = getBox(x)
                                     // TODO is this correct? test this
          if (x.scope === "local" || x.unique == null) {
            return x
          } else {
            return [symBypass("box"), x.unique]
          }
        } else {
          return x
        }
      }

      function checkSplicing(x) {
        if (isFirst(x, ",") && isFirst(x[1], "@")) {
          throw new n.Error(n.enrich({}, s, x[1][0]), "',@ is invalid")
        }
      }

      compileOnlyError(s)
      checkSplicing(a[1])

      return loop(a[1])
    }
  })

  set(",", {})
  set("@", {})
  set("=", {})

  macro("if", function (a) {
    return withBlockScope(function () {
      var r = a.slice(1).map(function (x, i) {
        if (i === 0) {
          return compile(x)
        } else {
          return withNewScope(function () {
            return compile(x)
          })
        }
      })
      return n.enrich(NINO.opArray("if", r), a[0])
    })
  })

  set("_", {
    pattern: function (a) {
      return a[2]
    }
  })

  function patternMatch(after, x, k, v) {
    if (boxOrSym(k)) {
      v = compile(v)
      after.push(function () {
        k = symToBox(k)
        if (k.scope !== "local" && mode === "compile") {
          return n.enrich(nino.op("=", compileValue(k), v), x)
        } else {
          return n.enrich(nino.op("var", nino.op("=", compile(k), v)), x)
        }
      })
    } else if (Array.isArray(k)) {
      var u
      if (isFirst(k, "[")) {
        v = compile(v)
        after.push(function () {
          k.slice(1).forEach(function (y) {
            if (isFirst(y, "@")) {
              patternMatch(after, x, y[1])
            } else {

            }
          })
          return n.enrich(nino.op("var", nino.op("array", )), x)
        })
      } else if (isFirst(k, "{")) {

      } else if (isFirst(k, ".")) {
        v = compile(v)
        after.push(function () {
          return n.enrich(nino.op("=", compile(k), v), x)
        })
      } else if (isFirst(k, "=")) {

      } else {
        throw new n.Error(k, "invalid pattern")
      }
    } else {
      throw new n.Error(k, "invalid pattern")
    }
  }

  macro("vars", function (a) {
    var after = []
    a.slice(1).forEach(function anon(x) {
      if (Array.isArray(x)) {
        checkAssign(x[0])
        patternMatch(after, x[0], x[1], x[2])
      } else {
        after.push(function () {
          var k = symToBox(x)
          if (k.scope !== "local" && mode === "compile") {
            return n.enrich(NINO.op("empty"), x)
          } else {
            return n.enrich(NINO.op("var", compile(k)), x)
          }
        })
      }
    })
    return n.enrich(NINO.opArray(",", after.map(function (f) { return f() })), a[0])
  })

  set("[", {
    builtin: function (a) {
      var r = []
      a.slice(1).forEach(function (x) {
        if (isFirst(x, "@")) {
          r.push(NINO.op("...", compile(x[1])))
        } else {
          r.push(compile(x))
        }
      })
      return n.enrich(NINO.opArray("array", r), a[0])
    }
  })

  macro("{", function (a) {
    var r = []
    a.slice(1).forEach(function (x) {
      r.push(n.op("=", compile(x[0]), compile(x[1])))
    })
    return n.enrich(NINO.opArray("object", r), a[0])
  })

  macro("->", function (a) {
    return withLocalScope(function () {
      return withNewScope(function () {
        var args = a[1]
          , body = []

        setExternal("this", "this")

        var x, u, r = []

        for (var i = 0, iLen = args.length - 1; i <= iLen; ++i) {
          x = args[i]
          if (boxOrSym(x)) {
            r.push(compile(symToBox(x)))
          } else if (isFirst(x, "@")) {
            r.push(n.op("...", x[1]))

            var y = symBypass("arguments")

            if (i !== iLen) {
              u = new n.Box()
              body.push(patternMatch(u, y))
            } else {
              u = y
            }

            body.push(patternMatch(x[1], slicer(u, i, args.length)))

            ;(function (i2) {
              var x
              while (i2 > i) {
                x = args[i2]
                body.push(patternMatch(x, lengther(u, args, i2)))
                --i2
              }
            })(iLen)
            break
          } else {
            u = new n.Box()
            r.push(compile(u))
            body.push(patternMatch(x, u))
          }
        }

        body.push(a[2])

        body = [box("|")].concat(body)
        body = compile(body)

        return n.enrich(NINO.op("function", NINO.opArray(",", r),
                                  NINO.op("return", body)),
                        a[0])
      })
    })
  })


  op("~", "!")
  op("break", "break")
  op("throw", "throw")
  op("return", "return")
  op("num", "+")

  op("mod", "%")
  op("while", "while")

  op("++", "++")
  op("--", "--")

  op("==", "===")
  op("~=", "!==")
  op("<",  "<")
  op("=<", "<=")
  op(">",  ">")
  op(">=", ">=")
  op("&&", "&&")
  op("||", "||")
  op("+", "+")
  op("*", "*")
  op("-", "-")
  op("/", "/")
  op("|", ",")
  op("new", "new")

  macro(".", function (a) {
    var x = a[1]
      , y = a[2]
      , f

    if ((f = isString(y)) !== null) {
      if (f[0] === "&") {
        y = n.enrich(new n.String(f.slice(1)), y)
      } else {
        throw new n.Error(y, "property " + f + " does not exist")
      }
    }

    return n.enrich(NINO.op(".", compile(x), compile(y)), a[0])
  })

  macro("<=", function (a) {
    var x = a[1]
      , y = a[2]
    if (boxOrSym(x)) {
      x = getBox(x)
      if (x.set != null) {
        return compile(x.set([x, y]))
      }
    }
    return n.enrich(NINO.op("=", compile(x), compile(y)), a[0])
  })

  var syntaxRulesGet = [box("."), symBypass("n"), "&syntaxRules"]
  //NINO.op(".", NINO.op("variable", "n"), NINO.op("string", "syntaxRules"))

  // TODO: why does this need to use compileOnlyError?
  set("syntax-rules", {
    get: function (a) {
      compileOnlyError(a[0])
      return syntaxRulesGet
      //return n.enrich(NINO.op("wrapper", syntaxRuleGet), a[0])
    },
    set: function (a) {
      compileOnlyError(a[0])
      return [box("<="), syntaxRulesGet, a[1]]
      //return n.enrich(NINO.op("=", syntaxRuleGet, compile(a[1])), a[0])
    }
  })

  func("uniq", function (x) {
    return new n.Box()
  })

  set("sym", {
    fn: function (x) {
      return new n.Symbol(x)
    },
    pattern: function (a) {
      var args = a[1]
        , v    = a[2]

      compileOnlyError(a[0])

      return [box("if"), [box("~"), [box("sym=="), v, args]],
               [box("&error"), v, [box("+"), "expected " + args + "but got ", v]]]
    }
  })

  func("&error", function (x, y) {
    throw new n.Error(x, y)
  })

  func("&compile", function (x) {
    return compile(x)
  })

  func("sym==", function (x, y) {
    return x instanceof n.Symbol && x.value === y
  })

  macro("$eval", function (a) {
    var x = a[1]
    if (mode === "compile") {
      return compile(x)
    } else {
      return withMode("compile", function () {
        x = compile(x)
        x = NINO.traverse(x, n.globals)
        x = NINO.compile(x, n.globals, "expression")
        if (n.options.debug) {
          n.options.debug(x)
        }
        return n.enrich(NINO.op("bypass", eval(x)), a[0])
      })
    }
  })

  // TODO should handle things like literals, etc.
  macro("&", function (a) {
    var x    = a[1]
      , args = [].slice.call(arguments, 1).map(compile)
      , f
    if (boxOrSym(x)) {
      f = x.value
    } else if ((f = isString(x)) === null) {
      throw new n.Error(x, "expected variable or string but got " + n.print(x))
    }
    return n.enrich(NINO.opArray(f, args), a[0])
  })

  macro("w/new-scope", function (a) {
    var x = a[1]
    return withBlockScope(function () {
      return compile(x)
    })
  })

  macro("\"", function (a) {
    var r = a.slice(1).map(compile)
    r.unshift(NINO.op("string", ""))
    return n.enrich(NINO.opArray("+", r), a[0])
  })

  macro("null?", function (a) {
    var x = a[1]
    return n.enrich(NINO.op("==", compile(x), NINO.op("null")), a[0])
  })

  macro("finally", function (a) {
    var x = a[1]
      , y = a[2]
    return n.enrich(NINO.op("try", compile(x), NINO.op("finally", compile(y))), a[0])
  })

  macro("del", function (a) {
    var x = a[1]
      , y = compile(x)
    if (x instanceof n.Box) {
      if (vars[x.value] === x.unique) {
        delete vars[x.value]
      } else {
        throw new n.Error(x, "undefined variable")
      }
      return y
    } else if (x instanceof n.Symbol) {
      delete vars[x.value]
      return y
    } else {
      var u = NINO.op("unique")
      return n.enrich(NINO.op(",", NINO.op("var", NINO.op("=", u, y)),
                                   NINO.op("delete", y),
                                   u),
                      a[0])
    }
  })

  macro("external", function (a) {
    a.slice(1).forEach(function (x) {
      if (Array.isArray(x)) {
        checkAssign(x[0])
        symToExternal(x[1], x[2])
      } else {
        symToExternal(x, x)
      }
    })
    return n.enrich(NINO.op("empty"), a[0])
  })

  macro("&builtin", function (a) {
    a.slice(1).forEach(function (x) {
      if (Array.isArray(x)) {
        checkAssign(x[0])
        x = symToExternal(x[1], x[2])
      } else {
        var y = x
        if (boxOrSym(x)) {
          x = n.enrich(new n.Symbol("&" + x.value), x)
        }
        x = symToExternal(x, y)
      }
      x.mode["run"]     = true
      x.mode["compile"] = true
    })
    return n.enrich(NINO.op("empty"), a[0])
  })


  n.globals = {}
  n.options = {}

  n.Box = function (s) {
    this.value      = s
    this.scope      = scope
    this.mode       = {}
    this.mode[mode] = true
    this.toNino     = NINO.op("unique", s)
  }

  n.isMacro = function (x) {
    if (x instanceof n.Symbol) {
      x = getBox(x)
    }
    if (x instanceof n.Box && x.macro != null) {
      return function (a) {
        return compile(x.macro(a))
      }
    } else if (x instanceof n.Box && x.builtin != null) {
      return x.builtin
    } else {
      return false
    }
  }

  n.compile = function (x) {
    return withMode("run", function () {
      return compile(x)
    })
  }

  n.eval = function (s) {
    try {
      var o = n.tokenize(s)
        , r = []
      while (o.has()) {
        r.push(NINO.traverse(n.compile(n.parse(o)), n.globals))
      }
      r = r.map(function (x) {
        console.log(x)
        return tap(NINO.compile(x, n.globals, "statement"))
      })
    } catch (e) {
      console.error(e.message)
    }
  }

  return n
})
