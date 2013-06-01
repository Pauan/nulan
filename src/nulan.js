var NULAN = (function (n) {
  "use strict"

  /**
   * Stable
   */
  function tap(x) {
    console.log(x)
    return x
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
    if (!x["&mode"][mode]) {
      var s = formatMode(Object.keys(x["&mode"]))
      throw new NINO.Error(x, "undefined variable (but it exists at " + s + " time): " + x["&name"])
    }
  }

  function boxOrSym(x) {
    x = NINO.unwrap(x)
    return x.op === "variable" || x.op === "unique"
  }

  function wildcard(w) {
    var x = NINO.unwrap(w)
    return boxOrSym(w) && x.args[0] === "_"
  }

  /**
   * Box stuff
   */
  var boxes = {} // Variable name -> Box
    , vars  = {} // String        -> Variable name
    , scope = "global"
    , mode  = "compile"

  function Box() {
    this["&scope"]      = scope
    this["&mode"]       = {}
    this["&mode"][mode] = true
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

  function box(s) {
    return boxes[s]
  }

  function isBox(w, s) {
    var x = NINO.unwrap(w)
    if (x.op === "variable") {
      w = getBox(w)
      x = NINO.unwrap(w)
    }
    if (x.op === "unique") {
      return x["&box"] === s
    } else {
      throw new NINO.Error(w, "expected variable but got " + NINO.print(w))
    }
  }

  function symBypass(s) {
    return new n.Bypass(NINO.op("variable", s))
  }

  function getBox(w) {
    var x = NINO.unwrap(w)
      , y = vars[x.args[0]]
    // TODO: not sure if this should enrich or not...
    //       it mostly affects macros:
    //
    //         $mac foo ->
    //           'sym "5"
    //         foo;
    if (y == null) {
      throw new NINO.Error(w, "undefined variable: " + x.args[0])
    }
    return NINO.enrich(Object.create(boxes[y]), w)
  }

  function compileValue(x) {
    if (x["&box"] == null) {
      if (x["&name"] == null) {
        throw new NINO.Error(x, "cannot use variable as a global at compile time")
      } else {
        throw new NINO.Error(x, "cannot use variable as a global at compile time: " + x["&name"])
      }
    }
    return NINO.enrich(NINO.op(".", NINO.op(".", NINO.op("variable", "boxes"),
                                                 NINO.op("string", x["&box"])),
                                    NINO.op("string", "&value")), x)
  }

  function makeUniq() {
    var x        = new Box()
    x["&unique"] = NINO.op("unique")
    return x
  }

  function setBox(s) {
    var x             = new Box()
    x["&name"]        = s
    x["&unique"]      = NINO.op("unique", s)
    x["&box"]         = NINO.getUnique(s, boxes) // TODO rename to setUnique
    boxes[x["&box"]]  = x
    vars[s]           = x["&box"]
    return x
  }

  function setExternal(x, y) {
    x = setBox(x)
    x["&external"] = y
    return x
  }

  function symToBox(x) {
    if (boxOrSym(x)) {
      return NINO.enrich(setBox(NINO.unwrap(x).args[0]), x)
    } else {
      throw new NINO.Error(x, "expected variable but got " + NINO.print(x))
    }
  }

  function symToExternal(x, y) {
    if (boxOrSym(x)) {
      if (boxOrSym(y)) {
        return NINO.enrich(setExternal(NINO.unwrap(x).args[0],
                                       NINO.unwrap(y).args[0]),
                           x)
      } else {
        throw new NINO.Error(y, "expected variable but got " + NINO.print(y))
      }
    } else {
      throw new NINO.Error(x, "expected variable but got " + NINO.print(x))
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
    return set(s, { "&macro": f })
  }

  function func(s, f) {
    return set(s, { "&value": f })
  }

  function op(len, s, s2) {
    return set(s, {
      "&macro": function (a) {
        /*if (len !== null && (a.length - 1) !== len) {
          throw new NINO.Error(a[0], s + " expected " +
                               one(len, "argument") + " but got " +
                               (a.length - 1))
        }*/
        return NINO.enrich(NINO.opArray(s2, a.slice(1).map(compile)), a[0])
      }
    })
  }


  function concater(l, r) {
    if (l.length === 0) {
      if (r.length === 1) {
        return r[0]
      } else {
        return NINO.opArray("call", NINO.op(".", r[0], NINO.op("string", "concat")),
                                    r.slice(1))
      }
    } else {
      l = NINO.opArray("array", l)
      if (r.length === 0) {
        return l
      } else {
        return NINO.opArray("call", NINO.op(".", l, NINO.op("string", "concat")), r)
      }
    }
  }

  function functionCall(a) {
    var f  = compile(a[0])
      , r1 = []
      , r2 = []
      , seen
    a.slice(1).forEach(function (w) {
      var x = NINO.unwrap(w)
      if (Array.isArray(x) && isBox(x[0], "@")) {
        seen = true
        r2.push(compile(x[1]))
      } else if (seen) {
        r2.push(NINO.op("array", compile(w)))
      } else {
        r1.push(compile(w))
      }
    })
    if (seen) {
      return NINO.op("call", NINO.op(".", f, NINO.op("string", "apply")),
                             NINO.op("null"),
                             concater(r1, r2))
    } else {
      return NINO.opArray("call", f, r1)
    }
  }

  function patternMatch(k, v, body) {
    var a = NINO.unwrap(k)
    if (wildcard(k)) {
      return body
    } else if (boxOrSym(k)) {
      a = [box("var"), [box("="), k, v]]
      return (body == null
               ? a
               : [box("|"), a, body])
    } else if (Array.isArray(a)) {
      var f = getBox(a[0])
        , g = NINO.unwrap(f)
      //if (f) { TODO
      if (g["&pattern"] != null) {
        return g["&pattern"](a, v, body)
      } else if ((f = n.isMacro(f))) {
        return patternMatch(f(a), v, body)
      } else {
        throw new NINO.Error(a[0], g.args[0] + " does not have a &pattern property")
      }
    } else {
      var err = [box("throw"),
                  [box("new"), symBypass("Error"),
                    [box("+"), "expected " + NINO.print(k) + " but got ", v]]]
      if (body == null) {
        return [box("if"),
                 [box("~="), v, k],
                 err]
      } else {
        return [box("if"),
                 [box("=="), v, k],
                 body,
                 err]
      }
    }
  }

  function slicer(v, i, iLen) {
    var r = [[box("."),
               [box("."), [box("[")], NINO.op("string", "slice")],
                NINO.op("string", "call")],
             v]
    var i2 = i - iLen + 1
    if (i2 !== 0 || i !== 0) {
      r.push(NINO.op("number", i))
    }
    if (i2 !== 0) {
      r.push(NINO.op("number", i2))
    }
    return r
  }

  function compile(w) {
    var x, a = NINO.unwrap(w)
    if (a instanceof n.Bypass) {
      return NINO.enrich(a.value, w)
    } else if (Array.isArray(a)) {
      if (a.length === 0) {
        return NINO.enrich(NINO.op("empty"), w)
      } else if ((x = n.isMacro(a[0]))) {
        return x(a) // TODO wrap ?
      } else {
        return NINO.enrich(functionCall(a), a[0]) // TODO wrap ?
      }
    } else if (a.op === "number" || a.op === "string") {
      return NINO.enrich(a, w)
    } else if (a.op === "variable") {
      return compile(getBox(w))
    } else if (a.op === "unique") {
      if (a["&get"] != null) {
        return NINO.enrich(a["&get"]([a]), w) // TODO
      } else {
        checkBox(w)
        if (a["&external"] != null) {
          return NINO.enrich(NINO.op("variable", a["&external"]), w)
        } else if (mode === "run" || a["&scope"] === "local") {
          //x._38_type = a._38_type
          return NINO.wrap(w, a)
        } else if (mode === "compile") {
          return compileValue(w)
        } else {
          throw new NINO.Error(w, "invalid mode: " + mode)
        }
      }
    } else {
      throw new NINO.Error(w, "invalid expression: " + NINO.print(w))
    }
  }

  "#"
  "="
  "pattern-match"
  "'"

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
      return NINO.enrich(NINO.opArray("if", r), a[0])
    })
  })

  macro("[", function (a) {
    return NINO.enrich(NINO.opArray("array", a.slice(1).map(compile)), a[0])
  })

  macro("{", function (a) {
    var r = []
    a.slice(1).forEach(function (x) {
      r.push(x[0])
      r.push(compile(x[1]))
    })
    return NINO.enrich(NINO.opArray("object", r), a[0])
  })

  macro("var", function (a) {
    var r = a.slice(1).map(function (w) {
      var x = NINO.unwrap(w)
      if (Array.isArray(x) && isBox(x[0], "=")) {
        var k = x[1]
          , v = x[2]
        if (boxOrSym(k) && !wildcard(k)) {
          v = compile(v)
          k = symToBox(k)
          return NINO.op("var", NINO.op("=", k, v))
        } else {
          //var u = makeUniq()
          return patternMatch(k, v)
        }
      } else {
        return NINO.op("var", w)
      }
    })
    return NINO.enrich(NINO.opArray(",", r), a[0])
  })

  macro("->", function (a) {
    var args = a[1]
      , body = a[2]
    return withLocalScope(function () {
      return withNewScope(function () {
        setExternal("this", "this")

        var w, x, u, r = []

        //body = [box("return"), body] // TODO

        for (var i = 0, iLen = args.length - 1; i <= iLen; ++i) {
          w = args[i]
          x = NINO.unwrap(w)
          if (boxOrSym(w) && !wildcard(w)) {
            r.push(symToBox(w))
          } else if (Array.isArray(x) && isBox(x[0], "@")) {
            var y = symBypass("arguments")

            if (i !== iLen) {
              u = makeUniq()
            } else {
              u = y
            }

            ;(function (i2) {
              var x
              while (i2 > i) {
                x = args[i2]
                body = patternMatch(x, [box("."), u,
                                         [box("-"),
                                           [box("."), u, NINO.op("string", "length")],
                                           NINO.op("number", args.length - i2)]],
                                       body)
                --i2
              }
            })(iLen)

            body = patternMatch(x[1], slicer(u, i, args.length), body)

            if (i !== iLen) {
              body = [box("|"), [box("var"), [box("="), u, y]], body]// patternMatch(u, y, body)
            }
            break
          } else {
            u = makeUniq()
            r.push(u)
            body = patternMatch(w, u, body)
          }
        }

        /*var i = args.length
        while (i--) {
          body = patternMatch(args[i], [box("."), symBypass("arguments"), NINO.op("number", i)], body)
        }*/

        return NINO.enrich(NINO.op("function", NINO.opArray(",", r),
                                     NINO.op("return", compile(body))),
                           a[0])
      })
    })
  })


  op(1, "~", "!")
  op(1, "break", "break")
  op(1, "throw", "throw")
  op(1, "return", "return")
  op(1, "num", "+")

  op(2, ".", ".")
  op(2, "mod", "%")
  op(2, "while", "while")

  op(null, "==", "===")
  op(null, "~=", "!==")
  op(null, "<",  "<")
  op(null, "=<", "<=")
  op(null, ">",  ">")
  op(null, ">=", ">=")
  op(null, "&&", "&&")
  op(null, "||", "||")
  op(null, "+", "+")
  op(null, "*", "*")
  op(null, "-", "-")
  op(null, "/", "/")
  op(null, "|", ",")
  op(null, "++", "++") // TODO 1 or 2 args
  op(null, "--", "--") // TODO 1 or 2 args
  op(null, "new", "new")

  macro("<=", function (a) {
    var w = a[1]
      , y = a[2]
      , x = NINO.unwrap(w)
    if (x.op === "variable") {
      w = getBox(w)
      x = NINO.unwrap(w)
    }
    if (x.op === "unique" && x["&set"] != null) {
      return NINO.enrich(x["&set"]([w, y]), w) // TODO
    } else {
      return NINO.enrich(NINO.op("=", compile(w), compile(y)), a[0])
    }
  })

  var syntaxRuleGet = NINO.op(".", NINO.op("variable", "n"), NINO.op("string", "syntax-rules"))

  // TODO: why does this need to use compileOnlyError?
  set("syntax-rules", {
    "&get": function (a) {
      compileOnlyError(a[0])
      return NINO.enrich(NINO.op("wrapper", syntaxRuleGet), a[0])
    },
    "&set": function (a) {
      compileOnlyError(a[0])
      return NINO.enrich(NINO.op("=", syntaxRuleGet, compile(a[1])), a[0])
    }
  })

  func("make-box", function (x) {
    if (x == null) {
      return NINO.op("unique")
    } else {
      return NINO.op("unique", x)
    }
  })

  set("sym", {
    "&value": function (x) {
      return NINO.op("variable", x)
    },
    "&pattern": function (args, v, body) {
      compileOnlyError(args[0])

      return [box("if"), [box("sym=="), v, args[1]],
               body,
                                                           // TODO
               [box("&error"), v, [box("+"), "expected " + args[1] + "but got ", v]]]
    }
  })

  func("&error", function (x, y) {
    throw new NINO.Error(x, y)
  })

  func("&compile", function (x) {
    return compile(x)
  })

  func("sym==", function (w, y) {
    var x = NINO.unwrap(w)
    return x.op === "variable" && x.args[0] === y
  })

  macro("$eval", function (a) {
    var x = a[1]
    if (mode === "compile") {
      return NINO.enrich(compile(x), a[0])
    } else {
      return withMode("compile", function () {
        x = compile(x)
        x = NINO.traverse(x, n.globals)
        x = NINO.compile(x, n.globals, "expression")
        if (n.options.debug) {
          n.options.debug(x)
        }
        return NINO.enrich(NINO.op("bypass", eval(x)), a[0])
      })
    }
  })

  // TODO should handle things like literals, etc.
  macro("&", function (a) {
    var w    = a[1]
      , x    = NINO.unwrap(w)
      , args = [].slice.call(arguments, 1).map(compile)
    if (!boxOrSym(w) && x.op !== "string") {
      throw new NINO.Error(w, "expected variable or string but got " + NINO.print(w))
    }
    return NINO.enrich(NINO.opArray(x.args[0], args), a[0])
  })

  macro("w/new-scope", function (a) {
    var x = a[1]
    return withBlockScope(function () {
      return NINO.enrich(compile(x), a[0])
    })
  })

  macro("\"", function (a) {
    var r = a.slice(1)
    r.unshift(NINO.op("string", ""))
    return NINO.enrich(NINO.opArray("+", r), a[0])
  })

  macro("null?", function (a) {
    var x = a[1]
    return NINO.enrich(NINO.op("==", compile(x), NINO.op("null")), a[0])
  })

  macro("finally", function (a) {
    var x = a[1]
      , y = a[2]
    return NINO.enrich(NINO.op("try", compile(x), NINO.op("finally", compile(y))), a[0])
  })

  macro("del", function (a) {
    var w = a[1]
      , x = NINO.unwrap(w)
      , y = compile(w)
    // TODO: make it work with boxes too?
    if (x.op === "variable") {
      delete vars[x.args[0]]
      return NINO.enrich(y, a[0])
    } else {
      var u = NINO.op("unique")
      return NINO.enrich(NINO.op(",", NINO.op("var", NINO.op("=", u, y)),
                                      NINO.op("delete", y),
                                      u),
                         a[0])
    }
  })

  macro("external", function (a) {
    a.slice(1).forEach(function (w) {
      var x = NINO.unwrap(w)
      if (Array.isArray(x)) {
        // TODO: use isSym ?
        if (!isBox(x[0], "=")) {
          throw new NINO.Error(x[0], "expected = but got " + NINO.print(x[0]))
        }
        symToExternal(x[1], x[2])
      } else {
        symToExternal(w, w)
      }
    })
    return NINO.enrich(NINO.op("empty"), a[0])
  })


  n.globals = {}
  n.options = {}

  n.isMacro = function (w) {
    var x = NINO.unwrap(w)
    if (x.op === "variable") {
      x = NINO.unwrap(getBox(w))
    }
    if (x.op === "unique" && x["&macro"] != null) {
      return x["&macro"]
    } else {
      return false
    }
  }

  n.compile = function (x) {
    return withMode("run", function () {
      return compile(x)
    })
  }

  return n
})(NULAN || {})
