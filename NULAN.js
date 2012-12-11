// TODO: try and make Uniq inherit from Symbol (or perhaps the other way around?)

var prn = console.log;

function prn(x) {
  if (typeof x === "function") {
    console.log("#<fn>")
  } else {
    console.log(x.toString())
  }
}



var NULAN = (function (n) {
  "use strict";

  ;(function () {
    var js_vars = {} // JS variables -> true (if variable exists)
      , local

    function findUniq(sOld) {
      var s = sOld
        , i = 2
      while (true) {
        s = sOld + i
        if (!js_vars[s]) {
          js_vars[s] = true
          return s
        }
        ++i
      }
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
      this.original = this.value = s
    }
    n.Box.prototype.uniqize = function () {
      this.local = local
      this.value = this.original ? findUniq(this.original) : getUniq()
    }

    n.withLocalScope = function (f) {
      var old  = js_vars
        , old2 = local
      js_vars = Object.create(js_vars)
      local   = true
      var x = f()
      js_vars = old
      local   = old2
      return x
    }
  })()


  function Macro(value) {
    this.value = value
  }
  Macro.prototype.toString = function () {
    return "#<mac>"
  }


  n.Wrapper = function (value) {
    this.value = value
  }

  n.Box.prototype.toString = n.Wrapper.prototype.toString = function () {
    return this.value
  }


  var vars   = {} // Nulan variables -> Boxes
    , values = {} // Boxes           -> Compile-time values

  n.withNewScope = function (f) {
    var old = vars
    vars = Object.create(vars)
    var x = f()
    vars = old
    return x
  }

  var mode = "run" // Whether code is evaluated in run-time or compile-time

  n.withMode = function (s, f) {
    var old = mode
    mode = s
    var x = f()
    mode = old
    return x
  }

  /*function isSym(x, s) {
    if (x instanceof n.Box) {
      return x.original === s
    } else {
      return false
    }
  }*/

  function isBox(x, s) {
    if (x instanceof n.Box) {
      return x.value === s
    } else {
      return false
    }
  }

  function mangle(s) {
    return s.replace(/([a-z])\-([a-z])/g, function (_, s1, s2) {
      return s1 + s2.toLocaleUpperCase()
    }).replace(/(?:^[^$a-zA-Z])|[^$a-zA-Z0-9]/g, function (s) {
      return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
    })
  }

  // Not actually used, but still nice to have
  function unmangle(s) {
    return s.replace(/([a-z])([A-Z])/g, function (_, s1, s2) {
      return s1 + "-" + s2.toLocaleLowerCase()
    }).replace(/_([^_]*)_/g, function (_, s) {
      return s === "" ? "_" : String.fromCharCode(s)
    })
  }

  function splicingArgsRest(x, i, iLen, a) {
    var r = [x]
    while (i < iLen) {
      x = a[i]
      if (Array.isArray(x) && isBox(x[0], "@")) {
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
      if (Array.isArray(x) && isBox(x[0], "@")) {
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

  function mac(a) {
    var x
    if (Array.isArray(a)) {
      if ((x = a[0]) instanceof n.Box && (x = values[x.value]) instanceof Macro) {
        return x.value.apply(null, a.slice(1))
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
    } else if (a === void 0) { // TODO
      return ["void", ["number", "0"]]
    } else if (a instanceof n.Box) {
      x = a.value
      if (typeof x !== "string") {
        // TODO: is this code ever run?
        throw new n.Error(a, "invalid variable: " + a)
      }

      if (!a.local && mode === "compile") {
        return ["[]", ["name", "values"], ["string", x]]
      } else if (mode === "run") {
        return ["name", mangle(x)]
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
    if (x instanceof n.Box) {
      x = ["name", x.original] // TODO
    } else if (typeof x !== "string") {
      throw new n.Error(x, "invalid expression: " + x)
    }
    return (a.length === 1
             ? x
             : [x].concat([].slice.call(a, 1).map(mac)))
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
    return new Macro(function () {
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
    return new Macro(function () {
      switch (arguments.length) {
      case 0:
        throw new n.Error({}, (typeof i === "string" ? i : s) +
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
  Object.prototype.tap = function () {
    console.log(require("util").inspect(this, false, null, false))
    return this
  }

  function binand(s, i) {
    return new Macro(function () {
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
    return new Macro(function (x) {
      return [s, mac(x)]
    })
  }

  function binary(s) {
    return new Macro(function (x, y) {
      return [s, mac(x), mac(y)]
    })
  }

  function evalIn(x) {
    return withMode("compile", function () {
      x = mac(x)
      x = NINO.compile(NINO.transform([x]))
      console.log(x)
      return ["id", eval(x)]
    })
  }

  function validJS(x) {
    if (typeof x === "string") {
      if (/^[$_a-zA-Z](?:[a-z]\-[a-z]|[$_a-zA-Z0-9])*$/.test(x)) {
        return mangle(x) // TODO mangle
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


  // Syntax stuff
  values["~"]  = unary("!")
  values["=="] = binand("===", ["boolean", "true"])
  values["<"]  = binand("<",   ["boolean", "true"])
  values["=<"] = binand("<=",  ["boolean", "true"])
  values[">"]  = binand(">",   ["boolean", "true"])
  values[">="] = binand(">=",  ["boolean", "true"])

  values["&&"] = binreduce0("&&", ["boolean", "true"])
  values["||"] = binreduce0("||", ["boolean", "false"])
  values["+"]  = binreduce0("+",  ["number", "0"])
  values["*"]  = binreduce0("*",  ["number", "1"])

  values["-"]  = binreduce1("-", function (x) { return ["u-", x] })
  values["|"]  = binreduce1(",", "|")
  values["/"]  = binreduce1("/")

  values["<="] = new Macro(function (x, y) {
    return ["=", mac(x), mac(y)]
  })

  values["list"] = new Macro(function () {
    return ["array", [].map.call(arguments, mac)]
  })

  values["dict"] = new Macro(function () {
    var args = pair(arguments)
      , a
      , u
    if (args.every(function (a) { return a[0] instanceof n.Box })) {
      a = args.map(function (a) {
                // TODO verify that this is correct
        return [a[0].value, mac(a[1])]
      })
      return ["object", a]
    } else {
      u = new Uniq()
      a = args.map(function (a) {
        var x = a[0]
          , y = a[1]
        if (x instanceof n.Box) {
          x = x.value
        }
        return [values["<="], [values["."], u, x], y]
      })
      return mac([values["|"], [values["var"], [u, [values["dict"]]]]].concat(a))
    }
  })

  values["get"] = new Macro(function (x, y) {
    var s = validJS(y)
    if (s) {
      return [".", mac(x), s]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  })

  values["."] = new Macro(function (x, y) {
    if (y instanceof n.Box) {
      y = y.value
    }
    return mac([values["get"], x, y])
  })

  values["'"] = new Macro(function loop(x) {
    // TODO: a little bit hacky, but it'll do for now
    if (Array.isArray(x)) {
      var s = x[0]
      if (isBox(x[0], ",") && (x = x[1])) {
        if (Array.isArray(x) && isBox(x[0], "@")) {
                            // TODO: use store somehow?
          throw new n.Error({ text:   s.text
                            , column: s.column - 1
                            , line:   s.line
                            , length: 3 }, "',@ is invalid")
        } else {
          return mac(x)
        }
      } else {
        return arraySplitter(x, function (x) {
          if (Array.isArray(x) && isBox(x[0], ",")  && (x = x[1]) &&
              Array.isArray(x) && isBox(x[0], "@") && (x = x[1])) {
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
    } else if (x instanceof n.Box) {
      console.log("quote", x)
      return x
    } else {
      return mac(x)
    }
  })

  values[","] = new Macro(function (x) {
    return mac(x)
  })

  values["null?"] = new Macro(function (x) {
    return ["==", mac(x), ["null"]]
  })

  // TODO: move into NULAN.macros somehow
  values["="] = new Macro(function (x, y) {
    return mac([values["if"], [values["null?"], x], [values["<="], x, y]])
  })

  values["box=="] = new Macro(function (x, y) {
    return ["call", ["name", "isBox"], [mac(x), mac(y)]]
  })

  values["box"] = new Macro(function (x) {
    return ["new", [".", ["name", "n"], "Box"], [mac(x)]]
  })

  function slicer(v, i, iLen) {
    var r = [[values["get"],
              [values["get"], [values["list"]], "slice"],
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
        if (Array.isArray(x) && isBox(x[0], "@")) {
          index = i
        }
      })
      return args.reduceRight(function (x, y, i, a) {
        if (i === index) {
          return destructure1(y[1], slicer(v, i, a.length), x)
        } else if (i > index) {
          return destructure1(y, [values["get"], v,
                                  [values["-"],
                                   [values["get"], v, "length"],
                                   a.length - i]],
                                 x)
        } else {
          return destructure1(y, [values["get"], v, i], x)
        }
      }, body)
    },
    "dict": function (args, v, body) {
      return pair(args).reduceRight(function (x, y) {
        return destructure1(y[1], [values["get"], v, y[0]], x)
      }, body)
    },
    "=": function (args, v, body) {
      return destructure1(args[0], [values["if"], [values["null?"], v], args[1], v], body)
    },
    "box": function (args, v, body) {
      return [values["if"], [values["box=="], v, args[0]], body]
    }
  }

  function destructure1(args, v, body) {
    if (complex(args)) {
      var u = new Uniq()
      return [values["|"], [values["var"], [values["="], u, v]], destructure(args, u, body)]
    } else {
      return destructure(args, v, body)
    }
  }

  // isSymbol
  // values
  // Uniq
  // Symbol
  // Bypass

  function destructure(args, v, body) {
    var a, f
    if (args instanceof n.Box) {
      a = [values["var"], [values["="], args, v]]
              // TODO
      return (body
               ? [values["|"], a, body]
               : a)
    } else if (Array.isArray(args)) {
      if (args[0] instanceof n.Box && (f = patterns[args[0].value])) {
        return f(args.slice(1), v, body)
      } else {
        throw new n.Error(args[0], "not a pattern")
      }
    } else {
      return [values["if"],
               [values["=="], v, args],
               body,
               [values["error"], [values["+"], "expected " + args + " but got ", v]]]
    }
  }

  //pretty(destructure(["dict", new n.Symbol("foo"), new n.Symbol("bar"), new n.Symbol("qux"), 30, new n.Symbol("corge"), ["=", new n.Symbol("uuuuuu"), new n.Symbol("bar")]], new n.Symbol("u"), "END"))

  //pretty(destructure(["list", new n.Symbol("c"), ["@", new n.Symbol("a")], 20, new n.Symbol("b")], new n.Symbol("u"), ["list", new n.Symbol("a"), new n.Symbol("b")]))

  //pretty(destructure(["list", new n.Symbol("a"), 20, ["@", new n.Symbol("HAHA")], new n.Symbol("b"), ["=", new n.Symbol("c"), new n.Symbol("b")]], new n.Symbol("u"), "END"))

  values["->"] = new Macro(function anon(args, body) {
    if (Array.isArray(args)) {
      return withLocalScope(function () {
        // TODO
        js_vars["arguments"] = true
        return withNewScope(function () {
  /*
          var slice = Function.prototype.call.bind([].slice)
          slice(arguments)
  */
          var x, u, s, r = []

          for (var i = 0, iLen = args.length; i < iLen; ++i) {
            x = args[i]

            if (x instanceof n.Box) {
              vars[x] = x.uniqize()
              r.push(mangle(x.value))
            } else if (Array.isArray(x) && isBox(x[0], "@")) {
              s = new n.Box("arguments")

              u = (i === iLen - 1
                    ? s
                    : new n.Box())

              ;(function (i2) {
                var x
                while (i2 > i) {
                  x = args[i2]
                                        // TODO: code duplication with the list pattern
                  body = destructure(x, [values["get"], u,
                                          [values["-"],
                                           [values["get"], u, "length"],
                                           args.length - i2]],
                                        body)
                  --i2
                }
              })(iLen - 1)

              body = destructure(x[1], slicer(u, i, args.length), body)

              if (i !== iLen - 1) {
                body = destructure(u, s, body)
              }
              break
            } else {
              u = new n.Box()
              vars[u] = u.uniqize() // TODO: does this need to be added to vars?
              r.push(mangle(u.value))
              body = destructure(x, u, body)
            }
          }

          return ["function", "", r, [["return", mac(body)]]]
        })
      })
    } else {
      throw new n.Error(args, "invalid function argument list: " + args)
    }
  })


  // Other
  vars["%t"] = "true"
  values["true"] = new n.Symbol("true") // TODO

  values["num"] = unary("u+")
  values["mod"] = binary("%")

  values["++"] = new Macro(function (x, y) {
    if (y === void 0 || y === 1) {
      return ["++", mac(x)]
    } else {
      return ["+=", mac(x), mac(y)]
    }
  })

  values["--"] = new Macro(function (x, y) {
    if (y === void 0 || y === 1) {
      return ["--", mac(x)]
    } else {
      return ["-=", mac(x), mac(y)]
    }
  })

  values["new"] = new Macro(function (x) {
    var args = [].slice.call(arguments, 1)
    return ["new", mac(x), args.map(mac)]
  })

  values["while"] = new Macro(function (test, body) {
    return ["while", mac(test), [mac(body)]]
  })

  values["var"] = new Macro(function () {
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
      var y, z
      if (Array.isArray(x)) {
        if (!isValue(x[0], "=")) {
          throw new n.Error(x[0], "expected = but got " + x[0])
        }
        y = x[2]
        x = x[1]
      }
      // TODO: uniq value
      if (isSymbol(x)) {
        z = mac(y)
        // TODO
        /*if (namespace && !locals[x.value]) {
          return ["<=", ["[]", ["name", namespace], ["string", setUniq(x)]], z]
        } else {*/
          x = setUniq(x)
          values[x] = new n.Symbol(x) // TODO
          if (y === void 0) {
            return ["var", [[mangle(x)]]]
          } else {
            return ["var", [[mangle(x), z]]]
          }
        //}
      } else {
        // TODO: code duplication with destructure1
        if (complex(y)) {
          var u = new Uniq()
          return mac([values["|"], [values["var"], [values["="], u, y]], destructure(x, u)])
        } else {
          return mac(destructure(x, y))
        }
      }
    }).reduce(function (x, y) {
      return [",", x, y]
    })
  })

  function complex(x) {
    return Array.isArray(x)
  }

  values["w/var"] = new Macro(function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    // TODO: this needs to understand destructuring
    return withPartialScope(args, function () {
      return mac([values["|"], [values["var"]].concat(args), body])
    })
  })

  /*values["w/var"] = new Macro(function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    var x = args.map(function (x) { return x[0] })
      , y = args.map(function (x) { return x[1] })

    return mac([[values["->"], x, body]].concat(y))
  })*/

  values["del"] = new Macro(function (x) {
    if (isSymbol(x)) {
      var s = mac(x)
      delete vars[getUniq(x)] // TODO test this
      return s
    } else {
      // TODO: return the value that's being deleted
      return ["delete", mac(x)]
    }
  })

  values["include"] = new Macro(function () {
    [].forEach.call(arguments, function (x) {
      x = symToString(x) // TODO test this
      vars[x] = x
      js_vars[x] = true
      locals[x] = true // TODO test this
                       // (&eval '(&list 1 2 3))
                       // (include &list)
                       // (&eval '(&list 1 2 3))
    })
    return ["empty"]
  })
/*
  ++
  --
  <
  <=
  >
  =>
  =
  !=
  if
*/

  /*values["uniq"] = new Macro(function () {
    return ["string", mangle(setUniq("u_"))]
  })*/

  values["w/new-scope"] = new Macro(function (body) {
    return withNewScope(function () {
      return mac(body)
    })
  })


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

  values["namespace"] = new Macro(function (x) {
    namespace = x
    js_vars   = {}
    return ["empty"]
  })

  /*function reducer(a, f) {
    return [].map.call(a, mac).reduce(f)
  }*/

  values["w/namespace"] = new Macro(function (x, body) {
    var old1 = namespace
      , old2 = js_vars
    namespace = x
    js_vars   = {}
    var r = mac(body)
    //var r = binr(",", [].slice.call(arguments, 1).map(mac))
    namespace = old1
    js_vars   = old2
    return r
  })

  // TODO: convert this into a macro
  values["sym"] = function (x) {
    return new n.Symbol(x) // TODO: should probably return n.Bypass
  }

  values["finally"] = new Macro(function (x, y) {
    return ["try", [mac(x)], ["finally", [mac(y)]]]
    /*var u = new Uniq()
    return ["try", [mac([values["var"], [u, x]])], ["finally", [mac(y), mac(u)]]]*/
  })


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
  // Functions
  values["uniq"] = new Macro(function () {
    // TODO: make this accessible only to compile-time
    return ["new", ["name", "Uniq"], []]
  })

  /*
  values["&mac"] = function (f) {
    return new Macro(f)
  }

  values["&compile"] = mac

  values["&compiler"] = function (f) {
    return function () {
      return mac(f.apply(this, arguments))
    }
  }*/

  values["mac"] = new Macro(function (n, v) {
    v = evalIn("values", v)[1]
    values[setUniq(n)] = new Macro(function () {
      return mac(v.apply(this, arguments))
    })
    return ["empty"]
  })


  // Implementation specific stuff
  values["&typeof"]     = unary("typeof")
  values["&in"]         = binary("in")
  values["&instanceof"] = binary("instanceof")

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

  values["if"] = new Macro(function anon() {
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
  })

  values["error"] = new Macro(function (x) {
    return ["throw", ["new", ["name", "Error"], [mac(x)]]]
  })

  values["&"] = new Macro(function () {
    return $mac(arguments)
  })

  values["&eval"] = new Macro(function (x) {
    return evalIn("values", x)
  })

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

  // TODO: lots of code duplication with mac
  values["&mac"] = new Macro(function (n, v) {
    v = evalIn("values", v)[1]
    values[setUniq(n)] = new Macro(function () {
      return v.apply(this, arguments)
    })
    return ["empty"]
  })


  // TODO: ew
  Object.keys(values).forEach(function (s) {
    vars[s] = s
    js_vars[s] = true
  })

  // TODO
  vars["test"] = "test"

  n.import = function () {
    [].forEach.call(arguments, function (s) {
      n.parse(n.readFile(s), function (x) {
        n.compile(x)
      })
    })
  }

  n.vars    = vars
  n.values  = values
  n.js_vars = js_vars
  n.macex   = mac

  n.compile = function (a) {
    // TODO: change NINO to accept a single argument
    return NINO.compile(NINO.transform([mac(a)]))
  }

  return n
})(NULAN || {})
