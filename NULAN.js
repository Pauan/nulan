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

  function Macro(value) {
    this.value = value
  }
  Macro.prototype.toString = function () {
    return "#<mac>"
  }

  n.Bypass = function (value) {
    this.value = value
  }

  n.String = function (value) {
    this.value = value
  }

  function Uniq() {}


  var vars    = {} // Nulan variables -> JS variables
    , locals  = {} // Nulan variables -> true (if variable is local)
    , js_vars = {} // JS variables    -> true (if variable exists)
    , values  = {} // JS variables    -> Compile-time values
    , namespace

  // Stuff dealing with variables
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

  function uniqToSym(s) {
    if (s instanceof Uniq) {
      if (s.var) {
        return s.var
      } else {
        return s.var = setUniq("_u")
      }
    } else {
      return s
    }
  }

  function setUniq(s) {
    if (s instanceof Uniq) {
      // TODO is this code needed in both setUniq and getUniq?
      return uniqToSym(s)
    } else if (typeof s === "string") {
      return vars[s] = findUniq(s)
    } else {
      throw new Error("invalid variable: " + s)
    }
  }

  function getUniq(s) {
    if (s instanceof Uniq) {
      // TODO is this code needed in both setUniq and getUniq?
      return uniqToSym(s)
    } else if (s instanceof n.Bypass) {
      return s.value
    } else if (typeof s === "string") {
      if (s in vars) {
        return vars[s]
      } else {
        throw new Error("undefined variable: " + s)
      }
    }
  }

  function isValue(x, s) {
    return x === values[s] || getUniq(x) === s
  }

  function mangle(s) {
    return s.replace(/(?:^[^$a-zA-Z])|[^$a-zA-Z0-9]/g, function (s) {
      return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
    })
  }

  // Not actually used, but still nice to have
  function unmangle(s) {
    return s.replace(/_([^_]*)_/g, function (_, s) {
      return s === "" ? "_" : String.fromCharCode(s)
    })
  }


  // Stuff dealing with scope
  function withNewScope(f) {
    var old  = vars
      , old2 = locals
    vars   = Object.create(vars)
    locals = Object.create(locals)
    var x = f()
    vars   = old
    locals = old2
    return x
  }

  function withNamespace(s, f) {
    var old = namespace
    namespace = s
    var x = f()
    namespace = old
    return x
  }

  function splicingArgsRest(x, i, iLen, a) {
    var r = [x]
    while (i < iLen) {
      x = a[i]
      if (Array.isArray(x) && isValue(x[0], "&splice")) {
        r.push(mac(x[1]))
      } else {
        r.push(["array", [mac(x)]])
      }
      ++i
    }
    return r
  }

  function arrayJoiner(a, when, end) {
    var x, x2, l = [], r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      x = a[i]
      x2 = when(x)
      if (x2) {
        r.push(mac(x2))
        ++i

        while (i < iLen) {
          x = a[i]
          x2 = when(x)
          if (x2) {
            r.push(mac(x2))
          } else {
            r.push(["array", [mac(x)]])
          }
          ++i
        }

        return end((l.length === 0
                     ? (r.length === 1
                         ? r[0]
                         : ["call", [".", r[0], "concat"], r.slice(1)])
                     : ["call", [".", ["array", l], "concat"], r]),
                   true)
      } else {
        l.push(mac(x))
      }
    }
    return end(l, false)
  }

  function splicingArgs(f, a) {
    return arrayJoiner(a, function (x) {
      if (Array.isArray(x) && isValue(x[0], "&splice")) {
        return x[1]
      }
    }, function (x, b) {
      return ["call", (b ? [".", f, "apply"] : f), [["null"], x]]
    })
/*
    var x, r2, r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      x = a[i]
      if (Array.isArray(x) && isValue(x[0], "&splice")) {
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
      if ((x = a[0]) instanceof Macro ||
          (x = values[getUniq(a[0])]) instanceof Macro) {
        return x.value.apply(null, a.slice(1))
      } else {
        return splicingArgs(mac(a[0]), a.slice(1))
      }
    } else {
      if (typeof a === "number") {
        return ["number", "" + a]
      } else if (a instanceof n.String) {
        return ["string", a.value]
      } else if (a === void 0) {
        return ["void", ["number", "0"]]
      } else {
        x = getUniq(a)
        if (typeof x !== "string") {
          throw new Error("invalid variable: " + x)
        }
        if (namespace && !locals[a]) {
          return ["[]", ["name", namespace],
                        ["string", x]] // (typeof x === "string" ? x : a)
        } else {
          return ["name", mangle(x)]
        }
      }
    }
  }

  // TODO: some code duplication with mac
  function $mac(a) {
    if (Array.isArray(a)) {
      return ["call", $mac(a[0]), a.slice(1).map($mac)]
    } else {
      if (typeof a === "number") {
        return ["number", "" + a]
      } else if (a instanceof n.String) {
        return ["string", a.value]
      } else {
        return ["name", a]
      }
    }
  }

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
        throw new Error((typeof i === "string" ? i : s) +
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

  function evalIn(s, x) {
    return withNamespace(s, function () {
      x = mac(x)
      x = NINO.compile1(x)
      console.log(x)
      return ["id", eval(x)]
    })
  }


  values["not"] = unary("!")
  values["num"] = unary("u+")

  values["mod"] = binary("%")

  values["add"] = binreduce0("+", ["number", "0"])
  values["mul"] = binreduce0("*", ["number", "1"])
  values["and"] = binreduce0("&&", ["boolean", "true"])
  values["or"]  = binreduce0("||", ["boolean", "false"])

  values["sub"] = binreduce1("-", function (x) { return ["u-", x] })
  values["div"] = binreduce1("/")
  values["do"]  = binreduce1(";", "do")

  values["new"] = new Macro(function (x) {
    var args = [].slice.call(arguments, 1).map(mac)
    return ["new", mac(x), args]
  })

  values["var"] = new Macro(function (x, y) {
    if (namespace && !locals[x]) {
      return ["=", ["[]", ["name", namespace], ["string", setUniq(x)]], mac(y)]
    } else {
      if (y === void 0) {
        return ["var", [[mangle(setUniq(x))]]]
      } else {
        y = mac(y)
        x = mangle(setUniq(x))
        return [";", ["var", [[x]]],
                           // TODO
                     ["=", ["name", x], y]]
      }
    }
  })

  values["del"] = new Macro(function (x) {
    if (typeof x === "string") {
      var s = mac(x)
      delete vars[x]
      return s
    } else {
      // TODO: return the value that's being deleted
      return ["delete", mac(x)]
    }
  })

  values["include"] = new Macro(function () {
    [].forEach.call(arguments, function (x) {
      vars[x] = x
      js_vars[x] = true
    })
    return ["id"]
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

  values["dict"] = new Macro(function () {
    if (arguments.length === 0) {
      return ["object", []]
    } else {
      // TODO
      var a = pair(arguments).map(function (a) {
        return [values["set!"], ["get", "foo", a[0]], a[1]]
      })
      return mac([values["do"], [values["var"], "foo", [values["dict"]]]].concat(a))
    }
    /*var x = setUniq("foo")
    var a = pair(arguments).map(function (a) {
      return ["=", [".", ["name", x], a[0]], mac(a[1])]
    })
    a.unshift(["var", [[x, ["object", []]]]])
    return binr(";", a)*/
  })

  values["list"] = new Macro(function () {
    return ["array", [].map.call(arguments, mac)]
  })

  values["uniq"] = new Macro(function () {
    return ["string", mangle(setUniq("u_"))]
  })

  values["w/newScope"] = new Macro(function (body) {
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


  values["set!"] = new Macro(function (x, y) {
    return ["=", mac(x), mac(y)]
  })

  values["get"] = new Macro(function (x, y) {
    if ((y instanceof n.String || typeof y === "number") &&
        /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(y.value)) {
      return [".", mac(x), y.value]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  })

  values["fn"] = new Macro(function (args, body) {
    if (typeof args === "string") {
      var x = ["call", [".", [".", ["array", []], "slice"], "call"],
                       [["name", "arguments"]]]
      return withNewScope(function () {
        args = uniqToSym(args)
        vars[args] = args
        locals[args] = true
        return ["function", "", [],
                 [["var", [[mangle(args), x]]],
                  ["return", mac(body)]]]
      })
    } else if (isValue(args[0], "list")) {
      return withNewScope(function () {
        args = args.slice(1).map(function (s) {
          s = uniqToSym(s)
          console.log(s)
          vars[s] = s
          locals[s] = true
          return mangle(s)
        })
        return ["function", "", args, [["return", mac(body)]]]
      })
    }
  })
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
    return ["id"]
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

  values["let"] = new Macro(function (x, y, body) {
    var old, b = (x in vars)
    if (b) {
      old = vars[x]
    }

    var r = mac([values["do"], [values["var"], x, y], body])

    if (b) {
      vars[x] = old
    } else {
      delete vars[x]
    }
    return r
  })

  values["let"] = new Macro(function (x, y, body) {
    return mac([[values["fn"], [values["list"], x], body], y])
  })


  // Implementation specific stuff
  values["&typeof"]     = unary("typeof")
  values["&in"]         = binary("in")
  values["&instanceof"] = binary("instanceof")

  values["&"] = new Macro(function (x) {
    return $mac(x)
  })

  values["&eval"] = new Macro(function (x) {
    return evalIn("values", x)
  })

  // (&eval '{1 2 3})
  // (&eval (let u (sym "foo") u))

  values["&quote"] = new Macro(function (x) {
    function spliceLoop(x) {
      var r = []
      x.forEach(function (x) {
        if (Array.isArray(x)) {

          if (isValue(x[0], "&comma")) {
            x = x[1]
            if (Array.isArray(x) && isValue(x[0], "&splice")) {
              console.log(mac(x[1]))
              return mac(x[1])
            } else {
              r.push(mac(x))
            }
          } else {
            r.push(["array", spliceLoop(x)])
          }
        } else {
          r.push(mac(x))
        }
      })
      return r
    }

    function loop(x) {
      if (Array.isArray(x)) {
        return arrayJoiner(x, function (x) {
          if (Array.isArray(x) && isValue(x[0], "&comma") &&
              Array.isArray(x[1]) && isValue(x[1][0], "&splice")) {
            return x[1][1]
          }
        }, function (x, b) {
          if (b) {
            return x
          } else {
            return ["array", x]
          }
        })
        //return ["array", spliceLoop(x)]
      } else {
        return mac(x)
      }
    }
    x = loop(x)
    console.log(x[1])
    return x
  })



  // '(1 2 ,@{3 4} 5)
  // {1 2 @{3 4} 5}
  // [1, 2].concat([3, 4], [5])

  // '({1 2} ,@{3 4} @5)
  // {{list 1 2} @{3 4} {&splice 5}}
  // [["list", 1, 2]].concat([3, 4], [["&splice", 5]])

  values["&comma"] = new Macro(function (x) {
    return mac(x)
  })

  values["sym"] = function (x) {
    return x
  }

  values["finally"] = new Macro(function (x, y) {
    var u = new Uniq()
    return ["try", [mac([values["var"], u, x])], ["finally", [mac(y), mac(u)]]]
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
  values["uniq"] = function () {
    return new Uniq()
  }

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
    return ["id"]
  })

  // TODO: lots of code duplication with mac
  values["&mac"] = new Macro(function (n, v) {
    v = evalIn("values", v)[1]
    values[setUniq(n)] = new Macro(function () {
      return v.apply(this, arguments)
    })
    return ["id"]
  })


  // Macros
  /*values["str"] = new Macro(function () {
    // TODO: a little clunky
    return mac([values["add"]].concat(new n.String(""), [].slice.call(arguments)))
  })

  values["let!"] = new Macro(function (x, y, body) {
    var u = new Uniq()
    return mac([values["let"], u, x,
                 [values["do"], [values["set!"], x, y],
                                [values["finally"], body,
                                                    [values["set!"], x, u]]]])
  })

  values["def"] = new Macro(function (n, v) {
    return mac([values["do"], [values["var"], n], [values["set!"], n, v]])
  })*/


  // TODO: ew
  Object.keys(values).forEach(function (s) {
    vars[s] = s
    js_vars[s] = true
  })

  n.import = function () {
    [].forEach.call(arguments, function (s) {
      s = n.parse(n.readFile(s))
      console.log(n.compile(s))
    })
  }

  n.vars    = vars
  n.values  = values
  n.js_vars = js_vars
  n.macex   = mac

  n.compile = function (a) {
    return NINO.compile(a.map(mac)) // NINO.hoistVars(
  }

  return n
})(NULAN || {})
