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

  n.Number = function (value) {
    this.value = value
  }

  n.String = function (value) {
    this.value = value
  }

  function Uniq() {}


  var vars    = {} // Compile-time variables
    , js_vars = {} // JavaScript run-time variables
    , namespace

  function withNamespace(s, f) {
    var old = namespace
    namespace = s
    var x = f()
    namespace = old
    return x
  }

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

  function setUniq(s) {
    if (s instanceof Uniq) {
      return setUniq("u_")
    } else if (typeof s === "string") {
      return vars[s] = findUniq(s)
    } else {
      throw new Error("invalid variable: " + s)
    }
  }

  function getUniq(s) {
    if (s instanceof Uniq) {
      return getUniq("u_")
    } else if (typeof s === "string") {
      if (s in vars) {
        return vars[s]
      } else {
        throw new Error("undefined variable: " + s)
      }
    } else {
      throw new Error("invalid variable: " + s)
    }
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

  function mac(a) {
    if (Array.isArray(a)) {
      var x;
      if ((x = a[0]) instanceof Macro ||
          //!(a[0] in js_vars) &&
          // vars[]
          (x = getUniq(a[0])) instanceof Macro) {
        return x.value.apply(null, a.slice(1))
      } else {
        return ["call", mac(a[0]), a.slice(1).map(mac)]
      }
    } else {
      if (a instanceof n.Number) {
        return ["number", a.value]
      } else if (a instanceof n.String) {
        return ["string", a.value]
      } else if (a === void 0) {
        return ["void", ["number", "0"]]
      } else {
        if (namespace) {
          return ["[]", ["name", namespace], ["string", getUniq(a)]]
        } else {
          return ["name", mangle(getUniq(a))]
        }
      }
    }
  }

  // TODO: some code duplication with mac
  function $mac(a) {
    if (Array.isArray(a)) {
      return ["call", $mac(a[0]), a.slice(1).map($mac)]
    } else {
      if (a instanceof n.Number) {
        return ["number", a.value]
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

  function bin(s, a, i) {
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
        return bin(s, [].map.call(arguments, mac))
      }
    })
  }

  function binreduce1(s, i) {
    return new Macro(function () {
      switch (arguments.length) {
      case 0:
        if (typeof i === "string") {
          throw new Error(i + " cannot be called with 0 arguments")
        } else {
          throw new Error(s + " cannot be called with 0 arguments")
        }
      case 1:
        if (typeof i === "function") {
          return i(mac(arguments[0]))
        } else {
          return mac(arguments[0])
        }
      default:
        return bin(s, [].map.call(arguments, mac))
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


  vars["not"] = unary("!")
  vars["num"] = unary("u+")

  vars["mod"] = binary("%")

  vars["add"] = binreduce0("+", ["number", "0"])
  vars["mul"] = binreduce0("*", ["number", "1"])
  vars["and"] = binreduce0("&&", ["boolean", "true"])
  vars["or"]  = binreduce0("||", ["boolean", "false"])

  vars["sub"] = binreduce1("-", function (x) { return ["u-", x] })
  vars["div"] = binreduce1("/")
  vars["do"]  = binreduce1(";", "do")

  vars["str"] = new Macro(function () {
    // TODO: a little clunky
    return mac([vars["+"]].concat(new n.String(""), [].slice.call(arguments)))
  })

  vars["new"] = new Macro(function (x) {
    var args = [].slice.call(arguments, 1).map(mac)
    return ["new", mac(x), args]
  })

  vars["var"] = new Macro(function (x, y) {
    if (namespace) {
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

  vars["del"] = new Macro(function (x) {
    if (typeof x === "string") {
      var s = mac(x)
      delete vars[x]
      return s
    } else {
      // TODO: return the value that's being deleted
      return ["delete", mac(x)]
    }
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

  vars["dict"] = new Macro(function () {
    if (arguments.length === 0) {
      return ["object", []]
    } else {
      // TODO
      var a = pair(arguments).map(function (a) {
        return [vars["set!"], ["get", "foo", a[0]], a[1]]
      })
      return mac([vars["do"], [vars["var"], "foo", [vars["dict"]]]].concat(a))
    }
    /*var x = setUniq("foo")
    var a = pair(arguments).map(function (a) {
      return ["=", [".", ["name", x], a[0]], mac(a[1])]
    })
    a.unshift(["var", [[x, ["object", []]]]])
    return bin(";", a)*/
  })

  vars["uniq"] = new Macro(function () {
    return ["string", mangle(setUniq("u_"))]
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


  vars["set!"] = new Macro(function (x, y) {
    return ["=", mac(x), mac(y)]
  })

  vars["get"] = new Macro(function (x, y) {
    return ["[]", mac(x), mac(y)]
  })

  vars["fn"] = new Macro(function () {
    return ["function", "", [], []]
  })

  vars["namespace"] = new Macro(function (x) {
    namespace = x
    js_vars   = {}
    return ["id"]
  })

  /*function reducer(a, f) {
    return [].map.call(a, mac).reduce(f)
  }*/

  vars["w/namespace"] = new Macro(function (x) {
    var old1 = namespace
      , old2 = js_vars
    namespace = x
    js_vars   = {}
    var r = bin(",", [].slice.call(arguments, 1).map(mac))
    namespace = old1
    js_vars   = old2
    return r
  })

  vars["let"] = new Macro(function (x, y) {
    var body = [].slice.call(arguments, 2)
    var r = mac([vars["do"], [vars["var"], x, y]].concat(body))
    --js_vars[x] // TODO
    return r
  })

  // Macros
  vars["let!"] = new Macro(function (x, y) {
    var body = [].slice.call(arguments, 2)
      , u    = new Uniq()
    return mac([vars["let"], u, x,
                 [vars["set!"], x, y]].concat(body, [[vars["set!"], x, u]]))
  })


  vars["&typeof"]     = unary("typeof")
  vars["&in"]         = binary("in")
  vars["&instanceof"] = binary("instanceof")

  vars["&"] = new Macro(function (x) {
    return $mac(x)
  })

  vars["&eval"] = new Macro(function (x) {
    return withNamespace("vars", function () {
      x = mac(x)
      x = NINO.compile1(x)
      console.log(x)
      return ["id", eval(x)]
    })
  })


  // TODO: ew
  Object.keys(vars).forEach(function (s) {
    js_vars[s] = true
  })

  n.vars    = vars
  n.js_vars = js_vars
  n.macex   = mac

  function transform(x) {
    if (Array.isArray(x)) {
      return x.map(transform)
    } else if (/^[0-9.]+$/.test(x)) {
      return new n.Number(x)
    } else {
      return x
    }
  }

  n.parse = function (s) {
    s = s.replace(/\(/g, "[")
         .replace(/\)/g, "]")
         .replace(/ +/g, ", ")
         .replace(/[^\[\], \n]+/g, "\"$&\"")
         //.replace(/[a-zA-Z\.+]+/g, "[\"name\", \"$&\"]")
         //.replace(/[0-9\.]+/g, "[\"number\", \"$&\"]")
    return transform(JSON.parse(s))
  }

  n.compile = function (s) {
    return NINO.compile1(mac(n.parse(s)))
  }

  return n
})(NULAN || {})
