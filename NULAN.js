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

  n.Symbol = function (value) {
    this.value = value
  }
  n.Symbol.prototype.toString = function () {
    return this.value
  }

  n.Error = function (s) {
    this.message = s
  }
  n.Error.prototype = new Error()
  n.Error.prototype.constructor = n.Error
  n.Error.prototype.name = "NULAN.Error"

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

  function symToString(s) {
    if (s instanceof Uniq) {
      if (s.var) {
        return s.var
      } else {
        return s.var = setUniq(new n.Symbol("_u"))
      }
    } else if (s instanceof n.Symbol) {
      return s.value
    }
  }

  function setUniq(s) {
    if (s instanceof Uniq) {
      // TODO is this code needed in both setUniq and getUniq?
      return symToString(s)
    } else if (s instanceof n.Symbol) {
      return vars[s.value] = findUniq(s.value)
    } else {
      throw new n.Error("invalid variable: " + s)
    }
  }

  function getUniq(s) {
    if (s instanceof Uniq) {
      // TODO is this code needed in both setUniq and getUniq?
      return symToString(s)
    } else if (s instanceof n.Bypass) {
      return s.value
    } else if (s instanceof n.Symbol) {
      s = s.value
      if (s in vars) {
        return vars[s]
      } else {
        throw new n.Error("undefined variable: " + s)
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
      if (Array.isArray(x) && isValue(x[0], "splice")) {
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
      if (Array.isArray(x) && isValue(x[0], "splice")) {
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
      if (Array.isArray(x) && isValue(x[0], "splice")) {
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
      } else if (a.length === 0) { // TODO: this should probably be in splicingArgs
        return ["void", ["number", "0"]]
      } else {
        return splicingArgs(mac(a[0]), a.slice(1))
      }
    } else {
      if (typeof a === "number") {
        return ["number", "" + a]
      } else if (typeof a === "string") {
        return ["string", a]
      } else if (a === void 0) {
        return ["void", ["number", "0"]]
      } else {
        x = getUniq(a)
        if (typeof x !== "string") {
          throw new n.Error("invalid variable: " + x)
        }
                          // TODO
        if (namespace && !locals[a.value]) {
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
      } else if (typeof a === "string") {
        return ["string", a]
      } else if (a instanceof n.Symbol) {
        return ["name", a.value]
      } else {
        throw new n.Error("invalid expression: " + a)
      }
    }
  }

  // TODO: ugh I wish I could do this natively in JS
  n.pair = function (a) {
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
        throw new n.Error((typeof i === "string" ? i : s) +
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
    console.log(this)
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

  function evalIn(s, x) {
    return withNamespace(s, function () {
      x = mac(x)
      x = NINO.compile(NINO.transform([x]))
      console.log(x)
      return ["id", eval(x)]
    })
  }


  vars["%t"] = "true"

  values["num"] = unary("u+")
  values["mod"] = binary("%")

  values["dict"] = new Macro(function () {
    var args = n.pair(arguments)
      , a
      , u
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
        if (x instanceof n.Symbol) {
          x = x.value
        }
        return [values["set!"], [values["get"], u, x], y]
      })
      return mac([values["do"], [values["var"], [values["is"], u, [values["dict"]]]]].concat(a))
    }
  })

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
    var args = [].slice.call(arguments, 1).map(mac)
    return ["new", mac(x), args]
  })

  values["var"] = new Macro(function (x) {
    var y, body

    if (Array.isArray(x)) {
      // TODO: code duplication with "let"
      if (!isValue(x[0], "is")) {
        y = [].slice.call(arguments, 1).join(" ")
        throw new n.Error("expected <" + x + " = " + y +
                          "> but got <" + x + " " + y + ">")
      }

      y = (arguments.length > 1
            ? [x[2]].concat([].slice.call(arguments, 1))
            : x[2])
      x = x[1]
    }

                      // TODO
    if (namespace && !locals[x.value]) {
      return ["=", ["[]", ["name", namespace], ["string", setUniq(x)]], mac(y)]
    } else {
      if (y === void 0) {
        x = setUniq(x)
        values[x] = new n.Symbol(x) // TODO
        return ["var", [[mangle(x)]]]
      } else {
        y = mac(y)
        x = setUniq(x)
        values[x] = new n.Symbol(x) // TODO
        return ["var", [[mangle(x), y]]]
      }
    }
  })

  values["del"] = new Macro(function (x) {
    if (x instanceof n.Symbol) {
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

  values["let"] = new Macro(function (x) {
    var y, body

    // TODO: code duplication with var
    if (!isValue(x[0], "is")) {
      y = [].slice.call(arguments, 1, -1).join(" ")
      throw new n.Error("expected <" + x + " = " + y +
                        "> but got <" + x + " " + y + ">")
    }

    y = (arguments.length > 2
          ? [x[2]].concat([].slice.call(arguments, 1, -1))
          : x[2])
    body = arguments[arguments.length - 1]
    x = x[1]

    var old
      , s = symToString(x)
      , b = (s in vars)

    if (b) {
      old = vars[s]
    }
    locals[s] = true // TODO

    var r = mac([values["do"], [values["var"], [values["is"], x, y]], body])

    if (b) {
      vars[s] = old
    } else {
      delete vars[s]
    }
    delete locals[s] // TODO

    return r
  })

  /*values["let"] = new Macro(function (x, y, body) {
    return mac([[values["fn"], [values["list"], x], body], y])
  })*/

  values["sym"] = function (x) {
    return new n.Symbol(x)
  }

  values["finally"] = new Macro(function (x, y) {
    return ["try", [mac(x)], ["finally", [mac(y)]]]
    /*var u = new Uniq()
    return ["try", [mac([values["var"], [values["is"], u, x]])], ["finally", [mac(y), mac(u)]]]*/
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
    return ["empty"]
  })


  // Implementation specific stuff
  values["&typeof"]     = unary("typeof")
  values["not"]         = unary("!")

  values["&in"]         = binary("in")
  values["&instanceof"] = binary("instanceof")

  values["is"]          = binand("===", ["boolean", "true"])
  values["lt"]          = binand("<",   ["boolean", "true"])
  values["islt"]        = binand("<=",  ["boolean", "true"])
  values["gt"]          = binand(">",   ["boolean", "true"])
  values["gtis"]        = binand(">=",  ["boolean", "true"])

  values["and"]         = binreduce0("&&", ["boolean", "true"])
  values["or"]          = binreduce0("||", ["boolean", "false"])
  values["add"]         = binreduce0("+",  ["number", "0"])
  values["mul"]         = binreduce0("*",  ["number", "1"])

  values["sub"]         = binreduce1("-", function (x) { return ["u-", x] })
  values["do"]          = binreduce1(",", "do")
  values["div"]         = binreduce1("/")

  values["set!"] = new Macro(function (x, y) {
    return ["=", mac(x), mac(y)]
  })

  values["list"] = new Macro(function () {
    return ["array", [].map.call(arguments, mac)]
  })

  values["get"] = new Macro(function (x, y) {
    if ((typeof y === "string" || typeof y === "number") &&
        /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(y)) {
      return [".", mac(x), y]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  })

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


  function destructure(args, x, body) {

  }

  function destructureList(args, body) {

  }

  function destructureDict(args, body) {

  }

  values["fn"] = new Macro(function anon(args, body) {
    if (args instanceof n.Symbol) {
      // TODO
      return anon(["splice", args], body)
    } else if (isValue(args[0], "list")) {
      var old = js_vars
      js_vars = {}
      var r = withNewScope(function () {
/*
        -> a b @c d @e
        -> a b @c d e


        1 2 3 4 5 6 7 8 9 10 11 12

        a = 1
        b = 2
        c = 3 4 5 6 7
        d =
        e = 8 9 10 11 12

        function (a, b) {
          var c   = [].slice.call(arguments, 2, -2)
            , __u = arguments.length
            , d   = arguments[__u - 2]
            , e   = arguments[__u - 1]
        }

        -> a b @c '(d '(o e ) @f)

        '(foo (bar qux) corge)

        {a (+ b 5) @c d}

        {def foo -> a b @c ...}

        '(def foo '-> a b '@c ...)

        function (__u) {
          var a = __u[0]
            , b = __u[1]
            , c = [].slice.call(__u, 2, -1)
            , d = __u[__u.length - 1]
        }

        var slice = Function.prototype.call.bind([].slice)

        slice(arguments)

        [].slice.call(arguments)


        -> {a @b} b

        function (__u) {
          var a = __u[0]
          var b = [].slice.call(__u, 1)
          return b
        }
*/
        var s, x, r = [], a = args.slice(1)
        var before = []
        for (var i = 0, iLen = a.length; i < iLen; ++i) {
          x = a[i]
          if (x instanceof n.Symbol || x instanceof Uniq) {
            s = symToString(x)
            vars[s] = s
            locals[s] = true
            r.push(mangle(s))
          } else if (Array.isArray(x)) {
            if (isValue(x[0], "splice")) {
              var aArgs, iLeft = (i - iLen + 1)
              aArgs = (iLeft === 0
                        ? (i === 0
                            ? [["name", "arguments"]]
                            : [["name", "arguments"],
                               ["number", "" + i]])
                        : [["name", "arguments"],
                          ["number", "" + i],
                          ["number", "" + iLeft]])
              aArgs = ["call", [".", [".", ["array", []], "slice"], "call"],
                               aArgs]
              s = symToString(x[1])
              vars[s] = s
              locals[s] = true
              before.push(["var", [[mangle(s), aArgs]]])
              ++i
              while (i < iLen) {
                x = a[i]
                s = symToString(x)
                vars[s] = s
                locals[s] = true
                aArgs = ["[]", ["name", "arguments"],
                               ["-", [".", ["name", "arguments"], "length"],
                                     ["number", "" + (iLen - i)]]]
                before.push(["var", [[mangle(s), aArgs]]])
                ++i
              }
            }
          } else {
            s = new Uniq()
            body = [values["if"],
                     [values["is"], s, x],
                     body,
                     [values["error"], [values["add"], "expected " + x + " but got ", s]]]
            r.push(mangle(symToString(s)))
          }
        }
        body = before.concat([mac(body)])
        body[body.length - 1] = ["return", body[body.length - 1]]
        return ["function", "", r, body]
      })
      js_vars = old
      return r
    } else {
      throw new n.Error("invalid function argument list: " + args)
    }
  })

  values["&"] = new Macro(function (x) {
    return $mac(x)
  })

  values["&eval"] = new Macro(function (x) {
    return evalIn("values", x)
  })

  // (&eval '{1 2 3})
  // (&eval (let u (sym "foo") u))

  // (&eval '{1 2 {3 4 5}})
  // (&eval '{1 ,@2 ,@{3 4 5}})
  // (&eval '(1 2 '(3 {4} 5)))

  values["quote"] = new Macro(function loop(x) {
    // TODO: a little bit hacky, but it'll do for now
    if (Array.isArray(x)) {
      if (isValue(x[0], "comma") && (x = x[1])) {
        if (Array.isArray(x) && isValue(x[0], "splice")) {
          throw new n.Error("',@ is invalid")
        } else {
          return mac(x)
        }
      } else {
        return arraySplitter(x, function (x) {
          if (Array.isArray(x) && isValue(x[0], "comma")  && (x = x[1]) &&
              Array.isArray(x) && isValue(x[0], "splice") && (x = x[1])) {
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
    } else {
      return mac(x)
    }
  })



  // '(1 2 ,@{3 4} 5)
  // {1 2 @{3 4} 5}
  // [1, 2].concat([3, 4], [5])

  // '({1 2} ,@{3 4} @5)
  // {{list 1 2} @{3 4} {&splice 5}}
  // [["list", 1, 2]].concat([3, 4], [["splice", 5]])

  values["comma"] = new Macro(function (x) {
    return mac(x)
  })

  // TODO: lots of code duplication with mac
  values["&mac"] = new Macro(function (n, v) {
    v = evalIn("values", v)[1]
    values[setUniq(n)] = new Macro(function () {
      return v.apply(this, arguments)
    })
    return ["empty"]
  })


  // Macros
  /*
  values["isnt"] = new Macro(function () {
    if (arguments.length === 2) {
      return ["!==", mac(arguments[0]), mac(arguments[1])]
    } else {
      return mac([values["not"], [values["is"]].concat([].slice.call(arguments))])
    }
  })
*/
/*
  (def len -> x
    (call-own x %len))
*/


  // TODO: ew
  Object.keys(values).forEach(function (s) {
    vars[s] = s
    js_vars[s] = true
  })

  // TODO
  vars["test"] = "test"

  n.import = function () {
    [].forEach.call(arguments, function (s) {
      n.compile(n.parse(n.readFile(s)))
    })
  }

  n.vars    = vars
  n.values  = values
  n.js_vars = js_vars
  n.macex   = mac

  n.compile = function (a) {
    return NINO.compile(NINO.transform(a.map(mac)))
  }

  return n
})(NULAN || {})
