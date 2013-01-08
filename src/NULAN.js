// TODO: only mangle the boxes at the last second ?

var NULAN = (function (n) {
  "use strict";


  Object.prototype.tap = function () {
    console.log(require("util").inspect(n.toJSON(this), false, null, false))
    return this
  }


  function compile(a) {
    // TODO: change NINO to accept a single argument
    // TODO: should partial or transform come first?
    // NINO.transform()
    return NINO.compile(NINO.partial(withExpression(function () {
      return withBlock(a)
    })))
  }


  function compileOnlyError(x) {
    if (mode !== "compile") {
      throw new n.Error(x, "cannot use " + x + " at " + mode + " time")
    }
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


  var expression = true
    , statements

  function isPure(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "empty":
      case "name":
      case "number":
      case "boolean":
      case "null":
      case "regexp":
      case "string":
      case "function":
        return true

      case "id":
      case "break":
      case "continue":
      case "debugger":
      case "return":
      case "throw":
      case "var":
      case "new":
      case "call":
      case "++":
      case "--":
      case "delete":
      case "=":
      case "+=":
      case "-=":
      case "*=":
      case "/=":
      case "%=":
      case "<<=":
      case ">>=":
      case ">>>=":
      case "&=":
      case "^=":
      case "|=":
      case "function-statement":
        return false

      default:
        return x.every(isPure)
      }
    } else {
      return true
    }
  }

  function isVoid(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "void":
      case "empty":
        return true
      }
    }
    return false
  }

  function withExpression(f) {
    var old = expression
    expression = true
    try {
      var x = f()
    } finally {
      expression = old
    }
    return x
  }

  function withStatement(f) {
    var old = expression
    expression = false
    try {
      var x = f()
    } finally {
      expression = old
    }
    return x
  }

  function mergeVars(x) {
    var r = []
      , a = []
    x.forEach(function (x) {
      if (Array.isArray(x) && x[0] === "var") {
        a.push(x[1][0])
      } else {
        if (a.length) {
          r.push(["var", a])
        }
        a = []
        r.push(x)
      }
    })
    if (a.length) {
      r.push(["var", a])
    }
    return r
  }

  function withBlock(x) {
    var old = statements
    statements = []
    try {
      statements.push(mac(x))
    } finally {
      x = statements
      statements = old
    }
    if (expression) {
      if (x.length) {
        // TODO: ew
        x = x.slice(0, -1).filter(function (x) {
          return !isPure(x)
        }).concat([x[x.length - 1]])
      }
    } else {
      x = x.filter(function (x) {
        return !isPure(x)
      })
    }
    return mergeVars(x)
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

  function validJS(x) {
    if (x instanceof n.Wrapper) {
      x = x.value
    }
    if (typeof x === "string" && /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)) {
      return x
    }
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
      if (Array.isArray(x) && n.isBox(x[0], "@")) {
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
    return {
      _38_macro: function (a) {
        switch (a.length) {
        case 1:
          throw new n.Error(a[0], a[0] + " cannot be called with 0 arguments")
        case 2:
          a = mac(a[1])
          return (typeof i === "function"
                   ? i(a)
                   : a)
        default:
          return binr(s, a.slice(1).map(mac))
        }
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
    var r = [[n.box("."),
              [n.box("."), [n.box("{")], "slice"],
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


  n.vars = {} // Nulan Variable -> JS Variable (guaranteed unique)

  var myMangle, Uniq, setBuiltin, tokenBox, setSymToBox, setSymExternal, setBox, getBox, withLocalScope, withNewScope, withPreviousScope, compileEval

/*
  var boxes = 5

  ->
    var boxes = 5

  $eval
    | var boxes = 5
    | ()

  $eval
    | ->
        var boxes = 5
    | ()


  var enum = 5

  ->
    var enum = 5

  $eval
    | var enum = 5
    | ()

  $eval
    | ->
        var enum = 5
    | ()
*/

  ;(function () {
    // TODO: I no longer need to expose boxes, but I may want to do so anyways
    n.boxes = {} // JS Variable -> Box

    var scope = "global"

    myMangle = function (x) {
      if (scope === "local" && mode === "compile" && x === "n") {
        return "_" + x
      } else {
        return n.mangle(x)
      }
    }

    function findUniq(sOld) {
      var s = sOld
        , i = 2
      while (n.boxes[s]) {
        s = sOld + i
        ++i
      }
      //n.boxes[s] = true
      return s
    }

    function getUniq() {
      var s, i = 1
      while (true) {
        for (var a = "a".charCodeAt(0), b = "z".charCodeAt(0); a <= b; ++a) {
          s = new Array(i + 1).join(String.fromCharCode(a))
          if (!n.boxes[s]) {
            //n.boxes[s] = true
            return s
          }
        }
        ++i
      }
    }

    n.Box = function (s) {
      this._38_originalName = this._38_uniqueName = s
      this._38_mode = {}
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
      return "#<box " + n.unmangle(this._38_uniqueName) + ">"
    }

    Uniq = function () {}
    Uniq.prototype = new n.Box()
    Uniq.prototype.toString = function () {
      if (this._38_uniqueName) {
               // TODO: should this use unmangle?
        return "#<uniq " + n.unmangle(this._38_uniqueName) + ">"
      } else {
        return "#<uniq>"
      }
    }
    /*Uniq.prototype.uniqize = function () {
      // TODO
      this.local = local
      //this.mode  = mode
      this.value = getUniq()
    }*/

    setBuiltin = function (sN, sJS) {
      var x = new n.Box(sJS)
      // TODO: code duplication with setSymToBox
      n.vars[sN] = x._38_uniqueName

      n.boxes[x._38_uniqueName] = x
      x._38_scope = "builtin"
      //x.local = local // TODO: local
      x._38_mode["compile"] = true
      x._38_mode["run"] = true
      return x
    }

    tokenBox = function (x, y) {
      n.tokenUpdate(x, function (o) {
        //o.type = "symbol"
        o.box = y
        //o.macro = !!n.isMacro(o.box)
      })
    }

    setSymToBox = function (x) {
      var y
      if (x instanceof n.Symbol) {
        y = setSymToBox(new n.Box(myMangle(x.value)))
        n.vars[x.value] = y._38_uniqueName
        tokenBox(x, y)
        return y
      } else if (x instanceof Uniq) {
        x._38_uniqueName = getUniq()
      } else if (x instanceof n.Box) {
        x._38_uniqueName = findUniq(x._38_originalName)
      }
      n.boxes[x._38_uniqueName] = x
      x._38_scope = scope
      //x.local = local
      x._38_mode[mode] = true
      return x
    }

    // TODO: should this call tokenBox?
    setSymExternal = function (s) {
      var x = setSymToBox(new n.Box(s.value))
      n.vars[s.value] = x._38_uniqueName
      return x
    }

    setBox = function (x, y) {
      x = setSymToBox(new n.Symbol(x))
      Object.keys(y).forEach(function (s) {
        x[s] = y[s]
      })
      return x
    }

    getBox = function (x, b) {
      if (x instanceof n.Box) {
        return x
      } else if (x instanceof n.Symbol) {
        var y = n.vars[x.value]
        if (y) {
          y = n.boxes[y]
          tokenBox(x, y)
          return y
        } else if (!b) {
          throw new n.Error(x, "undefined symbol: " + x)
        }
      }
    }

    n.box = function (s) {
      /*
      // TODO
      return withMode("compile", function () {
        return withGlobalScope(function () {
          //console.log(mode, local)

        })
      })*/
      // TODO does this need to call mangle?
      return n.boxes[myMangle(s)]
    }
/*
    function withGlobalScope(f) {
      var old2 = local
      local = false
      try {
        var x = f()
      } finally {
        local = old2
      }
      return x
    }*/

    var prevBoxes = n.boxes
      , prevVars  = n.vars

    withLocalScope = function (f) {
      var old  = n.boxes
        , old2 = scope
      n.boxes   = Object.create(n.boxes)
      prevBoxes = n.boxes
      scope     = "local"
      try {
        var x = f()
      } finally {
        n.boxes = old
        scope   = old2
      }
      return x
    }

    withNewScope = function (f) {
      var old  = n.vars
        , old2 = scope // TODO
      n.vars   = Object.create(n.vars)
      prevVars = n.vars
      scope    = "local" // TODO
      try {
        var x = f()
      } finally {
        n.vars = old
        scope  = old2 // TODO
      }
      return x
    }

    withPreviousScope = function (f) {
      var old  = n.boxes
        , old2 = n.vars
      n.boxes = prevBoxes
      n.vars  = prevVars
      try {
        var x = f()
      } finally {
        n.boxes = old
        n.vars  = old2
      }
      return x
    }

    n.withNewContext = function () {
      var old  = n.vars
        , old2 = n.boxes
        , old3 = n.syntaxRules
      n.vars        = Object.create(n.vars)
      n.boxes       = Object.create(n.boxes)
      n.syntaxRules = Object.create(n.syntaxRules)
      prevBoxes = n.boxes
      prevVars  = n.vars
      return function () {
        n.vars        = old
        n.boxes       = old2
        n.syntaxRules = old3
      }
    }

    n.isMacro = function (x) {
      if ((x = getBox(x)) && "_38_macro" in x) {
        return x._38_macro
      }
    }
  })()

  function compileEval(x) {
    if (mode === "compile") {
      return mac(x)
    } else {
      return withMode("compile", function () {
        x = compile(x)
        if (n.options.debug) {
          n.options.debug(x)
        }
        return ["id", eval(x)]
      })
    }
  }

  function isBox(x, s) {
    x = getBox(x)
    return x instanceof n.Box && x._38_uniqueName === s
  }

  n.isBox = function (x, s) {
    return isBox(x, myMangle(s))
  }

  function setMacro(s, f) {
    return setBox(s, {
      _38_macro: function (a) {
        return f.apply(this, a.slice(1))
      }
    })
  }

  /*setValue("syntax-rules", new Alias(function () {
    return n.syntaxRules
  }, function (x) {
    n.syntaxRules = x
  }))*/


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

  function checkBox(x, y) {
    // TODO: x.local ||
    if (y._38_mode[mode]) {
      return y
    } else {
      y = formatMode(Object.keys(y._38_mode))
      throw new n.Error(x, "undefined symbol (but it exists at " + y + " time): " + x)
    }
  }

  /*function isSym(x, s) {
    if (x instanceof n.Box) {
      return x.name === s
    } else {
      return false
    }
  }*/

  function macBox(x) {
    var y = getBox(x)
    /*if (x instanceof n.Symbol) {
      n.tokenUpdate(x, function (o) {
        //o.type = "symbol"
        o.box = y
        //o.macro = !!n.isMacro(o.box)
      })
    }*/
    if (y instanceof n.Box) {
      if (y._38_scope === "local") {
        return ["name", y._38_uniqueName]
      } else {
        return [".", [".", ["name", "n"], "boxes"], y._38_uniqueName]
      }
      /*return withMode("quote", function () {
        return mac(x)
      })*/
    }
    //withMode("quote", function () { return mac(a) })
  }

  function mac(a) {
    var x
    if (Array.isArray(a)) {
      if ((x = n.isMacro(a[0]))) {
        return x(a)
      } else if (a.length === 0) { // TODO: this should probably be in splicingArgs
        return ["empty"] // TODO: is this correct?
        //return ["void", ["number", "0"]]
      } else {
        return splicingArgs(mac(a[0]), a.slice(1))
      }
    } else if (a instanceof n.Wrapper) {
/*      n.tokenUpdate(a, function (o) {
        o.type = typeof a.value
      })*/
      return mac(a.value) // TODO: check all the places that currently expect strings/numbers and won't work with a wrapper
    } else if (typeof a === "number") {
      return ["number", "" + a]
    // TODO: find all places that unwrap to strings but don't call mac
    } else if (typeof a === "string") {
      return ["string", a]
    /*} else if (a === void 0) { // TODO
      return ["void", ["number", "0"]]*/
    } else if (a instanceof n.Symbol) {
      x = getBox(a)
      if ("_38_get" in x) {
        return mac(x)
      } else {
        return mac(checkBox(a, x))
      }
/*      n.tokenUpdate(a, function (o) {
        //o.type = "symbol"
        o.box = x
        //o.macro = !!n.isMacro(x)
      })*/
    } else if (a instanceof n.Box) {
      if ("_38_get" in a) {
        return a._38_get([a])
        /*if (mode === "compile") {
          return ["call", [".", macBox(a), "get"], []]
        } else {
          throw new n.Error(a, "cannot use getter at " + mode + " time")
        }*/
      } else if (mode === "run" || a._38_scope === "local" || a._38_scope === "builtin") {
        return ["name", a._38_uniqueName]
      } else if (mode === "compile") {
        //if ("_38_value" in a) {
        return [".", macBox(a), "_38_value"]
        // TODO
        /*} else {
          throw new n.Error(a, a + " does not have `&get` or `&value` property")
        }*/
      //} else if (mode === "quote") {
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
    a = a.map(mac)
    /*
    // TODO: make it work with boxes too
    if (x instanceof n.Symbol) {
      x = x.value
      //x = ["name", x.value]
    } else if (typeof x !== "string") {
      throw new n.Error(x, "invalid expression: " + x)
    }*/
    //return [n.box("{"), x].concat([].slice.call(a, 1))
    return (a.length === 1
             ? a[0]
             : [a[0][1]].concat(a.slice(1)))
  }

/*  function splicingArgsRest(x, i, iLen, a) {
    var r = [x]
    while (i < iLen) {
      x = a[i]
      if (Array.isArray(x) && n.isBox(x[0], "@")) {
        r.push(mac(x[1]))
      } else {
        r.push(["array", [mac(x)]])
      }
      ++i
    }
    return r
  }*/


  function destructure1(args, v, body) {
    if (complex(args)) {
      var u = new Uniq()
      return [n.box("|"), [n.box("box"), [n.box("="), u, v]], destructure(args, u, body)]
    } else {
      return destructure(args, v, body)
    }
  }

  function destructure(args, v, body) {
    var a, f
    if (wildcard(args)) {
      return body
    } else if (boxOrSym(args)) {
      a = [n.box("box"), [n.box("="), args, v]]
              // TODO better detection for if body is empty
      return (body
               ? [n.box("|"), a, body]
               : a)
    } else if (Array.isArray(args)) {
      if ((f = getBox(args[0])) && "_38_pattern" in f) {
        return f._38_pattern(args, v, body)
      } else {
        throw new n.Error(args[0], args[0] + " is not a pattern")
      }
    } else {
      return [n.box("if"),
               [n.box("=="), v, args],
               body,
               [n.box("throw"),
                 [n.box("new"), n.box("Error"),
                   [n.box("+"), "expected " + args + " but got ", v]]]]
    }
  }


  setBuiltin("%t", "true")
  setBuiltin("%f", "false")

  // TODO: `syntax-rules <= [ ... ]`
  //       `w/dict! syntax-rules ...
  setBox("syntax-rules", {
    _38_get: function (a) {
      compileOnlyError(a[0])
      return [".", ["name", "n"], "syntaxRules"]
    },
    _38_set: function (a) {
      compileOnlyError(a[0])
      return ["=", [".", ["name", "n"], "syntaxRules"], mac(a[1])]
    }
  })


  // Functions
  // TODO: rename?
  setBox("&box==", {
    _38_value: function (x, y) {
      return isBox(x, y)
    }
  })

  setBox("sym==", {
    _38_value: function (x, y) {
      return x instanceof n.Symbol && x.value === y
    }
  })

  setBox("make-uniq", {
    _38_value: function () {
      return new Uniq()
    }
  })

  setBox("sym", {
    _38_value: function (x) {
      return new n.Symbol(x)
    },
    _38_pattern: function (args, v, body) {
      compileOnlyError(args[0])

      return [n.box("if"), [n.box("sym=="), v, args[1]],
                           body,
                           [n.box("&error"), v, [n.box("+"), "expected " + args[1] + "but got ", v]]]
    }
  })

  setBox("&error", {
    _38_value: function (x, y) {
      throw new n.Error(x, y)
    }
  })

  setBox("&compile", {
    _38_value: function (x) {
      return mac(x)
    }
  })

  setBox("bound?", {
    _38_value: function (x) {
      // TODO x.value in n.vars
      return !!n.vars[x.value]
    }
  })


  // Simple complexity
  setMacro("num",         unary("u+"))
  setMacro("~",           unary("!"))
  setMacro("&typeof",     unary("typeof"))

  setMacro("&in",         binary("in"))
  setMacro("&instanceof", binary("instanceof"))
  setMacro("mod",         binary("%"))
  //setMacro("<=",          binary("="))

  setMacro("==",          binand("===", ["boolean", "true"]))
  setMacro("<",           binand("<",   ["boolean", "true"]))
  setMacro("=<",          binand("<=",  ["boolean", "true"]))
  setMacro(">",           binand(">",   ["boolean", "true"]))
  setMacro(">=",          binand(">=",  ["boolean", "true"]))

  setMacro("&&",          binreduce0("&&", ["boolean", "true"]))
  setMacro("||",          binreduce0("||", ["boolean", "false"]))
  setMacro("+",           binreduce0("+",  ["number", "0"]))
  setMacro("*",           binreduce0("*",  ["number", "1"]))

  setBox("-",             binreduce1("-", function (x) { return ["u-", x] }))
  //setBox("|",             binreduce1(","))
  setBox("/",             binreduce1("/"))

  setMacro("<=", function (x, y) {
    // TODO: a teensy tiny itty bitty bit hacky that I'm using getBox and checkBox in here
    var b = getBox(x)
    if (b) {
      if ("_38_set" in b) {
        return b._38_set([b, y])
        /*if (mode === "compile") {
          return ["call", [".", macBox(b), "set"], [mac(y)]]
        } else {
          throw new n.Error(b, "cannot use setter at " + mode + " time")
        }*/
      } else {
        return ["=", mac(checkBox(x, b)), mac(y)]
      }
    } else {
      return ["=", mac(x), mac(y)]
    }
  })

  // TODO: hacky, but it works
  setMacro("#", function () {
    var args = [].slice.call(arguments)
    //console.info([].slice.call(arguments))
    withPreviousScope(function () {
      args.forEach(function anon(x) {
        if (Array.isArray(x)) {
          x.forEach(anon)
        } else if (boxOrSym(x)) {
          getBox(x, true)
        }
      })
    })
    return ["empty"]
  })

// TODO: make , and @ into make-macro-error macros
// TODO: change if so it only accepts 0-3 arguments
/*
  var i = 0

  prn i

  if 1
    i <= 20
    i <= 30

  prn i
*/

  setMacro("&break", function () {
    statements.push(["break"])
    return ["empty"]
  })

  setMacro("throw", function (x) {
    statements.push(["throw", mac(x)])
    return ["empty"]
  })

  setMacro("&return", function (x) {
    x = mac(x)
    if (isVoid(x)) {
      if (expression) {
        statements.push(["return"])
      }
    } else {
      statements.push(["return", x])
    }
    return ["empty"]
  })

  setMacro("|", function () {
    var args = [].slice.call(arguments, 0, -1)
    args.forEach(function (x) {
      statements.push(mac(x))
    })
    if (arguments.length) {
      return mac(arguments[arguments.length - 1])
    } else {
      // TODO: put error here
      return ["empty"]
    }
  })

  setMacro("$eval", function (x) {
    return compileEval(x)
  })

  // TODO: maybe I can make this into a function instead?
  setMacro("&", function () {
    return $mac([].slice.call(arguments))
  })

  setBox("{", {
    _38_pattern: function (args, v, body) {
      args = args.slice(1)
      var index
      args.forEach(function (x, i) {
        if (Array.isArray(x) && n.isBox(x[0], "@")) {
          index = i
        }
      })
      return args.reduceRight(function (x, y, i, a) {
        if (i === index) {
          return destructure1(y[1], slicer(v, i, a.length), x)
        } else if (i > index) {
          return destructure1(y, [n.box("."), v,
                                  [n.box("-"),
                                   [n.box("."), v, "length"],
                                   a.length - i]],
                                 x)
        } else {
          //console.log(y)
          return destructure1(y, [n.box("."), v, i], x)
        }
      }, body)
    },
    _38_macro: function (a) {
      return ["array", a.slice(1).map(mac)]
    }
  })

  setMacro(".", function (x, y) {
    var s
    if ((s = validJS(y))) {
      return [".", mac(x), s]
    } else {
      return ["[]", mac(x), mac(y)]
    }
  })

  // TODO: can't be defined in NULAN.macros because it's the primitive for the " syntax
  setMacro("\"", function () {
    var a = [].slice.call(arguments)
    /*
                          // TODO: why is this needed for ~= ?
    if (a.length === 1 && (typeof a[0] === "string" || a[0] instanceof n.Wrapper && typeof a[0].value === "string")) {
      return mac(a[0])
    } else {

    }*/
    return mac([n.box("+"), ""].concat(a))
  })

  // TODO
  setBox(",", {})
  setBox("@", {})

/*
  setValue(",", macro(function (x) {
    return mac(x)
  }))

  setValue("@", macro(function (x) {
    // TODO
    throw new n.Error(x, "invalid use of @")
  }))
*/

  setMacro("null?", function (x) {
    return ["==", mac(x), ["null"]]
  })

  // TODO: even though it's an ordinary macro, I can't move it into NULAN.macros because "box" depends upon it
  setBox("=", {
    _38_pattern: function (args, v, body) {
      args = args.slice(1)
      return destructure1(args[0], [n.box("if"), [n.box("null?"), v], args[1], v], body)
    },
    _38_macro: function (args) {
      var x = args[1]
        , y = args[2]
      return mac([n.box("if"), [n.box("null?"), x], [n.box("<="), x, y]])
    }
  })

  setMacro("++", function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["++", mac(x)]
    } else {
      return ["+=", mac(x), mac(y)]
    }
  })

  setMacro("--", function (x, y) {
    if (y == null) y = 1
    if (y === 1) {
      return ["--", mac(x)]
    } else {
      return ["-=", mac(x), mac(y)]
    }
  })

  setMacro("new", function (x) {
    var args = [].slice.call(arguments, 1)
    return ["new", mac(x), args.map(mac)]
  })

  setMacro("while", function (test, body) {
    return withNewScope(function () {
      statements.push(["while", mac(test), withStatement(function () {
        return withBlock(body)
      })])
      return ["empty"]
    })
  })

  setMacro("w/new-scope", function (body) {
    // TODO
    //return mac([[n.box("->"), [], body]])
    //return ["call", ["function", "", [], [["return", mac(body)]]], []]
    return withNewScope(function () {
      return mac(body)
    })
  })

  setMacro("if", function () {
    function branch(u, x) {
      return withNewScope(function () {
        //return withStatement(function () {
        //[n.box("<="), u, x]
        return withExpression(function () {
          return withBlock(x)
        })
        //})
      })
    }

    // TODO: I'm not sure whether the then/else branches should get their own withNewScope or not
    function loop(a) {
      switch (a.length) {
      case 0:
               // TODO
        return ["empty"] //["void", ["number", "0"]]
      case 1:
        return mac(a[0])
      case 2:
      case 3:
        var x = mac(a[0])
          , y = branch(u, a[1])
          , z = (a.length === 3
                  ? branch(u, a[2])
                  : [])

        var b1 = (y.length && !isVoid(y[y.length - 1]))
          , b2 = (z.length && !isVoid(z[z.length - 1]))

        if (b1 || b2) {
          var u = new Uniq()
          statements.push(mac([n.box("box"), u]))
          if (b1) {
            y.push(["=", mac(u), y.pop()])
          }
          if (b2) {
            z.push(["=", mac(u), z.pop()])
          }
        }
        if (!b1) {
          y.pop()
        }
        if (!b2) {
          z.pop()
        }

        statements.push(["if", x, y, z ])

        if (b1 || b2) {
          return mac(u)
        } else {
          return ["empty"]
        }
      // TODO: maybe simplify this a bit?
      default:
        return loop([a[0], a[1], loop(a.slice(2))])
        /*return ["if", mac(a[0]),
                 [withNewScope(function () { return mac(a[1]) })],
                 [loop.apply(this, a.slice(2))]]*/
      }
    }

    var a = [].slice.call(arguments)
    return withNewScope(function () {
      return loop(a)
    })
  })
/*
  setMacro("re", function (x) {
    return ["regexp", x]
  })*/

  setMacro("finally", function (x, y) {
    // TODO: fix this so it works the same as the "if" macro
    var u = new Uniq()
    statements.push(["try", withStatement(function () {
                              return withBlock(mac([n.box("box"), [n.box("="), u, x]]))
                            }),
                            [],
                            withStatement(function () {
                              return withBlock(y)
                            })])
    return mac(u)
    /*var u = new Uniq()
    return ["try", [mac([values["box"], [u, x]])], ["finally", [mac(y), mac(u)]]]*/
  })


  // Medium complexity
  // TODO: make it work at compile-time
  // var foo = 5
  // $eval
  //   var foo = 10
  // $eval
  //   del foo
  // foo
  setMacro("del", function (x) {
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

  setBox("[", {
    _38_pattern: function (args, v, body) {
      args = args.slice(1)
      return args.reduceRight(function (x, y) {
        return destructure1(y[1], [n.box("."), v, y[0]], x)
      }, body)
    },
    _38_macro: function (args) {
      args = args.slice(1)
      if (args.every(function (a) { return a[0] instanceof n.Wrapper })) {
        // TODO: does this work for numeric keys?
        return ["object", args.map(function (a) {
          return [a[0].value, mac(a[1])]
        })]
      } else {
        var u = new Uniq()
        args = args.map(function (a) {
          var x = a[0]
            , y = a[1]
          /*
          // TODO: make it work with boxes too?
          if (x instanceof n.Symbol) {
            // TODO: eh, not sure if this should be in here or not
            n.tokenUpdate(x, function (o) {
              o.type = "property"
            })
            x = x.value
          }*/
          return [n.box("<="), [n.box("."), u, x], y]
        })
        return mac([n.box("|"), [n.box("box"), [n.box("="), u, [n.box("[")]]]].concat(args))
      }
    }
  })

  setBox("'", {
    _38_pattern: function (args, v, body) {
      compileOnlyError(args[0])

      var x = getBox(args[1]) // TODO: checkBox ?
      return [n.box("if"), [n.box("&box=="), v, x._38_uniqueName],
                           body,
                           [n.box("&error"), v, [n.box("+"), "expected " + args[1] + " but got ", v]]]
    },
    _38_macro: function (a) {
      var s = a[0]
        , i = 1

      // TODO: move these both outside of ' ?
      function findCommas(x, f) {
        var i2 = i
        while (Array.isArray(x) && n.isBox(x[0], ",") && i2 > 0) {
          x = x[1]
          --i2
        }
        return f(x, i2)
      }

      function withQuote(x, f) {
        if (n.isBox(x[0], "'")) {
          ++i
          try {
            x = f()
          } finally {
            --i
          }
          return x
        } else {
          return f()
        }
      }

      function loop(x) {
        if (Array.isArray(x)) {
          if (n.isBox(x[0], ",")) {
            return findCommas(x, function (y, i) {
              if (i === 0) {
                if (Array.isArray(y) && n.isBox(y[0], "@")) {
                                    // TODO: use enrich on this
                                    // enrich(s, y[0])
                  throw new n.Error({}, "',@ is invalid")
                } else {
                  y = mac(y)
                }
              } else {
                y = loop(y)
              }

              var b = loop(x[0])
              while (i > 0) {
                y = ["array", [b, y]]
                --i
              }
              return y
            })
          } else {
            /*return ["array", x.map(function (x) {
              if (Array.isArray(x) && n.isBox(x[0], ",")) {
                return findCommas(x, function (x, i) {
                  var y
                  if (Array.isArray(x) && n.isBox(x[0], "@")) {

                  } else {

                  }
                  console.log(x, i)
                })
              } else {
                return loop(x)
              }
            })]*/
            return withQuote(x, function () {
              // TODO: a little bit hacky, but I don't really know of a better alternative
              return arraySplitter(x, function (x) {
                return findCommas(x, function (x, i) {
                  if (i === 0 && Array.isArray(x) && n.isBox(x[0], "@")) {
                    return x[1]
                  }
                })
              }, function (x, b) {
                if (b) {
                  return mac(x)
                } else {
                  return loop(x)
                }
              }, function (x, b) {
                return b ? x : ["array", x]
              })
            })
          }
        } else if (wildcard(x)) {
          // TODO: a little hacky
          return mac([n.box("sym"), x.value])
        } else if (boxOrSym(x)) {
          return macBox(x)
        } else {
          // TODO
          return mac(x)
        }
      }

      compileOnlyError(s)
      return loop(a[1])
    }
  })

  setMacro("external", function () {
    [].forEach.call(arguments, function (x) {
      tokenBox(x, setSymExternal(x))
      //setSymToBox(x) // TODO: is this correct?
      //setNewBox(x)
                       // (&eval '(&list 1 2 3))
                       // (include &list)
                       // (&eval '(&list 1 2 3))
    })
    return ["empty"]
  })

  setMacro("builtin", function () {
    [].forEach.call(arguments, function (x) {
                     // TODO: meh
      tokenBox(x, setBuiltin(x.value, x.value))
    })
    return ["empty"]
  })


  // Heavy complexity
  setMacro("->", function (args, body) {
    if (Array.isArray(args)) {
      return withLocalScope(function () {
        return withNewScope(function () {
  /*
          var slice = Function.prototype.call.bind([].slice)
          slice(arguments)
  */
          var x, u, s, r = []

          setSymExternal(new n.Symbol("this"))

          for (var i = 0, iLen = args.length; i < iLen; ++i) {
            x = args[i]

            if (boxOrSym(x) && !wildcard(x)) {
              x = setSymToBox(x)
              r.push(x._38_uniqueName)

            } else if (Array.isArray(x) && n.isBox(x[0], "@")) {
              s = setSymExternal(new n.Symbol("arguments"))
              //s = new n.Box("arguments") // TODO: ew
              //s._38_scope = "local" // TODO: ew

              u = (i !== iLen - 1
                    ? new Uniq()
                    : s)

              ;(function (i2) {
                var x
                while (i2 > i) {
                  x = args[i2]
                                        // TODO: code duplication with the { pattern
                  body = destructure(x, [n.box("."), u,
                                          [n.box("-"),
                                           [n.box("."), u, "length"],
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
              u = new Uniq()
              u = setSymToBox(u) // TODO: this is called only for uniqize, is that correct?
              //uniqize(u)
              //setVar(u) // TODO: does this need to be added to vars?
              r.push(u._38_uniqueName)
              body = destructure(x, u, body)
            }
          }

          return ["function", "", r, withStatement(function () {
            return withBlock([n.box("&return"), body])
          })]
        })
      })
    } else {
      throw new n.Error(args, "invalid function argument list: " + args)
    }
  })

  setMacro("box", function () {
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

    var lastSym

    args = args.map(function (x) {
      var y
      if (Array.isArray(x)) {
        // TODO: use isSym ?
        if (!n.isBox(x[0], "=")) {
          throw new n.Error(x[0], "expected = but got " + x[0])
        }
        y = x[2]
        x = x[1]
      }
      // TODO: should return the result of this
      if (wildcard(x)) {
        return mac(y)
      } else if (boxOrSym(x)) {
        if (y !== void 0) {
          y = mac(y)
        }

        lastSym = x = setSymToBox(x)

            // TODO: !x.local
        if (x._38_scope !== "local" && mode === "compile") {
          if (y === void 0) {
            return ["empty"] // TODO
          } else {
            return ["=", [".", macBox(x), "_38_value"], y]
            //return ["=", [".", [".", ["name", "n"], "values"], x._uniqueName], y]
          }
        } else {
          if (y === void 0) {
            return ["var", [[x._38_uniqueName]]]
          } else {
            return ["var", [[x._38_uniqueName, y]]]
          }
        }
      } else {
        // TODO: code duplication with destructure1
        if (complex(y)) {
          console.info(y)
          var u = new Uniq()
          return mac([n.box("|"), [n.box("box"), [n.box("="), u, y]], destructure(x, u)])
        } else {
          return mac(destructure(x, y))
        }
      }
    })

    args.forEach(function (x) {
      statements.push(x)
    })

    if (lastSym === void 0) {
      return ["empty"]
    } else {
      return mac(lastSym)
    }
/*
    return .reduce(function (x, y) {
      return [",", x, y]
    })*/
  })

/*
  // Non-primitives, but it's easier to define them in here than in NULAN.macros
  setValue("$run", macro(function (x) {
    return mac([n.box("$eval"), [n.box("|"), x, []]])
  }))

  setValue("w/var", macro(function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    return mac([n.box("w/new-scope"), [n.box("|"), [n.box("box")].concat(args), body]])
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
  /*setValue("w/var", function () {
    var args = [].slice.call(arguments, 0, -1)
      , body = arguments[arguments.length - 1]

    var x = args.map(function (x) { return x[0] })
      , y = args.map(function (x) { return x[1] })

    return mac([[n.box("->"), x, body]].concat(y))
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

  n.external = function () {
    [].forEach.call(arguments, function (x) {
      setSymExternal(new n.Symbol(x))
      //setSymToBox(new n.Symbol(x)) // TODO is this correct?
      //setNewBox(new n.Symbol(x))
    })
  }

  n.builtin = function (x, y) {
    setBuiltin(x, y)
    /*Object.keys(o).forEach(function (x) {
      setValue(x, o[x])
    })*/
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

  n.builtins = function () {
    [].forEach.call(arguments, function (x) {
      n.builtin(x, x)
    })
  }

  // TODO: make it possible to have a variable at both runtime and compiletime
  //n.include("console", "Array")
  //n.builtin({ "console": console, "Array": Array, "Object": Object })

  // Object.getOwnPropertyNames(global)
  /*var n = {
    builtin: function () {
      console.log([].filter.call(arguments, function (x) {
        return (x in window)
      }))
      console.log([].filter.call(arguments, function (x) {
        return !(x in window)
      }))
    }
  }*/

  // Globals that exist in all environments
  n.builtins("Number", "Math", "Boolean", "TypeError", "String",
             "Int16Array", "Float32Array", "isFinite", "Array", "DataView",
             "Float64Array", "ReferenceError", "SyntaxError", "Int32Array",
             "Uint16Array", "clearTimeout", "decodeURIComponent",
             "Uint32Array", "setTimeout", "eval", "console", "URIError",
             "unescape", "Date", "escape", "encodeURI", "Error",
             "Int8Array", "EvalError", "RangeError", "NaN", "isNaN",
             "parseInt", "undefined", "Object", "Uint8ClampedArray",
             "parseFloat", "Uint8Array", "clearInterval", "Infinity",
             "JSON", "Function", "setInterval", "encodeURIComponent",
             "decodeURI", "ArrayBuffer", "RegExp")

  n.builtins("this")

  // Modes for a specific environment
  n.modes = {
    "Node.js": function (f) {
      f("Buffer", "global", "GLOBAL", "process", "root", "require", "module")
    },
    "browser": function (f) {
      f("window", "document")
    },
    "Chrome Extension": function (f) {
      n.modes["browser"](f)
      f("chrome")
    }
  }

  n.from = function (from) {
    return {
      to: function (to) {
        if (from === to) {
          n.modes[from](n.builtins)
        } else {
          n.modes[from](n.external)
          withMode("run", function () {
            n.modes[to](n.external)
          })
        }
      }
    }
  }
/*
  n.import = function () {
    [].forEach.call(arguments, function (s) {
      n.eval(n.readFile(s))
    })
  }*/

  n.eval = function (s, f) {
    var r = []
    n.parse(s, function (err, x) {
      if (err) {
        //console.trace("" + err)
        throw err
      }
      r.push(n.compile(x))
    })
    r = r.join(";\n\n")
    if (f) {
      f(r)
    } else {
      // TODO: should be global eval, maybe?
      //       or at least it should have access to values and boxes, right?
      //       or does it need access to those things?
      eval(r)
      //require("vm").runInNewContext(r, global)
    }
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
      return compile(a)
    })
  }

  n.options = {
    debug: false
  }

  return n
})(NULAN || {})
