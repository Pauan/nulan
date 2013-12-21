// TODO proper "isComplex" function
define(["./options", "./print", "./0 data/box", "./0 data/symbol", "./0 data/string", "./0 data/number", "./0 data/array", "./0 data/macex", "./2 macex/macex"], function (options, a, b, c, d, e, f, g, h) {
  // tokenize, compile, state, module
  // data macex
  "use strict";

  var error     = a.error

    , Box       = b.Box
    , toBox     = b.toBox
    , setBox    = b.setBox
    , checkBox  = b.checkBox
    , isBox     = b.isBox

    , Symbol    = c.Symbol
    , String    = d.String
    , Number    = e.Number
    , unwrap    = f.unwrap
    
    , isGet     = g.isGet
    , isSet     = g.isSet
    , isMacro   = g.isMacro
    , isPattern = g.isPattern
    , isSyntax  = g.isSyntax
    , mode      = g.mode

    , macex     = h.macex

  function withNewScope(f) {
    return state.vars.push({}, f)
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
  
  function missingLeft(s) {
    error(s, "missing expression on the left side of ", [s])
  }
  
  function missingRight(s) {
    error(s, "missing expression on the right side of ", [s])
  }

  function unary(o) {
    if (o == null) {
      o = {}
    }
    if (o.associativity == null) {
      o.associativity = "right"
    }
    if (o.parse == null) {
      o.parse = function (l, s, r) {
        if (r.length === 0) {
          missingRight(s)
        } else {
          var y = r[0]
          // TODO is this correct ?
          if (o.indent) {
            return l.concat([[s, unwrap(y)]], r.slice(1))
          } else {
            return l.concat([[s, y]], r.slice(1))
          }
        }
      }
    }
    return o
  }

  function infix(o) {
    if (o == null) {
      o = {}
    }
    if (o.parse == null) {
      o.parse = function (l, s, r) {
        var y = r[0]
        if (l.length === 0) {
          missingLeft(s)
          //return [[s, y]].concat(r.slice(1))
        } else if (r.length === 0) {
          missingRight(s)
        } else {
          var x = l[l.length - 1]
          return l.slice(0, -1).concat([[s, x, y]], r.slice(1))
        }
      }
    }
    return o
  }

  function inert(start, end) {
    set(end, function (o) {
      o[isSyntax] = {
        delimiter: true,
        startAt: start
        /*parse: function (l, s, r) {
          error(s, "missing starting " + start)
        }*/
      }
    })
  }
  
  function whitespace(x) {
    if (x == null) {
      x = {}
    }
    if (x.delimiter == null) {
      x.delimiter = true
    }
    if (x.whitespace == null) {
      x.whitespace = true
    }
    if (x.tokenize == null) {
      x.tokenize = function (o) {
        o.read()
        return []
      }
    }
    return x
  }

  
  var vars = {}

  function set(sName, f) {
    var o = new Box(sName)
    f(o)
    vars[sName] = o
  }
  
  function get(s) {
    // TODO console.assert
    if (!(s in vars)) {
      throw new Error("builtin \"" + s + "\" does not exist")
    }
    //console.assert(s in vars)
    return vars[s]
  }

  function plural(i) {
    if (i === 1) {
      return ""
    } else {
      return "s"
    }
  }
  
  function checkArguments(a, i) {
    if (a.length - 1 !== i) {
      error(a[0], "expected " + i + " argument" + plural(i) + " but got " + (a.length - 1))
    }
  }
  
  function checkArgumentsl(a, i) {
    if (a.length - 1 < i) {
      error(a[0], "expected at least " + i + " argument" + plural(i) + " but got " + (a.length - 1))
    }
  }
  
  function op(s, loc, args) {
    var o = new data.Op(s, args)
    o.loc = loc.loc
    return o
  }
  
  function opargs(s, i) {
    return function (a) {
      checkArguments(a, i)
      return op(s, a[0], a.slice(1).map(macex))
    }
  }
  
  function opargsl(s, i) {
    return function (a) {
      checkArgumentsl(a, i)
      return op(s, a[0], a.slice(1).map(macex))
    }
  }
  
  function patternMatch1(k, x, val, info) {
    return new MacexBypass(k[isPattern]([x[0], x.slice(1), val, info]))
  }
  
  // TODO so much MacexBypass ...
  function patternMatch(x, val, info) {
    if (Array.isArray(x)) {
      var k = toBox(x[0])
      console.assert(k instanceof Box)
      if (isPattern in k) {
        // TODO isComplex
        if (Array.isArray(val)) {
          var u = new Box()
                            // TODO is this correct ?
          return [get("|"), patternMatch(u, val, { wrapVar: true }),
                            // TODO code duplication
                            patternMatch1(k, x, u, info)]
        } else {
          return patternMatch1(k, x, val, info)
        }
      } else {
        error(x[0], [x[0]], " is not a pattern")
      }
    } else {
      var k = (info.wrapVar
                ? setBox(x)
                : toBox(x))
      if (k instanceof Box && isSet in k) {
        return new MacexBypass(k[isSet]([x, val]))
      } else {
        if (mode.get() === "compile" && !k.local) {
          if (val == null) {
            return new MacexBypass(op("empty", x, []))
          } else {
            return new MacexBypass(op("=", x, [macex.compileBoxValue(k), macex(val)]))
          }
        } else {
          if (val == null) {
            return new MacexBypass(op("var", x, [k])) // TODO compile(k) ?
          } else {
            if (info.wrapVar) {
              return new MacexBypass(op("var", x, [op("=", x, [k, macex(val)])]))
            } else {
              return new MacexBypass(op("=", x, [k, macex(val)]))
            }
          }
        }
      }
    }
  }


  set("/", function (o) {
    o[isMacro]  = opargs("/", 2)
    o[isSyntax] = infix({ priority: 70 })
  })
  set("*", function (o) {
    o[isMacro]  = opargs("*", 2)
    o[isSyntax] = infix({ priority: 70 })
  })
  set("+", function (o) {
    o[isMacro]  = opargs("+", 2)
    o[isSyntax] = infix({ priority: 60 })
  })
  set("-", function (o) {
    o[isMacro]  = opargs("-", 2)
    o[isSyntax] = infix({ priority: 60 })
  })
  set("<", function (o) {
    o[isMacro]  = opargs("<", 2)
    o[isSyntax] = infix({ priority: 50 })
  })
  set("=<", function (o) {
    o[isMacro]  = opargs("<=", 2)
    o[isSyntax] = infix({ priority: 50 })
  })
  set(">", function (o) {
    o[isMacro]  = opargs(">", 2)
    o[isSyntax] = infix({ priority: 50 })
  })
  set(">=", function (o) {
    o[isMacro]  = opargs(">=", 2)
    o[isSyntax] = infix({ priority: 50 })
  })
  set("==", function (o) {
    o[isMacro]  = opargs("===", 2)
    o[isSyntax] = infix({ priority: 40 })
  })
  set("|=", function (o) {
    o[isSyntax] = infix({ priority: 40 })
  })
  set("~=", function (o) {
    o[isMacro]  = opargs("!==", 2)
    o[isSyntax] = infix({ priority: 40 })
  })
  set("&&", function (o) {
    o[isMacro]  = opargs("&&", 2)
    o[isSyntax] = infix({ priority: 30 })
  })
  set("||", function (o) {
    o[isMacro]  = opargs("||", 2)
    o[isSyntax] = infix({ priority: 20 })
  })
  set("num", function (o) {
    o[isMacro]  = opargs("+", 1)
  })
  set("sub", function (o) {
    o[isMacro] = opargs("-", 1)
  })
  set("mod", function (o) {
    o[isMacro] = opargs("%", 1)
  })
  set("new", function (o) {
    o[isMacro] = opargsl("new", 1)
  })

  inert("(", ")")
  inert("[", "]")
  inert("{", "}")
  inert("#|", "|#")
  
  set(" ", function (o) {
    o[isSyntax] = whitespace()
  })

  set("\n", function (o) {
    o[isSyntax] = whitespace()
    /* {
      //priority: Infinity,
      tokenize: function (o) {
        while (o.has() && (o.peek() === " " || o.peek() === "\n")) {
          o.read()
        }
        var s = o.position()
        var x = new data.Symbol("\n")
        x.loc = o.loc(s, o.position())
        return [x]
      },
      parse: function (l, s, r) {
        return l.concat(r)
      }
    } */
  })
  
  set("~", function (o) {
    o[isMacro]  = opargs("!", 1)
    o[isSyntax] = {
      priority: 80,
      associativity: "right",
      // TODO handle escapes with \ as well
      tokenize: function (o) {
        var s = o.position()
          , a = [o.read()]
        if (o.peek() === "=") {
          a.push(o.read())
        }
        var x = new Symbol(a.join(""))
        x.loc = o.loc(s, o.position())
        return [x]
      },
      parse: function (l, s, r) {
        if (r.length === 0) {
          missingRight(s)
        } else {
          var y = r[0]
          return l.concat([[s, y]], r.slice(1))
        }
      }
    }
  })
  
  /*set("->macro", {
    function: function (f) {
      return function (a) {
        var args = a.slice(1)
        return macex(f.apply(this, args))
      }
    }
  })*/
  
  function compileEval(x) {
    return mode.set("compile", function () {
      x = compile.compile(compile.expression([macex(x)]))
      options.$eval(x)
      if (typeof options.eval !== "function") {
        throw new Error("Nulan requires `options::eval` to be set to a function")
      }
      return options.eval(x)
    })
  }
  
  function makeBoxSetter(s) {
    return function (a) {
      checkArguments(a, 2)
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
  
  set("$pattern!", function (o) {
    o[isMacro] = makeBoxSetter(isPattern)
  })
  
  set("$syntax!", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 2)
      // TODO destructuring ?
      var x = toBox(a[1])
        , f = compileEval(a[2])
      x[isSyntax] = f
      return macex([])
    }
  })

  // TODO expose syntax-unary function and move this into core  
  set("$syntax-unary!", function (o) {
    o[isMacro] = function (a) {
      // TODO checkArguments
      // TODO destructuring ?
      var x = toBox(a[1])
        , f = compileEval(a[1])
      x[isSyntax] = unary(f)
      return macex([])
    }
  })
  
  set("$syntax-infix!", function (o) {
    o[isMacro] = function (a) {
      // TODO checkArguments
      // TODO destructuring ?
      var x = toBox(a[1])
        , f = compileEval(a[2])
      x[isSyntax] = infix(f)
      return macex([])
    }
  })

  set("$mac!", function (o) {
    o[isMacro] = makeBoxSetter(isMacro)
  })
  set("$get!", function (o) {
    o[isMacro] = makeBoxSetter(isGet)
  })
  set("$set!", function (o) {
    o[isMacro] = makeBoxSetter(isSet)
  })
  
  set("$eval", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 1)
      return macex(compileEval(a[1]))
    }
  })
  
  set("$import", function (o) {
    // TODO generic
    function checkSym(x) {
      if (!(x instanceof Symbol)) {
        error(x, "expected symbol but got ", [x])
      }
    }

    o[isMacro] = function (a) {
      if (state.module.has()) {
        var m = state.module.get()
        // TODO "iter" module ?
        a.slice(1).forEach(function (x) {
          if (Array.isArray(x)) {
            checkBox(x[0], get("="))
            return (function () {
              var s = compileEval(x[2])
                , m = module.fromPath(s)
              
              var k = x[1]
              if (Array.isArray(k)) {
                checkBox(k[0], get("{"))
                k.slice(1).forEach(function (k) {
                  var x, y
                  if (Array.isArray(k)) {
                    checkBox(k[0], get("="))
                    x = k[1]
                    y = k[2]
                  } else {
                    x = k
                    y = k
                  }
                  checkSym(x)
                  checkSym(y)
                  state.vars.set(x.value, module.getBox(m, s, y))
                })
              } else {
                checkSym(k)
                k = setBox(k)
                module.importBox(m, k)
                m.assigns.push(op("var", k, [op("=", k, m.exportBox)]))
              }
            })()
          } else {
            error(x, "expected (... = \"...\") but got ", [x])
          }
        })
        return macex([])
      } else {
        error(a[0], [a[0]], " can only be used inside of a module")
      }
    }
  })
  
  set("<=", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 2)
      return macex(patternMatch(a[1], a[2], { wrapVar: false }))
    }
    o[isSyntax] = {
      associativity: "right",
      //indent: "left",
      parse: function (l, s, r) {
        if (l.length === 0) {
          missingLeft(s)
        } else if (r.length === 0) {
          missingRight(s)
        } else {
          return [[s, unwrap(l), unwrap(r)]]
        }
      }
    }
  })
  
  set("prn", function (o) {
    o[isGet] = function (a) {
      return op(".", a[0], [new Symbol("console"), new String("log")])
    }
  })

  set("vars", function (o) {
    o[isMacro] = function (a) {
      var after = []
      // TODO "iter" module ?
      a.slice(1).forEach(function anon(x) {
        if (Array.isArray(x)) {
          checkBox(x[0], get("="))
          // TODO get rid of this somehow
          var u
          // TODO isComplex ?
          if (Array.isArray(x[1]) && Array.isArray(x[2])) {
            u = new Box()
            after.push(function () {
              return op("var", x[0], [op("=", x[0], [u, macex(x[2])])])
            })
          } else {
            u = new MacexBypass(macex(x[2]))
          }
          after.push(function () {
            return macex(patternMatch(x[1], u, { wrapVar: true }))
          })
        } else {
          after.push(function () {
            return macex(patternMatch(x, null, { wrapVar: true }))
          })
        }
      })
      return op(",", a[0], after.map(function (f) { return f() }))
    }
  })
  
  set("|", function (o) {
    o[isMacro]  = opargsl(",", 1)
    o[isSyntax] = {
      indent: "right",
      vertical: true,
      priority: Infinity,
      parse: function (l, s, r) {
        /*if (r[0].length === 0) {
          missingRight(s)
        }*/
        return l.concat([[s].concat(r[0])], r.slice(1))
        
        /*
        // TODO a teensy bit hacky
        if (r[0].length === 1 && r[0][0].length === 0) {
          return l.concat([[s]], r.slice(1))
        } else {
          return l.concat([[s].concat(r[0].map(data.unwrap))], r.slice(1))
        }*/
      }
    }
  })
  
  function slicer(val, i, len) {
    var s = [[get("."), [get("."), [get("[")], "slice"], "call"], val]
      , n = i - len
    if (n !== 0) {
      s.push(i)
      s.push(n)
    } else if (i !== 0) {
      s.push(i)
    }
    return s
  }
  
  function slicerLast(val, i, len) {
    return [get("."), val, [get("-"), [get("."), val, "length"], (len + 1) - i]]
  }

  set("[", function (o) {
    o[isPattern] = function (a) {
      var args = a[1]
        , val  = a[2]
        , info = a[3]

      var r    = []
        , seen = false
        , len  = args.length - 1
      args.forEach(function (x, i) {
        if (Array.isArray(x) && isBox(x[0], get("@"))) {
          if (seen) {
            error(x[0], "[] pattern cannot have more than one @")
          } else {
            seen = true
            r.push(patternMatch(x[1], slicer(val, i, len), info))
          }
        } else {
          if (seen) {
            r.push(patternMatch(x, slicerLast(val, i, len), info))
          } else {
            r.push(patternMatch(x, [get("."), val, i], info))
          }
        }
      })

      return macex([get("|")].concat(r))
    }
    o[isMacro] = function (a) {
      var left  = []
        , right = []
      // TODO use "iter" module ?
      a.slice(1).forEach(function (x) {
        if (Array.isArray(x) && isBox(x[0], get("@"))) {
          right.push(macex(x[1]))
        } else {
          if (right.length) {
            right.push(new data.Op("array", [macex(x)]))
          } else {
            left.push(macex(x))
          }
        }
      })
      if (right.length) {
        return op("call", a[0], [op(".", a[0], [op("array", a[0], left),
                                                new String("concat")])].concat(right))
      } else {
        return op("array", a[0], left)
      }
    }
    o[isSyntax] = {
      delimiter: true,
      priority: Infinity,
      endAt: "]",
      parse: function (l, s, r) {
        if (s.whitespace) {
          return l.concat([[s].concat(r[0])], r.slice(1))
        } else {
          if (l.length === 0) {
            missingLeft(s)
          }
          var x = new Symbol(".") // TODO use a box for this
          x.loc = s.loc
          return l.slice(0, -1).concat([[x, l[l.length - 1], unwrap(r[0])]], r.slice(1)) // TODO should this unwrap ?
        }
      }
    }
  })
  
  set("true", function (o) {}) // TODO

  set("(", function (o) {
    o[isSyntax] = {
      delimiter: true,
      priority: Infinity,
      endAt: ")",
      indent: "right",
      parse: function (l, s, r) {
        return l.concat([unwrap(r[0])], r.slice(1))
      }
    }
  })
  
  set("{", function (o) {
    o[isPattern] = function (a) {
      var args = a[1]
        , val  = a[2]
        , info = a[3]

      var r = []
      args.forEach(function (x) {
        if (Array.isArray(x)) {
          checkBox(x[0], get("="))
          r.push(patternMatch(x[1], [get("."), val, toString(x[2])], info))
        } else {
                                                 // TODO should work with boxes ?
          r.push(patternMatch(x, [get("."), val, toString(x)], info))
        }
      })
        
      return macex([get("|")].concat(r))
    }
    o[isMacro] = function (a) {
      return op("object", a[0], a.slice(1).map(function (x) {
        if (Array.isArray(x)) {
          checkBox(x[0], get("="))
          return op("=", x[0], [toString(x[1])].concat(x.slice(2).map(macex)))
        } else {
          return macex(x)
        }
      }))
    }
    o[isSyntax] = {
      delimiter: true,
      priority: Infinity,
      endAt: "}",
      //indent: "right",
      parse: function (l, s, r) {
        /*y.forEach(function (x) {
          if (x.length > 2) {
            for (var i = 0, iLen = x.length; i < iLen; ++i) {
              // TODO check this
              if (i + 1 < iLen) {
                a.push([toString(x[i]), x[i + 1]])
              } else {
                a.push([toString(x[i])])
              }
            }
          } else {
            // TODO check this
            if (x.length > 1) {
              a.push([toString(x[0]), x[1]])
            } else {
              a.push([toString(x[0])])
            }
          }
        })*/

        /*for (var i = 0, iLen = y.length; i < iLen; ++i) {
          a2 = []
          if (y[i] instanceof data.Symbol) {
            a2.push(tokenize.enrich(new data.String(y[i].value), y[i].start, y[i].end))
          } else {
            a2.push(y[i])
          }
          ++i
          console.log(y[i])
          if (Array.isArray(y[i]) && !isSym(y[i][0], "=")) {
            ++i
          }
          a2.push(y[i])
          a.push(a2)
        }*/
        return l.concat([[s].concat(r[0])], r.slice(1))
        //return l.concat([a], r.slice(1))
      }
    }
  })
  
  set("'", function (o) {
    o[isSyntax] = {
      delimiter: true,
      tokenize: tokenize.string,
      priority: Infinity,
      endAt: "'",
      parse: function (l, s, r) {
        var a = r[0]
        var every = a.every(function (x) {
          return x instanceof String
        })
        if (every) {
          var x = new Symbol(a.map(function (x) { return x.value }).join(""))
          x.loc = s.loc
          return l.concat([x], r.slice(1))
        } else {
          error(s, "cannot use @ with '")
        }
      }
    }
  })
  
  set("\"", function (o) {
    o[isMacro] = function (a) {
      var r = a.slice(1).map(function (x) {
        return macex(x)
      })
      var every = r.every(function (x) {
        return x instanceof String
      })
      if (every) {
        r = r.map(function (x) {
          return x.value
        })
        var s = new String(r.join(""))
        s.loc = a[0].loc
        return s
      } else {
        if (!(r[0] instanceof String)) {
          var s = new String("")
          s.loc = a[0].loc
          // TODO inefficient ?
          r.unshift(s)
        }
        return r.reduce(function (x, y) {
          return op("+", a[0], [x, y])
        })
      }
    }
    o[isSyntax] = {
      delimiter: true,
      tokenize: tokenize.string,
      priority: Infinity,
      endAt: "\"",
      parse: function (l, s, r) {
        return l.concat([[s].concat(r[0])], r.slice(1))
      }
    }
  })

  set(";", function (o) {
    o[isSyntax] = {
      delimiter: true,
      priority: 90,
      parse: function (l, s, r) {
        if (l.length === 0) {
          missingLeft(s)
        } else {
          return l.slice(0, -1).concat([[l[l.length - 1]]], r)
        }
      }
    }
  })

  set(":", function (o) {
    o[isSyntax] = {
      delimiter: true,
      priority: 90, // TODO: does this need to be 90?
      indent: "right",
      parse: function (l, s, r) {
        if (r[0].length === 0) {
          missingRight(s)
        } else {
          return l.concat([r[0]], r.slice(1))
        }
      }
    }
  })
  
  // TODO: update "Customizable syntax.rst" with the new definition of "."
  set(".", function (o) {
    o[isMacro]  = opargs(".", 2)
    o[isSyntax] = {
      delimiter: true,
      priority: 90,
      parse: function (l, s, r) {
        if (l.length === 0) {
          missingLeft(s)
        } else if (r.length === 0) {
          missingRight(s)
        } else {
          var x = l[l.length - 1]
            , y = r[0]

          if (x instanceof Number && y instanceof Number) {
            var i = (x.value + "." + y.value)
            s = new Number(+i)
            s.loc = data.loc(x.loc, y.loc)
          } else {
            s = [s, x, toString(y)]
          }

          return l.slice(0, -1).concat([s], r.slice(1))
        }
      }
    }
  })

  set(",", function (o) {
    o[isSyntax] = unary({
      priority: 80,
      delimiter: true
    })
  })

  set("@", function (o) {
    o[isSyntax] = unary({
      priority: 80,
      delimiter: true
    })
  })

/*
  vars foo = 1
  $eval
    `foo + 1

  `1 (2 + 2) 3
  
  ['[', 1, ['[', '+', 2, 2], 3]
  

  `1 ,(2 + 2) 3
  
  ['[', 1, 2 + 2, 3]


  `1 ,@2 3
  
  ['[', 1, ['@', 2], 3]
  
  
  `1 2
     `3 4
  
  ['[', 1, 2, ['[', '`', 3, 4]]
  
  
  `1 2
     `,3 ,,4
  
  ['[', 1, 2, ['[', '`', ['[', ',', 3], 4]]
  
  
  `1 2
     `,2 ,@4
  
  ['[', 1, 2, ['[', '`', ['[', ',', 2], ['[', ',', ['[', '@', 4]]]]


  `1 2
     `,2 ,@,4
  
  ['[', 1, 2, ['[', '`', ['[', ',', 2], ['@', 4]]]
  
  
  `1 2
     `2 3
        `,,4
  
  ['[', 1, 2, ['[', '`', 2, 3, ['[', '`'`, [',', 4]]]]
  
  
  `1 2
     ``,2 ,,@,4
  
  ['[' 1 2 ['[' ` ['[' ` ['[' , 2] ['[' , ['[' @ 4]]]]]
  
  
  `1 2
     ` `,2 ,,@,4
  
  `1 2
     ` `,2 ,,,@4
  
  $eval
    `1 2
       `,2 ,@,[4 + 5]
  
  $eval
    `1 2
       `,2 ,@(4 + 5)
*/
  
  var quasiquote = (function () {
    function anon(x, depth, i) {
      if (Array.isArray(x)) {
        if (isBox(x[0], get("`"))) {
          return [get("[")].concat(x.map(function (x) {
            return anon(x, depth + 1, i)
          }))
        } else if (isBox(x[0], get(","))) {
          if (i === depth) {
            return x[1]
          } else {
            return [get("[")].concat(x.map(function (x) {
              return anon(x, depth, i + 1)
            }))
          }
        } else {
          return [get("[")].concat(x.map(function (x) {
            return anon(x, depth, i)
          }))
        }
      } else if (x instanceof Box) {
        // TODO if I used the . box I can get rid of MacexBypass ?
        return new MacexBypass(macex.compileBox(x))
      } else if (x instanceof Symbol) {
        return anon(toBox(x), depth, i)
      } else {
        return x
      }
    }

    return function (x) {
      var first = x
        , next  = first
      if (Array.isArray(next) && isBox(next[0], get(","))) {
        next = next[1]
        if (Array.isArray(next) && isBox(next[0], get("@"))) {
          error({ loc: data.loc(first[0].loc, next[0].loc) },
                ",@ cannot be used immediately after `")
        }
      }
      return anon(x, 1, 1)
    }
  })()
      
  set("`", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 1)
      compileOnlyError(a[0])
      return macex(quasiquote(a[1]))
    }
    o[isSyntax] = {
      whitespace: true,
      delimiter: true,
      priority: 80, // TODO: 10
      indent: "right",
      parse: function (l, s, r) {
        if (r[0].length === 0) {
          missingRight(s)
        } else {
          return l.concat([[s, unwrap(r[0])]], r.slice(1))
        }
      }
    }
  })
  
  set("_", function (o) {
    o[isPattern] = function (a) {
      return macex(a[2])
    }
    o[isSyntax] = {
      priority: Infinity,
      parse: function (l, s, r) {
        return l.concat([[s]], r)
      }
    }
  })
  
  function bypassSym(x) {
    return new MacexBypass(new Symbol(x))
  }

  set("->", function (o) {
    o[isMacro] = function (a) {
      var args = []
        , body = []
        , seen = false
        , len  = a[1].length - 1
      // TODO local
      a[1].forEach(function (x, i) {
        if (Array.isArray(x)) {
          if (isBox(x[0], get("@"))) {
            if (seen !== false) {
              error(x[0], "-> cannot have more than one @")
            } else {
              console.log(i, len)
              if (i === len) {
                seen = bypassSym("arguments")
              } else {
                seen = new Box()
                body.push(macex(patternMatch(seen, bypassSym("arguments"), { wrapVar: true })))
              }
              body.push(macex(patternMatch(x[1], slicer(seen, i, len), { wrapVar: true })))
            }
          } else {
            var u = new Box()
            if (seen !== false) {
              u = slicerLast(seen, i, len)
            } else {
              u = new Box()
              args.push(u)
            }
            body.push(macex(patternMatch(x, u, { wrapVar: true })))
          }
        } else {
          if (seen !== false) {
            body.push(macex(patternMatch(x, slicerLast(seen, i, len), { wrapVar: true })))
          } else {
            args.push(setBox(x))
          }
        }
      })
      body.push(op("return", a[0], [macex(a[2])]))
      return op("function", a[0], [op(",", a[0], args), op(",", a[0], body)])
    }
    o[isSyntax] = {
      priority: 10,
      associativity: "right",
      //indent: "left",
      parse: function (l, s, r) {
        var args = r.slice(0, -1)
        if (r.length === 0) {
          return l.concat([[s, args, []]])
        } else {
          return l.concat([[s, args, r[r.length - 1]]])
        }
      }
    }
  })
  
  set("null?", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 1)
      return op("==", a[0], [macex(a[1]), op("null", a[0], [])])
    }
  })
  
  set("if", function (o) {
    o[isMacro] = function (a) {
      //checkArguments(a, 1, 3) // TODO
      return withNewScope(function () {
        var test = macex(a[1])
          , yes  = withNewScope(function () { return macex(a[2]) })
          , no   = withNewScope(function () { return macex(a[3]) })
        return op("if", a[0], [test, yes, no])
      })
    }
  })

  set("w/new-scope", function (o) {
    o[isMacro] = function (a) {
      checkArguments(a, 1)
      return withNewScope(function () {
        return macex(a[1])
      })
    }
  })

  // TODO change this so it doesn't use `indent: "right"` anymore ?
  set("=", function (o) {
    o[isPattern] = function (a) {
      var args = a[1]
        , val  = a[2]
        , info = a[3]
      return macex(patternMatch(args[0], [get("if"), [get("null?"), val], args[1], val], info))
    }
    o[isSyntax] = {
      //priority: 100, // TODO why is this priority 10 ?
      indent: "left",
      parse: function (l, s, r) {
        if (l.length === 0) {
          missingLeft(s)
        } else if (r[0].length === 0) {
          missingRight(s)
        } else {
          var x = l[l.length - 1]
          return l.slice(0, -1).concat([[s, x, r[0]]], r.slice(1))
        }
      }
    }
  })

  /*set("\\", {
    syntax: {
      delimiter: true,
      // TODO
      tokenize: function (o, info) {
        o.read()
        return [new data.ParseBypass(tokenize.one(o, info))]
      }
    }
  })*/
  
  set("#", function (o) {
    o[isSyntax] = {
      delimiter: true,
      whitespace: true,
      tokenize: tokenize.comment,
      //priority: 9001,
      //associativity: "right",
      
      /*endAt: "|#", // TODO: hacky, but it works
      // TODO: hacky, but it works
      parse: function (l, s, r) {
        l.push([s].concat(r[0]))
        return l.concat(r.slice(1))
      }*/
    }
  })

  /*rules.set("`", {
    delimiter: true
  })*/

  return {
    vars: vars,
    unary: unary,
    infix: infix,
    whitespace: whitespace,
  }
})
