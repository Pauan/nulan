define(["./box", "./data", "./macex", "./tokenize", "./compile", "./options", "./eval", "./error"], function (box, data, macex, tokenize, compile, options, nulanEval, error) {
  "use strict";

  // TODO move this stuff somewhere else 
  var mode = "run"

  function withMode(x, f) {
    var old = mode
    mode = x
    try {
      return f()
    } finally {
      mode = old
    }
  }
  
  function compileOnlyError(x) {
    if (mode !== "compile") {
      error(x, "cannot use ", [x], " at " + mode + " time")
    }
  }
  
  // TODO syntax stuff, should probably be in a separate module ?
  function toString(x) {
    if (x instanceof data.Symbol) {
      var o = new data.String(x.value)
      o.loc = data.loc(x, x)
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

  function unary(i, o) {
    if (o == null) {
      o = {}
    }
    console.assert(o.priority == null)
    console.assert(o.parse == null)
    if (o.associativity == null) {
      o.associativity = "right"
    }
    o.priority = i
    o.parse = function (l, s, r) {
      if (r.length === 0) {
        missingRight(s)
      } else {
        var y = r[0]
        if (o.indent) {
          return l.concat([[s, data.unwrap(y)]], r.slice(1))
        } else {
          return l.concat([[s, y]], r.slice(1))
        }
      }
    }
    return o
  }

  function infix(i, o) {
    if (o == null) {
      o = {}
    }
    console.assert(o.priority == null)
    console.assert(o.parse == null)
    o.priority = i
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
    return o
  }

  function inert(start, end) {
    set(end, {
      syntax: {
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
    var o = box.make(sName)
    // TODO Object.keys ?
    for (var s in f) {
      o[s] = f[s]
    }
    box.vars.set(sName, o)
    vars[sName] = o
  }
  
  function get(s) {
    console.assert(s in vars)
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


  set("/",   { macex: opargs("/",    2), syntax: infix(70)    })
  set("*",   { macex: opargs("*",    2), syntax: infix(70)    })
  set("+",   { macex: opargs("+",    2), syntax: infix(60)    })
  set("-",   { macex: opargs("-",    2), syntax: infix(60)    })
  set("<",   { macex: opargs("<",    2), syntax: infix(50)    })
  set("=<",  { macex: opargs("<=",   2), syntax: infix(50)    })
  set(">",   { macex: opargs(">",    2), syntax: infix(50)    })
  set(">=",  { macex: opargs(">=",   2), syntax: infix(50)    })
  set("==",  { macex: opargs("===",  2), syntax: infix(40)    })
  set("|=",  {                           syntax: infix(40)    })
  set("~=",  { macex: opargs("!==",  2), syntax: infix(40)    })
  set("&&",  { macex: opargs("&&",   2), syntax: infix(30)    })
  set("||",  { macex: opargs("||",   2), syntax: infix(20)    })
  set("num", { macex: opargs("+",    1)                       })
  set("sub", { macex: opargs("-",    1)                       })
  set("mod", { macex: opargs("%",    1)                       })
  set("new", { macex: opargsl("new", 1)                       })

  inert("(", ")")
  inert("[", "]")
  inert("{", "}")
  inert("#|", "|#")
  
  set("~", {
    macex: opargs("!", 1),
    syntax: {
      priority: 80,
      associativity: "right",
      // TODO handle escapes with \ as well
      tokenize: function (o) {
        var s = o.position()
          , a = [o.read()]
        if (o.peek() === "=") {
          a.push(o.read())
        }
        var x = new data.Symbol(a.join(""))
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
    return withMode("compile", function () {
      x = compile.compile(compile.expression([macex(x)]))
      options.$eval(x)
      return nulanEval(x)
    })
  }
  
  function makeBoxSetter(s) {
    return function (a) {
      checkArguments(a, 2)
      // TODO destructuring ?
      var x = box.toBox(a[1])
        , f = compileEval(a[2])
      x[s] = function () {
        return macex(f.apply(this, arguments))
      }
      return macex([])
    }
  }
  
  set("$pattern!", { macex: makeBoxSetter("pattern") })
  
  set("$syntax!", {
    macex: function (a) {
      checkArguments(a, 2)
      // TODO destructuring ?
      var x = box.toBox(a[1])
        , f = compileEval(a[2])
      x.syntax = f
      return macex([])
    }
  })
  
  set("$syntax-unary!", {
    macex: function (a) {
      // TODO checkArguments
      // TODO destructuring ?
      var x = box.toBox(a[1])
        , i = compileEval(a[2])
        , f = compileEval(a[3])
      x.syntax = unary(i, f)
      console.log(x.syntax)
      return macex([])
    }
  })

  set("$mac!",     { macex: makeBoxSetter("macex")   })
  set("$get!",     { macex: makeBoxSetter("get")     })
  set("$set!",     { macex: makeBoxSetter("set")     })
  
  set("$eval", {
    macex: function (a) {
      checkArguments(a, 1)
      return macex(compileEval(a[1]))
    }
  })
  
  set("<=", {
    macex: function (a) {
      checkArguments(a, 2)
      var x = box.toBox(a[1])
      if (x instanceof data.Box && x.set != null) {
        return x.set([a[1], a[2]])
      } else {
        return op("=", a[0], a.slice(1).map(macex))
      }
    },
    syntax: {
      associativity: "right",
      //indent: true,
      parse: function (l, s, r) {
        if (l.length === 0) {
          missingLeft(s)
        } else if (r.length === 0) {
          missingRight(s)
        } else {
          return [[s, data.unwrap(l), data.unwrap(r)]]
        }
      }
    }
  })

  set("vars", {
    macex: function (a) {
      var after = []
      a.slice(1).forEach(function anon(x) {
        if (Array.isArray(x)) {
          box.check(x[0], get("="))
          after.push(function () {
            var k = box.set(x[1])
            return op("var", x[0], [op("=", x[0], [k, macex(x[2])])])
          })
          //patternMatch(after, x[0], x[1], x[2])
        } else {
          after.push(function () {
            var k = box.set(x)
            if (k.scope !== "local"/* && mode === "compile"*/) {
              return op("empty", x, [])
            } else {
              return op("var", x, [k]) // TODO compile(k) ?
            }
          })
        }
      })
      return op(",", a[0], after.map(function (f) { return f() }))
    }
  })
  
  set("|", {
    macex: opargsl(",", 1),
    syntax: {
      indent: true,
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

  set("[", {
    macex: function (a) {
      var left  = []
        , right = []
      // TODO use "iter" module ?
      a.slice(1).forEach(function (x) {
        if (Array.isArray(x) && box.isBox(x[0], get("@"))) {
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
                                                new data.String("concat")])].concat(right))
      } else {
        return op("array", a[0], left)
      }
    },
    syntax: {
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
          var x = new data.Symbol(".") // TODO use a box for this
          x.loc = s.loc
          return l.slice(0, -1).concat([[x, l[l.length - 1], data.unwrap(r[0])]], r.slice(1)) // TODO should this unwrap ?
        }
      }
    }
  })
  
  set("true", {}) // TODO

  set("(", {
    syntax: {
      delimiter: true,
      priority: Infinity,
      endAt: ")",
      indent: true,
      parse: function (l, s, r) {
        return l.concat([data.unwrap(r[0])], r.slice(1))
      }
    }
  })
  
  set("{", {
    macex: function (a) {
      return op("object", a[0], a.slice(1).map(function (x) {
        if (Array.isArray(x)) {
          box.check(x[0], get("="))
          return op("=", x[0], [x[1]].concat(x.slice(2).map(macex)))
        } else {
          return macex(x)
        }
      }))
    },
    syntax: {
      delimiter: true,
      priority: Infinity,
      endAt: "}",
      //indent: true,
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
  
  set("\"", {
    macex: function (a) {
      var r = a.slice(1).map(function (x) {
        return macex(x)
      })
      var every = r.every(function (x) {
        return x instanceof data.String
      })
      if (every) {
        r = r.map(function (x) {
          return x.value
        })
        var s = new data.String(r.join(""))
        s.loc = a[0].loc
        return s
      } else {
        if (!(r[0] instanceof data.String)) {
          var s = new data.String("")
          s.loc = a[0].loc
          // TODO inefficient ?
          r.unshift(s)
        }
        return r.reduce(function (x, y) {
          return op("+", a[0], [x, y])
        })
      }
    },
    syntax: {
      delimiter: true,
      tokenize: tokenize.string,
      priority: Infinity,
      endAt: "\"",
      parse: function (l, s, r) {
        return l.concat([[s].concat(r[0])], r.slice(1))
      }
    }
  })
  
  /*"_": {
    priority: Infinity,
    parse: function (l, s, r) {
      return l.concat([[s]], r)
    }
  },*/

  set(";", {
    syntax: {
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

  set(":", {
    syntax: {
      delimiter: true,
      priority: 90, // TODO: does this need to be 90?
      indent: true,
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
  set(".", {
    macex: opargs(".", 2),
    syntax: {
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

          if (x instanceof data.Number && y instanceof data.Number) {
            var i = (x.value + "." + y.value)
            s = new data.Number(+i)
            s.loc = data.loc(x.loc, y.loc)
          } else {
            s = [s, x, toString(y)]
          }

          return l.slice(0, -1).concat([s], r.slice(1))
        }
      }
    }
  })

  set(",", {
    syntax: unary(80, {
      delimiter: true
    })
  })

  set("@", {
    syntax: unary(80, {
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
        if (box.isBox(x[0], get("`"))) {
          return [get("[")].concat(x.map(function (x) {
            return anon(x, depth + 1, i)
          }))
        } else if (box.isBox(x[0], get(","))) {
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
      } else if (x instanceof data.Box) {
        return [[get("."), new data.MacexBypass(new data.Symbol("box")), new data.String("get")], new data.Number(x.id)]
        // TODO if I used the . box I can get rid of MacexBypass
        /*return new data.MacexBypass(new data.Op("call", [new data.Op(".", [,
                                                                           ]),
                                                         ]))*/
      } else if (x instanceof data.Symbol) {
        return anon(box.toBox(x), depth, i)
      } else {
        return x
      }
    }

    return function (x) {
      var first = x
        , next  = first
      if (Array.isArray(next) && box.isBox(next[0], get(","))) {
        next = next[1]
        if (Array.isArray(next) && box.isBox(next[0], get("@"))) {
          error({ loc: data.loc(first[0].loc, next[0].loc) },
                ",@ cannot be used immediately after `")
        }
      }
      return anon(x, 1, 1)
    }
  })()
      
  set("`", {
    macex: function (a) {
      checkArguments(a, 1)
      compileOnlyError(a[0])
      return macex(quasiquote(a[1]))
    },
    syntax: {
      whitespace: true,
      delimiter: true,
      priority: 80, // TODO: 10
      indent: true,
      parse: function (l, s, r) {
        if (r[0].length === 0) {
          missingRight(s)
        } else {
          return l.concat([[s, data.unwrap(r[0])]], r.slice(1))
        }
      }
    }
  })

  set("->", {
    macex: function (a) {
      return op("function", a[0],
                  [op(",", a[0], []),
                   op("return", a[0], [macex(a[2])])])
    },
    syntax: {
      priority: 10,
      associativity: "right",
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

  set("=", {
    syntax: {
      //priority: 100, // TODO why is this priority 10 ?
      indent: true,
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
  
  set("'", {
    syntax: {
      delimiter: true,
      tokenize: tokenize.symbol
    }
  })
  
  set("#", {
    syntax: {
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
    unary: unary,
    infix: infix,
    whitespace: whitespace,
  }
})
