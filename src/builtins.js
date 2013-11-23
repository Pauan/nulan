define(["./box", "./data", "./macex", "./tokenize"], function (box, data, macex, tokenize) {
  "use strict";
  
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
    throw new data.Error(s, "missing expression on the left side of " + data.print(s))
  }
  
  function missingRight(s) {
    throw new data.Error(s, "missing expression on the right side of " + data.print(s))
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
          throw new data.Error(s, "missing starting " + start)
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
    var o = new box.Box(sName)
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
      throw new data.Error(a[0], "expected " + i + " argument" + plural(i) + " but got " + (a.length - 1))
    }
  }
  
  function checkArgumentsl(a, i) {
    if (a.length - 1 < i) {
      throw new data.Error(a[0], "expected at least " + i + " argument" + plural(i) + " but got " + (a.length - 1))
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
  
  function opargsl(s) {
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
  
  set("<=", {
    macex: function (a) {
      checkArguments(a, 2)
      var x = box.toBox(a[1])
      if (x instanceof box.Box && x.set != null) {
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
    syntax: {
      delimiter: true,
      tokenize: tokenize.string,
      priority: Infinity,
      endAt: "\"",
      parse: function (l, s, r) {
        return l.concat([[s].concat(r[0])], r.slice(1))
        /*var x = r[0]
          , a = []
        for (var i = 0, iLen = x.length; i < iLen; ++i) {
          if (x[i] instanceof data.String) {
            a.push(x[i].value)
          } else {
            return l.concat([[s].concat(x)], r.slice(1))
          }
        }
        a = new data.String(a.join(""))
        a.loc = s.loc
        return l.concat([a], r.slice(1))*/
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

  set("`", {
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
