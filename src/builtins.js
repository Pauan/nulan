"use strict";

var a = require("./1 tokenize")
  , b = require("./2 parse")
  , c = require("./3 macex")


var Error   = a.Error
  , Literal = a.Literal
  , Symbol  = a.Symbol
  , proxy   = b.proxy
  , Box     = c.Box
  , isGet   = c.isGet
  , isMacro = c.isMacro
  , vars    = c.vars
  , macex   = c.macex
  , toBox   = c.toBox


var builtins = exports.builtins = {}

function get(s) {
  console.assert(s in builtins)
  return builtins[s]
}

function setBox(x) {
  if (x.type === "x-nulan-box") {
    return x
  } else if (x.type === "Identifier") {
    var o = new Box(x.name, x.loc)
    vars.set(x.name, o)
    return o
  } else {
    throw new Error(x, "expected symbol but got: " + x)
  }
}

function set(s, f) {
  var o = builtins[s] = new Box(s)
  f(o)
  return o
}

function assertSymbol(x) {
  if (!(x.type === "x-nulan-box" || x.type === "Identifier")) {
    throw new Error(x, "expected box or symbol but got " + x)
    //throw new Error(x, "expected symbol but got: " + x)
  }
}

function isBox(x, y) {
  console.assert(y.type === "x-nulan-box")
  /*if (x instanceof data.Symbol) {

  } else if (!(x instanceof Box)) {
    error(x, "expected box or symbol but got ", [x])
  }*/
  x = toBox(x)
  return x.type === "x-nulan-box" && x.id === y.id
}

function isFirst(a, x) {
  return Array.isArray(a) && isBox(a[0], x)
}

// TODO test this
function assertFirst(a, x) {
  if (Array.isArray(a)) {
    if (!isBox(a[0], x)) {
      throw new Error(a[0], "expected " + x + " but got " + a[0])
    }
  } else {
    throw new Error(a, "expected (...) but got " + a)
  }
}

function empty(loc) {
  var r = []
  r.loc = loc
  return macex(r)
}

function plural(i) {
  return (i === 1 ? "" : "s")
}

function checkArguments(a, min, max) {
  var len = a.length - 1
    , x   = a[0]
  if ((min != null && len < min) || (max != null && len > max)) {
    if (max == null) {
      throw new Error(a, x + " expects at least " + min + " argument" + plural(min) + " but got " + len)
    } else if (min == null) {
      throw new Error(a, x + " expects at most " + max + " argument" + plural(max) + " but got " + len)
    } else if (min === max) {
      throw new Error(a, x + " expects " + min + " argument" + plural(min) + " but got " + len)
    } else {
      throw new Error(a, x + " expects " + min + " to " + max + " arguments but got " + len)
    }
  }
}

function unary(s) {
  return function (a) {
    checkArguments(a, 1, 1)
    return {
      loc: a.loc,
      type: "UnaryExpression",
      operator: s,
      prefix: true,
      argument: macex(a[1])
    }
  }
}

function infix(s, f) {
  return function (a) {
    checkArguments(a, 1, null)
    var args = a.slice(1).map(macex)

    if (args.length === 1) {
      if (f != null) {
        return f(a, args[0])
      } else {
        return proxy(args[0], a.loc)
      }

    } else {
      return args.reduce(function (x, y) {
        return {
          loc: a.loc,
          type: "BinaryExpression",
          operator: s,
          left: x,
          right: y
        }
      })
    }
  }
}


set("=", function () {})

set("+", function (o) {
  o[isMacro] = infix("+")
})

set("*", function (o) {
  o[isMacro] = infix("*")
})

set("/", function (o) {
  o[isMacro] = infix("/")
})

set("-", function (o) {
  o[isMacro] = infix("-", function (a, x) {
    return {
      loc: a.loc,
      type: "UnaryExpression",
      prefix: true,
      operator: "-",
      argument: x
    }
  })
})

set("not", function (o) {
  o[isMacro] = unary("!")
})

set("do", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 1, null)
    return {
      loc: a.loc,
      type: "SequenceExpression",
      expressions: a.slice(1).map(macex)
    }
  }
})

set("set!", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 2, 2)
    
    return {
      loc: a.loc,
      type: "AssignmentExpression",
      operator: "=",
      left: macex(a[1]),
      right: macex(a[2])
    }
  }
})

set("js/require", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 1, null)
    return {
      loc: a.loc,
      type: "VariableDeclaration",
      kind: "var",
      declarations: a.slice(1).map(function (x) {
        assertFirst(x, get("="))
        var l = x[1]
          , r = x[2]

        assertFirst(l, get("{")) // TODO

        var u = new Box(null, r.loc)

        l.slice(1).forEach(function (v) {
          var b
          if (isFirst(v, get("="))) {
            b = setBox(v[1])
            v = v[2]
          } else {
            b = setBox(v)
          }
          b[isGet] = function (a) {
            return {
              loc: a[0].loc,
              type: "MemberExpression",
              object: proxy(u, b.loc),
              property: v,
              computed: false
            }
          }
        })

        return {
          loc: x[0].loc,
          type: "VariableDeclarator",
          id: u,
          init: {
            loc: r.loc,
            type: "CallExpression",
            callee: new Symbol("require", r.loc),
            // TODO
            arguments: [{
              loc: r.loc,
              type: "BinaryExpression",
              operator: "+",
              left: {
                loc: r.loc,
                type: "Literal",
                value: "./"
              },
              right: macex(r)
            }]
          }
        }
      })
    }
  }
})

set("throw", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 1, 1)
    return {
      loc: a.loc,
      type: "ThrowStatement",
      argument: macex(a[1])
    }
  }
})

set("builtins", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 1, null)
    a.slice(1).forEach(function (x) {
      assertSymbol(x)
      var o = new Box(x.name, x.loc)
      o[isGet] = function (a) {
        return proxy(x, a[0].loc)
      }
      vars.set(x.name, o)
    })
    return empty(a.loc)
  }
})

set("var", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 1, null)
    return {
      loc: a.loc,
      type: "VariableDeclaration",
      kind: "var",
      declarations: a.slice(1).map(function (x) {
        var loc, l, r

        if (isFirst(x, get("="))) {
          loc = x[0]
          l   = x[1]
          r   = macex(x[2])
        } else {
          loc = x
          l   = x
          r   = null
        }

        assertSymbol(l)

        var o = new Box(l.name, l.loc)
        vars.set(l.name, o)

        return {
          loc: loc.loc,
          type: "VariableDeclarator",
          id: o,
          init: r
        }
      })
    }
  }
})

set("get", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 2, 2)
    return {
      loc: a.loc,
      type: "MemberExpression",
      object: macex(a[1]),
      property: macex(a[2]),
      computed: true
    }
  }
})

set("[", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 0, null)

    return {
      loc: a.loc,
      type: "ArrayExpression",
      elements: a.slice(1).map(macex)
    }
  }
})

set("{", function (o) {
})

set("->", function (o) {
  o[isMacro] = function (a) {
    checkArguments(a, 2, 2)

    return vars.push({}, function () {
      var args = []
        , body = []
        , seen = false
        , len  = a[1].length - 1

      a[1].forEach(function (x, i) {
        args.push(setBox(x))
      })

      body.push({
        loc: a[2].loc,
        type: "ReturnStatement",
        argument: macex(a[2])
      })

      return {
        loc: a.loc,
        type: "FunctionExpression",
        id: null,
        params: args,
        defaults: [],
        rest: null,
        body: {
          loc: a.loc, // TODO is this loc correct ?
          type: "BlockStatement",
          body: body,
        }
      }
    })
  }
})

set("\"", function (o) {
  // TODO
  o[isMacro] = function (a) {
    return new Literal(a[1].value, a.loc)
  }
})