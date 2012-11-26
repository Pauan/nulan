var NULAN = (function (n) {
  "use strict";

  var t = new PARSE.Tokens()
  var hadOperator = false

  t.binary = function (s, i, s2) {
    return this.infix(s, i, function (l, r) {
      hadOperator = true
      return [new n.Bypass(s2), l, r]
    })
  }

  t.braces = function (l, r, info) {
    var self = this
    self.inert(r)
    self[l] = {
      name: l,
      prefix: function (o) {
        var old = hadOperator
        hadOperator = false

        var a = []
        while (o.has() && o.peek() !== t[r]) {
          a.push(PARSE.expression(o))
        }
        PARSE.check(o, t[r])

        if (a.length === 1 && hadOperator) {
          a = info.single(a)
        } else {
          a = info.transform(a)
        }

        hadOperator = old
        return a
      }
    }
  }


  t.prefix("u-", 40, function (r) {
    hadOperator = true
    return [new n.Bypass("sub"), r]
  })

  t.binary("*", 30, "mul")
  t.binary("/", 30, "div")
  t.binary("+", 20, "add")
  t.binary("-", 20, "sub")/*.prefix = function (o) {
    hadOperator = true
    return [new n.Bypass("sub"), PARSE.expression(o, 40)]
  }*/

  t["->"] = {
    name: "->",
    priority: 10,
    prefix: function (o) {
      hadOperator = true

      var c, r = []
      while (o.has() && (c = o.peek()) !== t[")"] && c !== t["]"] && c !== t["}"]) {
        r.push(PARSE.expression(o, 10))
      }
      var args = r.slice(0, -1)
      args.unshift(new n.Bypass("list"))
      return [new n.Bypass("fn"), args, r[r.length - 1]]
    }
  }

  t.prefix("'", 0, function (r) {
    return [new n.Bypass("&quote"), r]
  })

  t.prefix(",", 0, function (r) {
    return [new n.Bypass("&comma"), r]
  })

  t.prefix("@", 0, function (r) {
    return [new n.Bypass("&splice"), r]
  })

  t.braces("{", "}", {
    single: function (a) {
      a = a[0]
      a.unshift(new n.Bypass("list"))
      return a
    },
    transform: function (a) {
      a.unshift(new n.Bypass("list"))
      return a
    }
  })

  t.braces("[", "]", {
    single: function (r) {
      a = a[0]
      a.unshift(new n.Bypass("dict"))
      return a
    },
    transform: function (a) {
      a.unshift(new n.Bypass("dict"))
      return a
    }
  })

  t.braces("(", ")", {
    single: function (a) {
      return a[0]
    },
    transform: function (a) {
      return a
    }
  })

  // NULAN.parse("{(id -foo)}")

  function number(s) {
    return /^(?:\d+\.)?\d+$/.test(s)
  }


  function tokenizeNumOrSym(o) {
    var r = []
                      // TODO
    while (o.has() && /[^ \n()\[\]{}]/.test(o.peek())) {
      r.push(o.read())
    }
    r = r.join("")
    if (r === "+" || r === "*" || r === "/" || r === "->") {
      return t[r]
    } else if (number(r)) {
      return PARSE.literal(+r)
    } else {
      r = r.replace(/(.)\-([a-z])/g, function (s, s1, s2) {
        return s1 + s2.toLocaleUpperCase()
      })
      r = r.split(/\./).reduce(function (x, y) {
        return [new n.Bypass("get"), x, (number(y) ? +y : new n.String(y))]
      })
      return PARSE.literal(r)
    }
  }

  function tokenizeString(o, a) {
    var q = o.peek()
      , r = []
    o.read()
    while (o.has() && o.peek() !== q) {
      r.push(o.read())
    }
    o.read()
    r = r.join("")
    a.push(PARSE.literal(new n.String(r)))
  }

  function tokenize(o) {
    var c, r = []
    while (o.has()) {
      c = o.peek()
      if (c !== " " && c !== "\n") {
        if (c === "(" || c === ")" ||
            c === "[" || c === "]" ||
            c === "{" || c === "}" ||
            c === "'" || c === "," || c === "@") {
          r.push(t[c])
        } else if (c === "-") {
          o.read()
          c = o.peek()
          if (c === ">") {
            o.read()
            r.push(t["->"])
          } else if (c === " " || c === "\n") {
            r.push(t["-"])
          } else {
            r.push(t["u-"])
          }
          continue
        } else if (c === "\"") {
          tokenizeString(o, r)
          continue
        } else {
          r.push(tokenizeNumOrSym(o))
          continue
        }
      }
      o.read()
    }
    return r
  }

  n.tokenize = function (s) {
    return tokenize(PARSE.iter(s))
  }

  n.parse = function (s) {
    return PARSE.parse(PARSE.iter(n.tokenize(s)))
  }

  return n
})(NULAN || {})
