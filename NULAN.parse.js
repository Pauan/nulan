var NULAN = (function (n) {
  "use strict";

  var t = new PARSE.Tokens()
  var hadOperator = false

  t.binary = function (s, i, s2) {
    return this.infix(s, i, function (l, r) {
      hadOperator = true
      return [n.vars[s2], l, r]
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
          console.log(o.peek())
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
    return [n.vars["sub"], r]
  })

  t.binary("*", 30, "mul")
  t.binary("/", 30, "div")
  t.binary("+", 20, "add")
  t.binary("-", 20, "sub")/*.prefix = function (o) {
    hadOperator = true
    return [n.vars["sub"], PARSE.expression(o, 40)]
  }*/

  t["->"] = {
    name: "->",
    priority: 10,
    prefix: function (o) {
      hadOperator = true

      var c, r = []
      while (o.has() && (c = o.peek()) !== t[")"] && c !== t["]"] && c !== t["}"]) {
        console.log(o.peek())
        r.push(PARSE.expression(o, 10))
      }
      var args = r.slice(0, -1)
      args.unshift(n.vars["list"])
      return [n.vars["fn"], args, r[r.length - 1]]
    }
  }

  t.braces("{", "}", {
    single: function (a) {
      a = a[0]
      a.unshift(n.vars["list"])
      return a
    },
    transform: function (a) {
      a.unshift(n.vars["list"])
      return a
    }
  })

  t.braces("[", "]", {
    single: function (r) {
      a = a[0]
      a.unshift(n.vars["dict"])
      return a
    },
    transform: function (a) {
      a.unshift(n.vars["dict"])
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


  function tokenizeNumOrSym(o) {
    var r = []
                      // TODO
    while (o.has() && /[^ \n()\[\]{}]/.test(o.peek())) {
      r.push(o.read())
    }
    r = r.join("")
    if (r === "+" || r === "*" || r === "/" || r === "->") {
      return t[r]
    } else if (/^(?:\d+\.)?\d+$/.test(r)) {
      return PARSE.literal(new n.Number(r))
    } else {
      return PARSE.literal(r.replace(/(.)\-([a-z])/g, function (s, s1, s2) {
        return s1 + s2.toLocaleUpperCase()
      }))
    }
  }

  function tokenize(o) {
    var c, r = []
    while (o.has()) {
      c = o.peek()
      if (c !== " " && c !== "\n") {
        if (c === "(" || c === ")" || c === "[" || c === "]" || c === "{" || c === "}") {
          r.push(t[c])
        } else if (c === "-") {
          o.read()
          c = o.peek()
          if (c === ">") {
            r.push(t["->"])
          } else if (c === " " || c === "\n") {
            r.push(t["-"])
          } else {
            r.push(t["u-"])
          }
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
