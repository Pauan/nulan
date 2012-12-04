var NULAN = (function (n) {
  "use strict";

  var t = new PARSE.Tokens()
  //var hadOperator = false

  t.unary = function (s, i, s2) {
    return this.prefix(s, i, function (r) {
      //hadOperator = true
      return [new n.Bypass(s2), r]
    })
  }

  t.binary = function (s, i, s2) {
    return this.infix(s, i, function (l, r) {
      //hadOperator = true
      return [new n.Bypass(s2), l, r]
    })
  }

  t.braces = function (l, r, f) {
    var self = this
    self.inert(r)
    self[l] = {
      name: l,
      prefix: function (o) {
        var a = []
        while (o.has() && o.peek() !== t[r]) {
          a.push(PARSE.expression(o))
        }
        PARSE.check(o, t[r])
        return f(a)
      }
    }
  }

  function untilBraces(o, i, f) {
    var c, r = []
    while (o.has() && (c = o.peek()) !== t[")"] && c !== t["]"] && c !== t["}"]) {
      r.push(PARSE.expression(o, i))
    }
    return f(r)
  }


  t.infix(".", 90, function (l, r) {
    if (r instanceof n.Symbol) {
      r = r.value
    }
    return [new n.Bypass("&get"), l, r]
  })
  //t.binary(".",   90, "&get")

  t.unary("u-",   80, "&sub")

  t.binary("*",   70, "&mul")
  t.binary("/",   70, "&div")

  t.binary("+",   60, "&add")
  t.binary("-",   60, "&sub")

  t.binary("<",   50, "&lt")
  t.binary("<=",  50, "&ltis")
  t.binary(">",   50, "&gt")
  t.binary(">=",  50, "&gte")

  t.binary("=",   40, "&is")
  t.binary("!=",  40, "&isnt")

  t.binary("&&",  30, "&and")
  t.binary("||",  20, "&or")

  t.binary("=<",  10, "&set!")

  t.unary("~",     0, "&not") // TODO: not sure if this should be 0 or 80 priority
  t.unary("'",     0, "&quote")
  t.unary(",",     0, "&comma")
  t.unary("@",     0, "&splice")

  t["->"] = {
    name: "->",
    priority: 0, // TODO does this need to be higher than 0, like 10?
    prefix: function (o) {
      //hadOperator = true
      return untilBraces(o, 10, function (r) {
        var args = r.slice(0, -1)
        args.unshift(new n.Bypass("&list"))
        return [new n.Bypass("&fn"), args, r[r.length - 1]]
      })
    }
  }

  t[":"] = {
    name: ":",
    prefix: function (o) {
      return untilBraces(o, 0, function (a) { return a })
    }
  }

  t.braces("{", "}", function (a) {
    a.unshift(new n.Bypass("&list"))
    return a
  })

  t.braces("[", "]", function (a) {
    a.unshift(new n.Bypass("&dict"))
    return a
  })

  t.braces("(", ")", function (a) {
    return a.length === 1 ? a[0] : a
  })

/*
  t["("].priority = 0
  t["("].infix = function (o, l) {
    console.log("FOO")
    return iterator(o, ")", 0, [l], function (a) { return a })
  }*/


  // NULAN.parse("{(id -foo)}")

  var delimiters = /^[ \n()[\]{}',@~:;."]$/

  function number(s) {
    return /^(?:\d+\.)?\d+$/.test(s)
  }

  function tokenizeNumOrSym(o, a) {
    var r = []
    while (o.has() && !delimiters.test(o.peek())) {
      r.push(o.read())
    }
    r = r.join("")
    if (r === "+"  || r === "*"  || r === "/"  ||
        r === "->" || r === "="  || r === "~=" || // TODO ~=
        r === "<"  || r === ">"  || r === "<=" ||
        r === ">=" || r === "||" || r === "&&" ||
        r === "=<") {
      a.push(t[r])
    } else if (number(r)) {
      a.push(PARSE.literal(+r))
    } else {
      r = r.replace(/(.)\-([a-z])/g, function (s, s1, s2) {
        return s1 + s2.toLocaleUpperCase()
      })
      r = r.split(/\./)
      if (r.length === 1) {
        r = new n.Symbol(r[0])
      } else {
        r = r.reduce(function (x, y) {
          return [new n.Bypass("&get"), new n.Symbol(x), (number(y) ? +y : y)]
        })
      }
      a.push(PARSE.literal(r))
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
    a.push(PARSE.literal(r))
  }

  function getIndent(o) {
    var i = 0
    while (o.peek() === " ") {
      o.read()
      ++i
    }
    return i
  }
/*
    foo 1 2 3
    bar 1 2 3
  qux 1 2 3

  (foo 1 2 3)
  (bar 1 2 3)
  (qux 1 2 3)

  foo 1 2 3
    bar 1 2 3

  (foo 1 2 3
    (bar 1 2 3))

    foo 1 2 3
  bar 1 2 3

  (foo 1 2 3)
  (bar 1 2 3)


  foo 1 = 2
      3 = 4
    bar 10 20
*/
  function trans(a) {
    if (Array.isArray(a)) {
      return a.map(trans)
    } else {
      a = a.name
      if (a instanceof n.Symbol) {
        return a.value
      } else {
        return a
      }
    }
  }

  function tokenize1(o) {
    var a, c, i2
      , r = [t["("]]
      , i = [getIndent(o)]
    while (o.has()) {
      c = o.peek()
      if (c === "\n") {
        o.read()

        i2 = getIndent(o)
        while (i2 <= i[i.length - 1]) {
          i.pop()
          r.push(t[")"])
        }

        //console.log(o.peek())

        //if (delimiters.test(o.peek())) {
        if (o.peek() === ".") {
          r.push(t["."])
          o.read()
        }

        r.push(t["("])
        i.push(i2)

/*
        //console.log(i1, i2, o.peek())
        if (i2 === i1) {
          //r.push.apply(r, tokenize1(i2, o))
          //console.log(i1, i2)
          break
        } else if (i2 > i1) {
          // TODO: a bit hacky
          if (o.peek() === ".") {
            a = tokenize1(i2, o).slice(1)
            a.splice(1, 0, t["("])
          } else {
            a = tokenize1(i2, o)
          }
          //console.log(trans(tokenize1(i2, o)))
          r.push.apply(r, a)
        } else {
          throw new Error("expected >= " + i1 + " indentation but got " + i2)
        }*/
      } else if (c === " ") {
        o.read()
      } else if (c === "#") {
        // TODO: multi-line comments
        while (o.has() && o.peek() !== "\n") {
          o.read()
        }
      } else if (c === "-") {
        o.read()
        c = o.peek()
        if (c === ">") {
          o.read()
          r.push(t["->"])
        } else if (c === " " || c === "\n") {
          r.push(t["-"])
        } else if (c === "-") {
          o.read()
          r.push(PARSE.literal(new n.Symbol("--"))) // TODO a bit hacky
        } else {
          r.push(t["u-"])
        }
      } else if (c === "\"") {
        tokenizeString(o, r)
      } else if (c === "'") {
        r.push(t[c])
        r.push(t["("])
        i.push(i[i.length - 1])
        o.read()
      } else if (delimiters.test(c)) {
        r.push(t[c])
        o.read()
      } else {
        tokenizeNumOrSym(o, r)
      }
    }

    /*if (i.length === 1) {
      r.push(t[")"])
    } else if (i.length === 2) {
      throw new Error("expected >= " + i[0] + " indentation but got " + i[1])
    } else {
      throw new Error("invalid indentation " + JSON.stringify(i))
    }*/

    while (i.length) {
      i.pop()
      r.push(t[")"])
    }

    console.log(trans(r))
    return r
  }

  //(foobar (quxcorge) (nou))

  //[ ['foobar', 'quxcorge', 'nou'] ]

  function tokenize(o) {
    var r = []
    while (o.has()) {
      r.push(tokenize1(o))
    }
    return r
  }

  n.tokenize = function (s) {
    return tokenize(PARSE.iter(s))
  }

  n.parse = function (s) {
    return n.tokenize(s).map(function (x) {
      return PARSE.parse(PARSE.iter(x))
    })
  }

  return n
})(NULAN || {})
