var NULAN = (function (n) {
  "use strict";

  function infix(i, f) {
    return {
      priority: i,
      action: function (l, s, r) {
        var x = l[l.length - 1]
          , y = r[0]
        l = l.slice(0, -1)
        if (f) {
          l.push(f(x, s, y))
        } else {
          l.push([new n.Bypass(s.value), x, y])
        }
        l.push.apply(l, r.slice(1))
        return l
      }
    }
  }

  function parseLine(a) {
    var x = a[0]
      , l = [x]
      , i = 1
      , iLen = a.length
    while (i < iLen) {
      if (a[i].line === x.line || a[i].column > x.column) {
        l.push(a[i])
      } else {
        break
      }
      ++i
    }
    return [l, a.slice(i)]
  }

  var t = {
    ".": infix(100, function (l, s, r) {
      if (r instanceof n.Symbol) {
        r = r.value
      }
      return [new n.Bypass(s.value), l, r]
    }),

    "*":  infix(90),
    "/":  infix(80),

    "+":  infix(70),
    "-":  infix(70),

    "<":  infix(60),
    "=<": infix(60),
    ">":  infix(60),
    ">=": infix(60),

    "==": infix(50),
    "~=": infix(50),

    "&&": infix(40),

    "||": infix(30),

    "<=": {
      order: "right",
      priority: 20,
      action: function (l, s, r) {
        r = parseLine(r)
        var y = r[0]
        l = [[new n.Bypass(s.value),
               l.length === 1 ? l[0] : l,
               y.length === 1 ? y[0] : y]]
        l.push.apply(l, r[1])
        return l
      }
    },

    // TODO does this need to be higher precedence than 0, like 10?
    "->": {
      action: function (l, s, r) {
        var args = r.slice(0, -1)
        args.unshift(new n.Bypass("list"))
        l.push([new n.Bypass(s.value), args, r[r.length - 1]])
        return l
      }
    },

    "=": {
      action: function (l, s, r) {
        var x = l[l.length - 1]
          , y = parseLine(r)
        l = l.slice(0, -1)
        l.push([new n.Bypass(s.value), x, y[0]])
        l.push.apply(l, y[1])
        return l
      }
    },

    ":": {
      action: function (l, s, r) {
        var y = parseLine(r)
        l.push(y[0])
        l.push.apply(l, y[1])
        return l
      }
    }


/*
    foo bar <= qux corge
      nou

    ((<= (foo bar) (qux corge))
       nou)

    foo bar = qux corge
      nou

    foo bar = qux corge
      nou
      foobar

    (foo (= bar (qux corge))
      nou)*/
  }
/*
  t.unary("~",    90, "not")
  t.unary("u-",   90, "sub")

  t["|"] = {
    name: "|",
    prefix: function (o) {
      return this
    }
  }

  //t.unary("'",     0, "quote")
  t.unary(",",     0, "comma")
  t.unary("@",     0, "splice")

  t["'"] = {
    name: "'",
    prefix: function (o) {
      return this
    }
  }*/


  var delimiters = /^[ \n()[\]{}',@:;."|]$/

  function enrich(x, o) {
    x.text = o.text
    x.line = o.line
    x.column = o.column
    return x
  }

  function store(o) {
    return { text: o.text
           , line: o.line
           , column: o.column + 1 }
  }

  function number(s) {
    return /^(?:\d+\.)?\d+$/.test(s)
  }

  function tokenizeNum(o, a) {
    var s = store(o)
      , r = []
    while (o.has() && (o.peek() === "." || !delimiters.test(o.peek()))) {
      r.push(o.read())
    }
    r = r.join("")
    if (number(r)) {
      a.push(enrich(new n.Wrapper(+r), s))
    } else {
      throw new PARSE.Error(s, "invalid number: " + r)
    }
  }

  function tokenizeSym(o, a) {
    var s = store(o)
      , r = []
    while (o.has() && !delimiters.test(o.peek())) {
      r.push(o.read())
    }
    r = r.join("")
    a.push(enrich(new n.Symbol(r), s))
  }

  function tokenizeString(o, a) {
    var s = store(o)
      , q = o.peek()
      , r = []
    o.read()
    while (o.has() && o.peek() !== q) {
      r.push(o.read())
    }
    o.read()
    r = r.join("")
    a.push(enrich(new n.Wrapper(r), s))
  }

  function tokenizeComment(o) {
    while (true) {
      if (!o.has()) {
        throw new PARSE.Error(o, "expected |# but got <EOF>") // TODO this error should probably be in PARSE.js
      }
      o.read()
      if (o.peek() === "|") {
        o.read()
        if (o.peek() === "#") {
          o.read()
          break
        }
      } else if (o.peek() === "#") {
        o.read()
        if (o.peek() === "|") {
          tokenizeComment(o)
        }
      }
    }
  }

  function tokenize(o) {
    var c, r = []

    while (o.has()) {
      c = o.peek()
      if (c === " " || c === "\n") {
        o.read()
      } else if (c === "#") {
        o.read()
        if (o.peek() === "|") {
          tokenizeComment(o)
        } else {
          while (o.has() && o.peek() !== "\n") {
            o.read()
          }
        }
      /*} else if (c === "-") {
        o.read()
        c = o.peek()
        if (c === ">") {
          o.read()
          // TODO: should enrich with a stored version of <o>
          r.push(enrich(t["->"], o))
        } else if (c === " " || c === "\n") {
          r.push(enrich(t["-"], o))
        } else if (c === "-") {
          o.read()
          // TODO: should enrich with a stored version of <o>
          // TODO: this should call tokenizeSym
          r.push(enrich(t.literal(new n.Symbol("--")), o)) // TODO a bit hacky
        } else {
          r.push(enrich(t["u-"], o))
        }*/
      } else if (c === "\"") {
        tokenizeString(o, r)
      /*} else if (c === "~") {
        o.read()
        if (o.peek() === "=") {
          o.read()
          r.push(enrich(t["~="], o)) // TODO: this should call tokenizeSym
        } else {
          r.push(enrich(t[c], o))
        }*/
      } else if (delimiters.test(c)) {
        o.read()
        // t[c]
        r.push(enrich(new n.Symbol(c), o))
      } else if (/\d/.test(c)) {
        tokenizeNum(o, r)
      } else {
        tokenizeSym(o, r)
      }
    }

    return r
  }

  function rempipe(a) {
    if (Array.isArray(a)) {
      var r = []
      a.forEach(function (x) {
        if (!PARSE.is(x, t["|"])) {
          r.push(rempipe(x))
        }
      })
      return r
    } else {
      return a
    }
  }

  function quote(a) {
    if (Array.isArray(a)) {
      if (PARSE.is(a[0], t["'"])) {
        a = quote(a.slice(1))
        return [new n.Bypass("quote"), a.length === 1 ? a[0] : a]
      } else {
        return a.map(quote)
      }
    } else {
      return a
    }
  }

  function processPipe(a) {
    var r = []
      , c = []
    a.forEach(function (x) {
      if (Array.isArray(x) && x[0] instanceof n.Symbol && x[0].value === "|") {
        x = x.slice(1)
        c.push(x.length === 1 ? x[0] : x)
      } else {
        if (c.length) {
          r.push([new n.Bypass("|")].concat(c))
          c = []
        }
        r.push(x)
      }
    })
    if (c.length) {
      r.push([new n.Bypass("|")].concat(c))
    }
    return r
  }

  // Modified Pratt Parser, designed for lists of symbols rather than tokens
  function process(o, i) {
    var pri, x, y, r, l = []

    while (o.has()) {
      x = o.peek()
      if (x instanceof n.Symbol && t[x.value]) {
        break
      }
      l.push(o.read())
    }

    // TODO: fold this into the above while loop somehow?
    while (o.has()) {
      x = o.peek()
      if (x instanceof n.Symbol && (y = t[x.value])) {
        pri = y.priority || 0
        if (pri > i) {
          o.read()
          r = process(o, (y.order === "right"
                           ? pri - 1
                           : pri))
          if (l.length === 0 && r.length === 0) {
            return [x] // TODO: does this need to be wrapped in an array?
          } else {
            l = y.action(l, x, r)
          }
        } else {
          break
        }
      } else {
        break
      }
    }
    return l
  }

  function indent(o) {
    var x = o.peek()
      , a = []
      , y
    while (o.has()) {
      y = o.peek()
      if (y.line === x.line) {
        o.read()
        a.push(y)
        //a.push(PARSE.expression(o))
      } else if (y.column > x.column) {
        a.push(indent(o))
      } else {
        break
      }
    }
    a = processPipe(a)
    a = process(PARSE.iter(a), -1)
    return a.length === 1 ? a[0] : a
  }

  function check(o, s) {
    var x
    if (o.has()) {
      x = o.peek()
      if (x instanceof n.Symbol && x.value === s) {
        o.read()
      } else {
        throw new PARSE.Error(x, "expected " + s + " but got " + x)
      }
    } else {
      throw new PARSE.Error(o, "expected " + s + " but got <EOF>")
    }
  }

  function until(o, s, f) {
    o.read()
    var y = braces(o, s)
    check(o, s)
    y = process(PARSE.iter(y), -1)
    return f(y)
  }

  function braces(o, end) {
    var x, y, r = []
    while (o.has()) {
      x = o.peek()
      if (x instanceof n.Symbol) {
        if (end && x.value === end) {
          break
        } else if (x.value === "(") {
          r.push(until(o, ")", function (x) {
            return x.length === 1 ? x[0] : x
          }))
        } else if (x.value === "{") {
          r.push(until(o, "}", function (x) {
            x.unshift(new n.Bypass("list"))
            return x
          }))
        } else if (x.value === "[") {
          r.push(until(o, "]", function (x) {
            x.unshift(new n.Bypass("dict"))
            return x
          }))
        } else {
          o.read()
          r.push(x)
        }
      } else {
        o.read()
        r.push(x)
      }
    }
    return r
  }

/*


  [[foo], bar, [qux]]
*/

  n.tokenize = function (s) {
    return tokenize(PARSE.stringBuffer(s))
  }

  n.parseRaw = function (a) {
    return indent(PARSE.iter(braces(PARSE.iter(a))))
    // quote(rempipe(fn(pipe(r))))
  }

  n.parse = function (s) {
    return n.parseRaw(n.tokenize(s))
  }

  return n
})(NULAN || {})
