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
        while (o.has() && !PARSE.is(o.peek(), t[r])) {
          a.push(PARSE.expression(o))
        }
        PARSE.check(o, t[r])
        return f(a)
      }
    }
  }
/*
  function untilBraces(o, i, f) {
    var c, r = []
    while (o.has() && (c = o.peek()) &&
           !PARSE.is(c, t[")"]) &&
           !PARSE.is(c, t["]"]) &&
           !PARSE.is(c, t["}"]) &&
           !PARSE.is(c, t["|"])) {
      r.push(PARSE.expression(o, i))
    }
    return f(r)
  }*/


  var t = {
    "*":  infix(90),
    "/":  infix(80),

    "+":  infix(70),
    "-":  infix(70),

    "<":  infix(60),
    "=>": infix(60),
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
        return [[s, l.length === 1 ? l[0] : l, r[0]]].concat(r[1])
      }
    },
    "->": {
      action: function (l, s, r) {
        var args = r.slice(0, -1)
        args.unshift(new n.Bypass("list"))
        l.push([new n.Bypass(s), args, r[r.length - 1]])
        return l
      }
    },
    "=": {
      action: function (l, s, r) {
        var x = l[l.length - 1]
          , y = parseLine(r)
        l = l.slice(0, -1)
        l.push([s, x, y[0]])
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


  t.infix(".", 100, function (l, r) {
    if (r instanceof n.Symbol) {
      r = r.value
    }
    return [new n.Bypass("get"), l, r]
  })

  t["."].prefix = function (o) {
    return [this, PARSE.expression(o)]
  }
  //t.binary(".",   90, "get")

  t.unary("~",    90, "not")
  t.unary("u-",   90, "sub")

  t.binary("*",   80, "mul")
  t.binary("/",   80, "div")

  t.binary("+",   70, "add")
  t.binary("-",   70, "sub")

  t.binary("<",   60, "lt")
  t.binary("=<",  60, "islt")
  t.binary(">",   60, "gt")
  t.binary(">=",  60, "gtis")

  t.binary("=",   50, "is")
  t.binary("~=",  50, "isnt")

  t.binary("&&",  40, "and")

  t.binary("||",  30, "or")

  t.binary("<=",  20, "set!")

  t.binary(";",   10, "do")

  t["|"] = {
    name: "|",
    //priority: 10,
    prefix: function (o) {
      return this
      /*var a = []
      while (o.has() && !PARSE.is(o.peek(), t[")"])) {
        a.push(PARSE.expression(o))
      }
      return [this, a]*/
      //return [this, PARSE.expression(o)]
    }/*,
    infix: function (o, l) {
      return l
    }*/
  }

  //t.unary("'",     0, "quote")
  t.unary(",",     0, "comma")
  t.unary("@",     0, "splice")

  t["'"] = {
    name: "'",
    prefix: function (o) {
      return this
      /*
      var a = []
      while (o.has() && !PARSE.is(o.peek(), t[")"])) {
        a.push(PARSE.expression(o))
      }
      if (a.length === 1) {
        a = a[0]
      }
      return [new n.Bypass("quote"), a]*/
    }
  }

  t["->"] = {
    name: "->",
    priority: 0, // TODO does this need to be higher than 0, like 10?
    prefix: function (o) {
      return this
      //hadOperator = true
      /*return untilBraces(o, 10, function (r) {
        var args = r.slice(0, -1)
        args.unshift(new n.Bypass("list"))
        return [new n.Bypass("fn"), args, r[r.length - 1]]
      })*/
    }
  }

  t[":"] = {
    name: ":",
    prefix: function (o) {
      return this
      //return untilBraces(o, 0, function (a) { return a })
    }
  }

  t.braces("{", "}", function (a) {
    a.unshift(new n.Bypass("list"))
    return a
  })

  t.braces("[", "]", function (a) {
    a.unshift(new n.Bypass("dict"))
    return a
    /*var r = [new n.Bypass("dict")]
    // TODO: pair is defined in NULAN.js
    n.pair(a).forEach(function (x) {
      if (x[0] instanceof n.Symbol) {
        r.push(x[0].value)
      } else {
        r.push(x[0])
      }
      r.push(x[1])
    })
    return r*/
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

  var delimiters = /^[ \n()[\]{}',@:;."|]$/

  function enrich(x, o) {
    x = Object.create(x)
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

/*
  r === "+"  || r === "*"  || r === "/"  ||
        r === "->" || r === "="  || r === "~=" ||
        r === "<"  || r === ">"  || r === "<=" ||
        r === ">=" || r === "||" || r === "&&" ||
        r === "=<"
*/
  function tokenizeNum(o, a) {
    var s = store(o)
      , r = []
    while (o.has() && (o.peek() === "." || !delimiters.test(o.peek()))) {
      r.push(o.read())
    }
    r = r.join("")
    if (number(r)) {
      a.push(enrich(t.literal(+r), s))
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
    if (t[r]) { // TODO
      a.push(enrich(t[r], s))
    } else {
      a.push(enrich(t.literal(r), s))
    }
    /*} else if (number(r)) {
      a.push(enrich(t.literal(+r), s))
    } else {
      r = r.replace(/(.)\-([a-z])/g, function (s, s1, s2) {
        return s1 + s2.toLocaleUpperCase()
      })
      r = r.split(/\./)
      if (r.length === 1) {
        r = new n.Symbol(r[0])
      } else {
        r = r.reduce(function (x, y) {
          return [new n.Bypass("get"), new n.Symbol(x), (number(y) ? +y : y)]
        })
      }
      a.push(enrich(t.literal(r), s))
    }*/
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
    a.push(enrich(t.literal(r), s))
  }

  /*function getIndent(o) {
    var i = 0
    while (o.peek() === " ") {
      o.read()
      ++i
    }
    return i
  }*/
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
      //, r = [enrich(t["("], o)]
      //, i = [getIndent(o)]

    while (o.has()) {
      c = o.peek()
/*      if (c === "\n") {
        o.read()

        i2 = getIndent(o)
        //console.log(i2, i)
        while (i2 <= i[i.length - 1]) {
          i.pop()
          r.push(enrich(t[")"], o))
        }

        if (o.peek() !== "\n" && !i.length) {
        r.push(enrich(t[";"], o))
        }

        //console.log(o.peek())

        //if (delimiters.test(o.peek())) {
        c = o.peek()

        if (c === ".") {
          r.push(enrich(t[c], o))
          o.read()
        }

        if (c !== "\n") {
          r.push(enrich(t["("], o))
          i.push(i2)
        }
*/
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
            a.splice(1, 0, enrich(t["("], o))
          } else {
            a = tokenize1(i2, o)
          }
          //console.log(trans(tokenize1(i2, o)))
          r.push.apply(r, a)
        } else {
          throw new PARSE.Error(o, "expected >= " + i1 + " indentation but got " + i2)
        }*/
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
      } else if (c === "-") {
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
        }
      } else if (c === "\"") {
        tokenizeString(o, r)
      /*} else if (c === "'") {
        o.read()
        r.push(enrich(t[c], o))
        r.push(enrich(t["("], o))
        i.push(i[i.length - 1])*/
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
        r.push(enrich(t[c], o))
      } else if (/\d/.test(c)) {
        tokenizeNum(o, r)
      } else {
        tokenizeSym(o, r)
      }
    }

    /*if (i.length === 1) {
      r.push(enrich(t[")"], o))
    } else if (i.length === 2) {
      throw new PARSE.Error(o, "expected >= " + i[0] + " indentation but got " + i[1])
    } else {
      throw new PARSE.Error(o, "invalid indentation " + JSON.stringify(i))
    }*/

    /*while (i.length) {
      i.pop()
      r.push(enrich(t[")"], o))
    }*/

    return r
  }

  // TODO: can I simplify this?
  function pipe(a) {
    if (Array.isArray(a)) {
      var r = []
        , c = []
      a.forEach(function (x) {
        if (Array.isArray(x) && PARSE.is(x[0], t["|"])) {
          x = pipe(x.slice(1))
          c.push(x.length === 1 ? x[0] : x)
          //c.push(pipe(x)[1]) // TODO
        } else {
          if (c.length) {
            r.push([new n.Bypass("do")].concat(c))
            c = []
          }
          r.push(pipe(x))
        }
      })
      if (c.length) {
        r.push([new n.Bypass("do")].concat(c))
      }
      return r
    } else {
      return a
    }
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

  function fn(a) {
    if (Array.isArray(a)) {
      var r = []
        , x
        , c
      for (var i = 0, iLen = a.length; i < iLen; ++i) {
        x = a[i]
        if (PARSE.is(x, t["->"])) {
          ++i
          c = []
          while (i < iLen) {
            if (PARSE.is(a[i], t["|"])) {
              break
            } else {
              c.push(fn(a[i]))
              ++i
            }
          }

          var args = c.slice(0, -1)
          args.unshift(new n.Bypass("list"))
          r.push([new n.Bypass("fn"), args, c[c.length - 1]])

          if (i === iLen) {
            return r.length === 1 ? r[0] : r
          }
          //r.push(new n.Bypass("fn"))
        } else {
          r.push(fn(x))
        }
      }
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

  /*[["pipe", 1], ["pipe", 2], ["pipe", 3], 4, ["pipe", 5], ["pipe", 6], ["foo", ["pipe", 1], ["pipe", 5]]]*/

  // This inserts ( and ) based on indentation
  function indent(a) {
    if (PARSE.is(a[0], t["["]) || PARSE.is(a[0], t["{"])) {
      return a
    } else {
      var x = a[0]
        , r = [enrich(t["("], x)]
        , s = [x.column]
        , i = 0
        //, iOld = i

      while (i < a.length) {
        /*if (PARSE.is(a[i], t["]"])) {
          while (s.length) {
            r.push(enrich(t[")"], a[i - 1]))
            s.pop()
          }
        }*/

        if (a[i].line > x.line) {
          x = a[i]
          while (x.column <= s[s.length - 1]) {
            s.pop()
            r.push(enrich(t[")"], x))
          }

          /*if (!s.length) {
            r.push(enrich(t[";"], x))
          }*/

          // TODO: not sure if this is enriching correctly
          r.push(enrich(t["("], x))
          s.push(x.column)
          //iOld = i + 1

          /*if (PARSE.is(x, t["."])) {
            //console.log("FOOBAR")
            // TODO: not sure if this is enriching correctly
            //r.splice(iOld, 0, enrich(t["("], x))
            r.push(x)
            r.push(enrich(t["("], x))
            s.push(x.column)
            ++i
            continue
          }*/
        }
  /*
        foo
          .bar 1 2 3
          .qux 5 10 20
          .corge

        ((foo.bar 1 2 3).qux 5 10 20).corge
  */
        r.push(a[i])

        /*if (PARSE.is(a[i], t["'"]) || PARSE.is(a[i], t["|"]) ||
            (x === a[i] && PARSE.is(a[i], t["."]))) {
          r.push(enrich(t["("], a[i]))
          s.push(a[i].column)
        }*/

        ++i
      }

      while (s.length) {
        r.push(enrich(t[")"], a[i - 1]))
        s.pop()
      }

      /*if (o.peek() !== "\n" && !i.length) {
      r.push(enrich(t[";"], o))
      }*/

      //console.log(o.peek())

      //if (delimiters.test(o.peek())) {
      /*c = o.peek()

      if (c === ".") {
        r.push(enrich(t[c], o))
        o.read()
      }

      if (c !== "\n") {
        r.push(enrich(t["("], o))
        i.push(i2)
      }
      r.push(enrich(t[")"], a[i - 1]))*/

      console.log(trans(r).join(" "))
      return r
    }
  }

  //(foobar (quxcorge) (nou))

  //[ ['foobar', 'quxcorge', 'nou'] ]

  // Modified Pratt Parser, designed for lists of symbols rather than tokens
  function process(o, i) {
    i = i || 0

    var x, l = []

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
      if (x instanceof n.Symbol && (x = t[x.value]) && x.priority > i) {
        l = x.action(l, o.read(), process(o, (x.order === "right"
                                               ? x.priority - 1
                                               : x.priority)))
      } else {
        break
      }
    }

    return l.length === 1 ? l[0] : l
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

  function indent(o) {
    var x = o.peek()
      , a = []
      , y
    while (o.has()) {
      y = o.peek()
      if (y.line === x.line) {
        a.push(PARSE.expression(o))
      } else if (y.column > x.column) {
        a.push(indent(o))
      } else {
        break
      }
    }
    return process(PARSE.iter(a))// a.length === 1 ? a[0] : a
  }

  n.tokenize = function (s) {
    return tokenize(PARSE.stringBuffer(s))
  }

  n.parseRaw = function (a) {
    return indent(PARSE.iter(a))
    /*var o = PARSE.iter(a)
      , r = []
      , x
    while (o.has()) {
      r.push(indent(o))
    }
    return quote(rempipe(fn(pipe(r))))*/
  }

  n.parse = function (s) {
    return n.parseRaw(n.tokenize(s))
  }

  return n
})(NULAN || {})
