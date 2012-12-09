var NULAN = (function (n) {
  "use strict";

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a  = [s]
      , b1 = "line"   in o
      , b2 = "column" in o
    if (o.text || b1 || b2) {
      a.push("\n")
      if (o.text) {
        a.push("  ", o.text.replace(/\n$/, ""))
      }
      if (b1 || b2) {
        a.push("  (")
        if (b1) {
          a.push("line ", o.line)
        }
        if (b1 && b2) {
          a.push(", ")
        }
        if (b2) {
          a.push("column ", o.column)
        }
        a.push(")")
      }
      if (o.text && b2) {
        a.push("\n ", new Array(o.column + 1).join(" "),
                      new Array(o.length + 1).join("^"))
      }
    }
    this.message = a.join("")
  }
  n.Error.prototype = new Error()
  n.Error.prototype.constructor = n.Error
  n.Error.prototype.name = "NULAN.Error"

  // Converts any array-like object into an iterator
  function iter(s) {
    var i = 0
    return {
      peek: function () {
        return s[i]
      },
      read: function () {
        // TODO: need more information on whether read should return the old or the new item
        return s[i++]
      },
      has: function () {
        return i < s.length
      }
    }
  }

  function reMatch(r, s) {
    var x = r.exec(s)
    return x ? x[0] : ""
  }

  // Buffers a string by line and keeps track of line and column information
  // Returns an iterator that moves through the string one character at a time
  // This is used for the error messages, and also significant whitespace parsing
  function stringBuffer(s) {
    var re = /^.*(\n|$)/gm
    return {
      line: 1,
      column: 1,
      text: reMatch(re, s),
      peek: function () {
        return this.text[this.column - 1]
      },
      read: function () {
        var x = this.text[this.column - 1]
        if (this.column > this.text.length) {
          var s = reMatch(re, s)
          if (s !== "") {
            this.text = s
            this.column = 1
            ++this.line
          }
        } else {
          ++this.column
        }
        return x
      },
      has: function () {
        return this.column <= this.text.length
      }
    }
  }


  function is(x, y) {
    return x instanceof n.Symbol && x.value === y
  }

  function infix(i, info) {
    info = info || {}
    info.priority = i

    var f = info.action

    info.action = function (l, s, r) {
      var x = l[l.length - 1]
        , y = r[0]
      l = l.slice(0, -1)
      if (f) {
        l.push(f(x, s, y))
      } else {
        l.push([s, x, y])
      }
      l.push.apply(l, r.slice(1))
      return l
    }

    return info
  }

  function unary(i) {
    return {
      delimiter: true,
      priority: i,
      order: "right",
      action: function (l, s, r) {
        var y = r[0]
        l.push([s, y])
        l.push.apply(l, r.slice(1))
        return l
      }
    }
  }

  function delimiters(s) {
    s.split("").forEach(function (x) {
      t[x] = { delimiter: true }
    })
  }

  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }

  var t = {
    ".": infix(110, {
      delimiter: true,
      action: function (l, s, r) {
        if (r instanceof n.Symbol) {
          r = r.value
        }
        return [s, l, r]
      }
    }),

    ",":  unary(100),
    "@":  unary(100),

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
      separator: true,
      priority: 20,
      order: "right",
      action: function (l, s, r) {
        l = [[s, unwrap(l), unwrap(r[0])]]
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "'": {
      delimiter: true,
      separator: true,
      action: function (l, s, r) {
        l.push([s, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "->": {
      order: "right",
      action: function (l, s, r) {
        var args = r.slice(0, -1)
        args.unshift(new n.Bypass("list")) // TODO: enrich this somehow?
        l.push([s, args, r[r.length - 1]])
        return l
      }
    },

    "=": {
      separator: true,
      action: function (l, s, r) {
        var x = l[l.length - 1]
        l = l.slice(0, -1)
        l.push([x, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    ":": {
      delimiter: true,
      separator: true,
      action: function (l, s, r) {
        l.push(r[0])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    ";": {
      delimiter: true,
      action: function (l, s, r) {
        l = [l]
        l.push.apply(l, r)
        return l
      }
    },

    /*"|": {
      order: "right",
      priority: 100,
      action: function (l, s, r) {
        var y = parseLine(r)
        l.push([s].concat(y[0]))
        l.push.apply(l, y[1])
        return l
      }
    },*/
  }

  delimiters(" \n()[]{}\"|#")
/*
  t.unary("~",    90, "not")
  t.unary("u-",   90, "sub")

  t["|"] = {
    name: "|",
    prefix: function (o) {
      return this
    }
  }*/


  function enrich(x, start, end) {
    x.text = start.text
    x.line = start.line
    x.column = start.column
    x.length = (end
                 ? (end.line === start.line
                     ? end.column - start.column
                     : 1)
                 : start.length)
    return x
  }

  function store(o) {
    return { text: o.text
           , line: o.line
           , column: o.column }
  }

  function number(s) {
    return /^(?:\d+\.)?\d+$/.test(s)
  }

  function tokenizeNum(o, a) {
    var s = store(o)
      , r = []
    while (o.has() && /[\d.]/.test(o.peek())) {
      r.push(o.read())
    }
    r = r.join("")
    if (number(r)) {
      a.push(enrich(new n.Wrapper(+r), s, o))
    } else {
      throw new n.Error(enrich({}, s, o), "invalid number: " + r)
    }
  }

  function tokenizeSym(o, a) {
    var s = store(o)
      , r = []
                      // TODO: cleanup
    while (o.has() && !(t[o.peek()] && t[o.peek()].delimiter)) {
      r.push(o.read())
    }
    r = r.join("")
    a.push(enrich(new n.Symbol(r), s, o))
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
    a.push(enrich(new n.Wrapper(r), s, o))
  }

  function tokenizeComment(o) {
    var s = store(o)
    --s.column
    s.length = 2
    while (true) {
      if (!o.has()) {
        throw new n.Error(s, "expected |# but got <EOF>")
      }
      o.read()
      if (o.peek() === "|") {
        o.read()
        if (o.peek() === "#") {
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
    var c, s, r = []

    while (o.has()) {
      c = o.peek()
      if (c === " " || c === "\n") {
        o.read()
      } else if (c === "#") {
        o.read()
        if (o.peek() === "|") {
          tokenizeComment(o)
          o.read()
        } else {
          while (o.has() && o.peek() !== "\n") {
            o.read()
          }
        }
      /*} else if (c === "-") {
        s = store(o)
        o.read()
        c = o.peek()
        if (c === ">") {
          o.read()
          r.push(enrich(t["->"], s, o))
        } else if (c === " " || c === "\n") {
          r.push(enrich(t["-"], s, o))
        } else if (c === "-") {
          o.read()
          // TODO: this should call tokenizeSym
          r.push(enrich(t.literal(new n.Symbol("--")), s, o)) // TODO a bit hacky
        } else {
          r.push(enrich(t["u-"], s, o))
        }*/
      } else if (c === "\"") {
        tokenizeString(o, r)
      } else if (t[c] && t[c].delimiter) {
        s = store(o)
        o.read()
        r.push(enrich(new n.Symbol(c), s, o))
      } else if (/\d/.test(c)) {
        tokenizeNum(o, r)
      } else {
        tokenizeSym(o, r)
      }
    }

    return r
  }

  function Wrap(x) { this.value = x }


  // Modified Pratt Parser, designed for lists of symbols rather than tokens
  function process(o, i) {
    var pri, x, y, r, l = []

    while (o.has()) {
      x = o.peek()
      if (x instanceof Wrap) {
        x = x.value
      } else if (x instanceof n.Symbol && t[x.value]) {
        break
      }
      o.read()
      l.push(x)
    }

    // TODO: fold this into the above while loop somehow?
    while (o.has() && (x = o.peek()) && x instanceof n.Symbol && (y = t[x.value])) {
      pri = y.priority || 0
      if (pri > i) {
        o.read()
        r = process(o, (y.order === "right"
                         ? pri - 1
                         : pri))
        // TODO: should this use n.Bypass or not?
        if (l.length === 0 && r.length === 0) {
          return [enrich(new n.Bypass(x.value), x)] // TODO: does this need to be wrapped in an array?
        } else {
          l = y.action(l, enrich(new n.Bypass(x.value), x), r)
        }
      } else {
        break
      }
    }
    return l
  }

  function until(o, s, f) {
    var x = o.read()
      , y
      , r = []
    while (true) {
      if (o.has()) {
        y = o.peek()
        if (is(y, s)) {
          o.read()
          break
        } else {
          r.push(braces(o))
        }
      } else {
        throw new n.Error(x, "expected " + s + " but got <EOF>")
      }
    }
    return new Wrap(f(process(iter(r), -1)))
  }

  function braces(o) {
    var x = o.peek()
    if (x instanceof n.Symbol) {
      if (x.value === "(") {
        return until(o, ")", function (x) {
          return unwrap(x)
        })
      } else if (x.value === "{") {
        return until(o, "}", function (x) {
          x.unshift(new n.Bypass("list"))
          return x
        })
      } else if (x.value === "[") {
        return until(o, "]", function (x) {
          x.unshift(new n.Bypass("dict"))
          return x
        })
      } else {
        o.read()
        return x
      }
    } else {
      o.read()
      return x
    }
  }

  function indent(o, x) {
    var a = []
      , y
      , r
    while (o.has()) {
      y = o.peek()
      if (y.line === x.line) {
        if (is(y, "|")) {
          r = []
          while (o.has() && is(o.peek(), "|") && o.peek().column === y.column) {
            o.read()
            r.push(unwrap(indent(o, o.peek())))
          }
          r.unshift(y)
          a.push(r)
                                            // TODO: clean this up a bit
        } else if (y instanceof n.Symbol && t[y.value] && t[y.value].separator) {
          o.read()
          a.push(y, indent(o, o.peek()))
        } else {
          a.push(braces(o))
        }
      } else if (y.column > x.column) {
        a.push(unwrap(indent(o, o.peek())))
      } else {
        break
      }
    }
    return process(iter(a), -1)
  }

  n.tokenize = function (s) {
    return tokenize(stringBuffer(s))
  }

  n.parseRaw = function (a) {
    a = iter(a)
    return unwrap(indent(a, a.peek()))
  }

  n.parse = function (s) {
    return n.parseRaw(n.tokenize(s))
  }

  return n
})(NULAN || {})
