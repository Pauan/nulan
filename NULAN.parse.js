var NULAN = (function (n) {
  "use strict";

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    if (o instanceof Object) {
      var b1 = "line"   in o
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
    return (x && x[0] !== ""
             ? x[1] + "\n"
             : "")
  }

  // Buffers a string by line and keeps track of line and column information
  // Returns an iterator that moves through the string one character at a time
  // This is used for the error messages, and also significant whitespace parsing
  function stringBuffer(s) {
    var re = /([^\n]*)(?:\n|$)/g
    return {
      line: 1,
      column: 1,
      text: reMatch(re, s),
      peek: function () {
        return this.text[this.column - 1]
      },
      read: function () {
        var x = this.text[this.column - 1]
        if (this.column >= this.text.length) {
          var y = reMatch(re, s)
          // TODO: a little bit hacky
          if (y === "") {
            ++this.column
            this.read = function () {
              return this.text[this.column - 1]
            }
          } else {
            this.text = y
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

  function infix(i, b) {
    return {
      delimiter: b,
      priority: i,
      action: function (l, s, r) {
        var x = l[l.length - 1]
          , y = r[0]
        return l.slice(0, -1).concat([[s, x, y]], r.slice(1))
      }
    }
  }

  function unary(i, b) {
    return {
      delimiter: b,
      priority: i,
      order: "right",
      action: function (l, s, r) {
        var y = r[0]
        return l.concat([[s, y]], r.slice(1))
      }
    }
  }

  function delimiters(s) {
    s.split("").forEach(function (x) {
      t[x] = { delimiter: true }
    })
  }

  function inert(start, end) {
    t[end] = {
      delimiter: true,
      action: function (l, s, r) {
        throw new n.Error(s, "missing starting " + start)
      }
    }
  }

  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }

  var t = {
    ";": {
      priority: 110,
      delimiter: true,
      action: function (l, s, r) {
        l = [l]
        l.push.apply(l, r)
        return l
      }
    },

    ".":  infix(110, true),

    ",":  unary(100, true),
    "@":  unary(100, true),
    "~":  unary(100, false),

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

    "'": {
      priority: 10,
      delimiter: true,
      separator: true,
      action: function (l, s, r) {
        l.push([s, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "->": {
      priority: 10,
      order: "right",
      action: function (l, s, r) {
        var args = r.slice(0, -1)
        l.push([s, args, r[r.length - 1]])
        return l
      }
    },

    "=": {
      priority: 10,
      separator: true,
      action: function (l, s, r) {
        var x = l[l.length - 1]
        l = l.slice(0, -1)
        l.push([s, x, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    ":": {
      priority: 10,
      delimiter: true,
      separator: true,
      action: function (l, s, r) {
        l.push(r[0])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "<=": {
      order: "right",
      action: function (l, s, r) {
        return [s, unwrap(l), unwrap(r)]
      }
    },

    "|": {
      //delimiter: true,
      action: function (l, s, r) {
        return l.concat(r)
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

  delimiters(" \n([{\"#")
  inert("(", ")")
  inert("[", "]")
  inert("{", "}")

/*t.unary("u-",   90, "sub")

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
    a.push(enrich(new n.Box(r), s, o))
  }

  function tokenizeString(o, a) {
    var s = store(o)
      , q = o.peek()
      , r = []
    o.read()
    while (o.peek() !== q) {
      if (o.has()) {
        r.push(o.read())
      } else {
        s.length = 1
        throw new n.Error(s, "missing ending \"")
      }
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
        throw new n.Error(s, "missing ending |#")
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
          r.push(enrich(t.literal(new n.Box("--")), s, o)) // TODO a bit hacky
        } else {
          r.push(enrich(t["u-"], s, o))
        }*/
      } else if (c === "\"") {
        tokenizeString(o, r)
      } else if (t[c] && t[c].delimiter) {
        s = store(o)
        o.read()
        r.push(enrich(new n.Box(c), s, o))
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
      } else if (x instanceof n.Box && t[x.value]) {
        break
      }
      o.read()
      l.push(x)
    }

    // TODO: fold this into the above while loop somehow?
    while (o.has() && (x = o.peek()) && x instanceof n.Box && (y = t[x.value])) {
      pri = y.priority || 0
      if (pri > i) {
        o.read()
        r = process(o, (y.order === "right"
                         ? pri - 1
                         : pri))
        if (l.length === 0 && r.length === 0) {
          return [x]
        } else {
          l = y.action(l, x, r)
        }
      } else {
        break
      }
    }
    return l
  }

  function separator(x) {
    return x instanceof n.Box && t[x.value] && t[x.value].separator
  }

  function until(o, s, f) {
    var x = o.read()
      , y
      , r = []
    while (true) {
      if (o.has()) {
        y = o.peek()
        if (n.isBox(y, s)) {
          break
        } else if (separator(y)) {
          r.push(y, until(o, s, function (x) { return x }))
        } else {
          braces(o, r)
        }
      } else {
        throw new n.Error(x, "missing ending " + s)
      }
    }
    return new Wrap(f(process(iter(r), -1)))
  }

  function braces(o, a) {
    var x = o.peek()
    if (x instanceof n.Box) {
      if (x.value === "(") {
        x = until(o, ")", function (x) {
          return unwrap(x)
        })
        o.read()
        a.push(x)
      } else if (x.value === "{") {
        x = until(o, "}", function (x) {
          x.unshift(new n.Box("list"))
          return x
        })
        o.read()
        a.push(x)
      } else if (x.value === "[") {
        x = until(o, "]", function (x) {
          x.unshift(new n.Box("dict"))
          return x
        })
        o.read()
        a.push(x)
      } else {
        o.read()
        a.push(x)
      }
    } else {
      o.read()
      a.push(x)
    }
  }

  function indent(o, x) {
    var a = []
      , y
      , r
    while (o.has()) {
      y = o.peek()
      if (y.line === x.line) {
        if (n.isBox(y, "|") && y.column === x.column) {
          r = []
          while (o.has() && n.isBox(o.peek(), "|") && o.peek().column === y.column) {
            o.read()
            r.push(unwrap(indent(o, o.peek())))
          }
          r.unshift(y)
          a.push(r)
        } else if (separator(y)) {
          o.read()
          a.push(y, indent(o, o.peek()))
        } else {
          braces(o, a)
        }
      } else if (y.column > x.column) {
        a.push(new Wrap(unwrap(indent(o, o.peek()))))
      } else {
        break
      }
    }
    return process(iter(a), -1)
  }

  n.syntaxRules = t

  n.tokenize = function (s) {
    return tokenize(stringBuffer(s))
  }

  n.parseRaw = function (a, f) {
    a = iter(a)
    while (a.has()) {
      f(unwrap(indent(a, a.peek())))
    }
  }

  n.parse = function (s, f) {
    n.parseRaw(n.tokenize(s), f)
  }

  return n
})(NULAN || {})
