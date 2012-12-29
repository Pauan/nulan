var NULAN = (function (n) {
  "use strict";

  // TODO: maybe move this into NULAN.js?
  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    if (o instanceof Object) {
      var b1 = "line"   in o
        , b2 = "column" in o
      if (o.text || b1 || b2) {
        var iOffset = 0
        a.push("\n")
        if (o.text) {
          a.push("  ", o.text.replace(/^( +)|\n$/g, function (_, s1) {
            if (s1) {
              iOffset = s1.length
            }
            return ""
          }))
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
          a.push("\n ", new Array(o.column - iOffset + 1).join(" "),
                        new Array(o.length + 1).join("^"))
        }
      }
      this.text = o.text
      this.line = o.line
      this.column = o.column
      this.length = o.length
    }
    this.originalMessage = s
    this.message = a.join("")
  }
  n.Error.prototype = new Error()
  n.Error.prototype.constructor = n.Error
  n.Error.prototype.name = "NULAN.Error"


  // TODO: make symbol inherit from wrapper?
  n.Wrapper = function (value) {
    this.value = value
  }
  n.Symbol = function (value) {
    this.value = value
  }

  n.Wrapper.prototype.toString =
  n.Symbol.prototype.toString = function () {
    return this.value
  }


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
  n.stringBuffer = function (s) {
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
      parse: function (l, s, r) {
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
      parse: function (l, s, r) {
        var y = r[0]
        return l.concat([[s, y]], r.slice(1))
      }
    }
  }

  function inert(start, end) {
    n.syntaxRules[end] = {
      delimiter: true,
      parse: function (l, s, r) {
        throw new n.Error(s, "missing starting " + start)
      }
    }
  }

  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }


  // TODO: make them into proper infix, so they behave correctly when only given a left or right side
  n.syntaxRules = {
    "(": {
      priority: 90,
      delimiter: true,
      endAt: ")",
      parse: function (l, s, r) {
        l.push(unwrap(r[0]))
        return l.concat(r.slice(1))
      }
    },

    "{": {
      priority: 90,
      delimiter: true,
      endAt: "}",
      parse: function (l, s, r) {
        r[0].unshift(s)
        l.push(r[0])
        return l.concat(r.slice(1))
      }
    },

    "[": {
      priority: 90,
      delimiter: true,
      endAt: "]",
      parse: function (l, s, r) {
        if (s.whitespace) {
          r[0].unshift(s)
          l.push(r[0])
        } else {
          var x = l[l.length - 1]
          l = l.slice(0, -1)
                  // TODO: does this need to enrich?
          l.push([enrich(new n.Symbol("."), s), x, unwrap(r[0])])
        }
        return l.concat(r.slice(1))
      }
    },

    "\"": {
      priority: 90,
      delimiter: true,
      endAt: "\"",
      tokenize: function (o) {
        return tokenizeString(o)
      },
      parse: function (l, s, r) {
        l.push([s].concat(r[0]))
        return l.concat(r.slice(1))
      }
    },

    ";": {
      priority: 90,
      delimiter: true,
      parse: function (l, s, r) {
        l.push([l.pop()])
        l.push.apply(l, r)
        return l
      }
    },

    ":": {
      priority: 90, // TODO: does this need to be 90?
      delimiter: true,
      separator: true,
      parse: function (l, s, r) {
        l.push(r[0])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    // TODO: update "Customizable syntax.rst" with the new definition of "."
    ".": {
      priority: 90,
      delimiter: true,
      parse: function (l, s, r) {
        function parse(x, y) {
          //console.log(l, y)
          if (x instanceof n.Wrapper &&
              y instanceof n.Wrapper &&
              typeof x.value === "number" &&
              typeof y.value === "number") {
            var i = (x.value + "." + y.value)
            i = enrich(new n.Wrapper(+i), x)
            i.length = x.length + y.length + 1
            return i
          } else if (y instanceof n.Symbol) {
            return [s, x, y.value]
          } else if (x === void 0) {

          } else {
            return [s, x, y]
          }
        }

        return l.slice(0, -1).concat([parse(l[l.length - 1], r[0])], r.slice(1))
      }
    },

                    // TODO
    ",":  unary(80, true),
    "@":  unary(80, true),
    //"~":  unary(90, false),

    /*"*":  infix(80),
    "/":  infix(80),

    "+":  infix(70),
    "-":  infix(70),

    "<":  infix(60),
    "=<": infix(60),
    ">":  infix(60),
    ">=": infix(60),*/

    //"==": infix(50),
    //"~=": infix(50),

    /*"&&": infix(40),

    "||": infix(30),*/

    "'": {
      priority: 80, // TODO: 10
      whitespace: true,
      delimiter: true,
      separator: true,
      parse: function (l, s, r) {
        l.push([s, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "->": {
      priority: 10,
      order: "right",
      parse: function (l, s, r) {
        var args = r.slice(0, -1)
        if (r.length === 0) {
          l.push([s, args, []])
        } else {
          l.push([s, args, r[r.length - 1]])
        }
        return l
      }
    },

    "=": {
      priority: 10,
      separator: true,
      parse: function (l, s, r) {
        var x = l[l.length - 1]
        l = l.slice(0, -1)
        l.push([s, x, unwrap(r[0])])
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "<=": {
      order: "right",
      parse: function (l, s, r) {
        return [s, unwrap(l), unwrap(r)]
      }
    },

    "|": {
      separator: true,
      vertical: true,
      parse: function (l, s, r) {
        l.push([s].concat(r[0].map(unwrap)))
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "#": {
      delimiter: true,
      whitespace: true,
      tokenize: function (o) {
        o.read()
        if (o.peek() === "|") {
          tokenizeComment(o)
          o.read()
        } else {
          while (o.has() && o.peek() !== "\n") {
            o.read()
          }
        }
        return []
      }
    }
  }

  // TODO: add in whitespace, comments, and strings to "Customizable syntax.rst"
  n.syntaxRules[" "] = n.syntaxRules["\n"] = {
    delimiter: true,
    whitespace: true,
    tokenize: function (o) {
      o.read()
      return []
    }
  }

  inert("(", ")")
  inert("[", "]")
  inert("{", "}")

  function tokenizeComment(o) {
    var s = store(o)
    // TODO: a teensy bit hacky
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

/*n.syntaxRules.unary("u-",   90, "sub")

  n.syntaxRules["|"] = {
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

  function isDelimiter(o) {
    if (o.has()) {
      var c = o.peek()
      return (c = n.syntaxRules[c]) && c.delimiter
    } else {
      return true
    }
  }

  var white

  function processOne(o) {
    var stack = []
      , r     = []
      , c

    o = tokenize(o)

    while (o.has()) {
      c = o.peek()
      r.push(c)
      if (c instanceof n.Symbol) {
        if (c.value === stack[stack.length - 1]) {
          stack.pop()
        } else if ((c = n.syntaxRules[c.value]) && "endAt" in c) {
          stack.push(c.endAt)
        }
      }
      if (stack.length === 0) {
        break
      } else {
        o.read()
      }
    }

    var temp = []
    // TODO: use something other than braces?
    braces(iter(r), temp)
    return process(iter(temp), -1)
  }

  function tokenizeString(o) {
    var s = store(o)
      , q = o.read()
      , r = [enrich(new n.Symbol(q), s, o)]
      , a = []
      , c
    while (true) {
      if (o.has()) {
        c = o.peek()
        if (c === q) {
          o.read()
          break
        } else if (c === "\\") {
          o.read()
          c = o.read()
          if (c === "r") {
            a.push("\r")
          } else if (c === "n") {
            a.push("\n")
          } else if (c === "t") {
            a.push("\t")
          } else if (c === "\"" || c === "@" || c === "\\") {
            a.push(c)
          } else {
            // TODO: a little hacky
            o.length = 2
            o.column -= 2
            throw new n.Error(o, "expected \\r \\n \\t \\\" \\@ \\\\ but got \\" + c)
          }
        } else if (c === "@") {
          o.read()
          if (a.length) {
            r.push(enrich(new n.Wrapper(a.join("")), s, o)) // TODO: use a different store
          }
          a = []
          r.push(new Bypass(unwrap(processOne(o))))
        } else {
          a.push(o.read())
        }
      } else {
        // TODO
        s.length = 1
        throw new n.Error(s, "missing ending \"")
      }
    }
    if (a.length) {
      r.push(enrich(new n.Wrapper(a.join("")), s, o))
    }
    r.push(enrich(new n.Symbol(q), o)) // TODO
    if (r.length === 3 && r[1] instanceof n.Wrapper) {
      return r[1]
    } else {
      return r
    }
  }

  function tokenizeNumOrSym(o) {
    var s = store(o)
      , r = []
      , b
    while (o.has() && /^[0-9]$/.test(o.peek())) {
      r.push(o.read())
    }
    if (isDelimiter(o)) {
      r = enrich(new n.Wrapper(+r.join("")), s, o)
      r.whitespace = white
      white = false
      return r
    } else {
      while (!isDelimiter(o)) {
        r.push(o.read())
      }
      r = r.join("")
      b = n.syntaxRules[r]
      r = enrich(new n.Symbol(r), s, o)
      r.whitespace = white
      white = (b ? !!b.whitespace : false)
      return r
    }
  }

  function tokenize(o) {
    white = true

    var lastIndex
      , lastToken = {}
      , last      = lastToken

    function next() {
      if (o.has()) {
        var x, s, c = o.peek()
        /*} else if (c === "-") {
          s = store(o)
          o.read()
          c = o.peek()
          if (c === ">") {
            o.read()
            r.push(enrich(n.syntaxRules["->"], s, o))
          } else if (c === " " || c === "\n") {
            r.push(enrich(n.syntaxRules["-"], s, o))
          } else if (c === "-") {
            o.read()
            // TODO: this should call tokenizeSym
            r.push(enrich(n.syntaxRules.literal(new n.Symbol("--")), s, o)) // TODO a bit hacky
          } else {
            r.push(enrich(n.syntaxRules["u-"], s, o))
          }*/
        /*if (c === end) {
          break*/
        // TODO: multi-character tokenize and delimiter
        if ((x = n.syntaxRules[c]) && x.tokenize) {
          // TODO: should probably pass in store somehow
          /*
          s = store(o)
          function (x) {
            push(enrich(x, s, o))
          }
          */
          lastIndex = 0
          last = x.tokenize(o)
          white = !!x.whitespace
        } else if (x && x.delimiter) {
          // TODO: some small code duplication with tokenizeNumOrSym
          s = store(o)
          o.read()
          /*if (n.syntaxRules[c].endAt) {
            // TODO
            if (o.has() && n.syntaxRules[o.peek()].endAt) {
              push(new n.Symbol(o.peek()))
              push(new n.Symbol(o.read()))
            }
          }*/
          last = enrich(new n.Symbol(c), s, o)
          last.whitespace = white
          white = !!x.whitespace
        } else {
          last = tokenizeNumOrSym(o)
        }
      }
    }

    function read() {
      next()
      while (Array.isArray(last) && last.length === 0 && o.has()) {
        next()
      }
    }

    var oIter = {
      location: function () {
        return o
      },
      peek: function () {
        if (last === lastToken) {
          read()
        }
        if (Array.isArray(last)) {
          return last[lastIndex]
        } else {
          return last
        }
      },
      read: function () {
        var old = oIter.peek()
        if (!(Array.isArray(last) && ++lastIndex < last.length)) {
          read()
        }
        return old
      },
      has: function () {
        return o.has() || (Array.isArray(last) && lastIndex < last.length)
      }
    }

    return oIter
  }


  function Bypass(x) { this.value = x }

  // Modified Pratt Parser, designed for lists of symbols rather than tokens
  function process(o, i) {
    var pri, x, y, r, l = []

    while (o.has()) {
      x = o.peek()
      if (x instanceof Bypass) {
        x = x.value
      } else if (x instanceof n.Symbol && n.syntaxRules[x.value]) {
        break
      }
      o.read()
      l.push(x)
    }

    // TODO: fold this into the above while loop somehow?
    while (o.has() && (x = o.peek()) && x instanceof n.Symbol && (y = n.syntaxRules[x.value])) {
      pri = y.priority || 0
      if (pri > i) {
        o.read()
        r = process(o, (y.order === "right"
                         ? pri - 1
                         : pri))
        if (l.length === 0 && r.length === 0) {
          return [x]
        } else {
          l = y.parse(l, x, r)
        }
      } else {
        break
      }
    }
    return l
  }

  function isSeparator(x) {
    return x instanceof n.Symbol && (x = n.syntaxRules[x.value]) && x.separator
  }

  function isVertical(x) {
    return x instanceof n.Symbol && (x = n.syntaxRules[x.value]) && x.vertical
  }

  function isEndAt(x) {
    return x instanceof n.Symbol && (x = n.syntaxRules[x.value]) && x.endAt
  }

  function isSym(x, y) {
    return x instanceof n.Symbol && x.value === y
  }

  function until(o, s) {
    var x = o.read()
      , y
      , z
      , r = []
    while (true) {
      if (o.has()) {
        y = o.peek()
        if (isSym(y, s)) {
          break
        } else if (isSeparator(y)) {
          //if (s === ")") { // TODO
          z = until(o, s)
          r.push(y)
          if (z.value.length !== 0) {
            r.push(z)
          }
          /*} else {
            r.push(y)
            o.read()
            r.push(new Bypass(process(iter([braces(o)]), -1))) // TODO
          }*/
        } else {
          braces(o, r)
        }
      } else {
        throw new n.Error(x, "missing ending " + s)
      }
    }
    return new Bypass(process(iter(r), -1))
  }

/*
  syntax-rule (
    braces ")"
    parse -> l s r
      `,@l ,r
*/

  function braces(o, a) {
    var x = o.peek()
    if (isEndAt(x)) {
      a.push(x)
      x = until(o, n.syntaxRules[x.value].endAt)
    }
    o.read()
    a.push(x)
  }

  function indent(o, x) {
    var a = []
      , y
      , b
      , r
      //, z
    while (o.has()) {
      y = o.peek()
      //z = o.location()
      //console.log("indent", y.line, z.line, y.column, z.column)
      if (y.line === x.line) {
        b = isSeparator(y)
        if (isVertical(y)) {
          r = []
          while (o.has() && isSym(o.peek(), y.value) && o.peek().column === y.column) {
            if (b) {
              o.read()
              r.push(indent(o, o.peek()))
            } else {
              r.push.apply(r, indent(o, o.read()))
            }
          }
          a.push(y, r)
        } else if (b) {
          o.read()
          a.push(y, indent(o, o.peek()))
        } else {
          braces(o, a)
        }
      } else if (y.column > x.column) {
        a.push(new Bypass(unwrap(indent(o, o.peek()))))
      } else {
        break
      }
    }
    return process(iter(a), -1)
  }

  n.tokenizeRaw = function (o) {
    return tokenize(o)
  }

  n.tokenize = function (s) {
    return n.tokenizeRaw(n.stringBuffer(s))
  }

  function lastIter(o) {
    return {
      last: o.peek(),
      location: function () {
        return o.location()
      },
      peek: function () {
        return o.peek()
      },
      read: function () {
        return (this.last = o.read())
      },
      has: function () {
        return o.has()
      }
    }
  }

  n.parseRaw = function (o, f) {
    var x
    try {
      o = lastIter(o)
    } catch (e) {
      f(e, null, null, null)
    }
    while (o.has()) {
      x = o.peek()
      try {
        f(null, unwrap(indent(o, o.peek())), x.line, o.last.line)
      } catch (e) {
        f(e, null, x.line, o.last.line)
      }
    }
  }

  n.parse = function (s, f) {
    n.parseRaw(n.tokenize(s), f)
  }

  return n
})(NULAN || {})
