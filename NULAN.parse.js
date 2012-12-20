var NULAN = (function (n) {
  "use strict";

  // TODO
  n.box = function (x) {
    return new n.Symbol(x)
  }

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
    }
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

  function infix(i, b, f) {
    return {
      delimiter: b,
      priority: i,
      parse: function (l, s, r) {
        var x = l[l.length - 1]
          , y = r[0]

        return l.slice(0, -1).concat([f ? f(x, s, y) : [s, x, y]], r.slice(1))
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
    t[end] = {
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
  var t = {}

  // TODO: do these need to be priority 120?
  t["("] = {
    priority: 110,
    delimiter: true,
    endAt: ")",
    parse: function (l, s, r) {
      l.push(unwrap(r[0]))
      return l.concat(r.slice(1))
    }
  }

  t["{"] = {
    priority: 110,
    delimiter: true,
    endAt: "}",
    parse: function (l, s, r) {
      r[0].unshift(n.box("list"))
      l.push(r[0])
      return l.concat(r.slice(1))
    }
  }

  t["["] = {
    priority: 110,
    delimiter: true,
    endAt: "]",
    parse: function (l, s, r) {
      if (s.whitespace) {
        r[0].unshift(n.box("dict"))
        l.push(r[0])
      } else {
        var x = l[l.length - 1]
        l = l.slice(0, -1)
        l.push([n.box("."), x, unwrap(r[0])])
      }
      return l.concat(r.slice(1))
    }
  }

  t[";"] = {
    priority: 100,
    delimiter: true,
    parse: function (l, s, r) {
      l.push([l.pop()])
      l.push.apply(l, r)
      return l
    }
  }

  t[":"] = {
    priority: 100,
    delimiter: true,
    separator: true,
    parse: function (l, s, r) {
      l.push(r[0])
      l.push.apply(l, r.slice(1))
      return l
    }
  }

  t["."] = infix(100, true, function (l, s, r) {
    if (l instanceof n.Wrapper &&
        r instanceof n.Wrapper &&
        typeof l.value === "number" &&
        typeof r.value === "number") {
      var x = (l.value + "." + r.value)
      x = enrich(new n.Wrapper(+x), l)
      x.length = l.length + r.length + 1
      return x
    } else if (r instanceof n.Symbol) {
      return [s, l, r.value]
    } else {
      return [s, l, r]
    }
  })

                      // TODO
  t[","] =  unary(90, true)
  t["@"] =  unary(90, true)
  //t["~"] =  unary(90, false),

  /*t["*"] =  infix(80),
  t["/"] =  infix(80),

  t["+"] =  infix(70),
  t["-"] =  infix(70),

  t["<"] =  infix(60),
  t["=<"] = infix(60),
  t[">"] =  infix(60),
  t[">="] = infix(60),*/

  //t["=="] = infix(50),
  //t["~="] = infix(50),

  /*t["&&"] = infix(40),

  t["||"] = infix(30),*/

  t["'"] = {
    priority: 10,
    whitespace: true,
    delimiter: true,
    separator: true,
    parse: function (l, s, r) {
      l.push([s, unwrap(r[0])])
      l.push.apply(l, r.slice(1))
      return l
    }
  }

  t["->"] = {
    priority: 10,
    order: "right",
    parse: function (l, s, r) {
      var args = r.slice(0, -1)
      l.push([s, args, r[r.length - 1]])
      return l
    }
  }

  t["="] = {
    priority: 10,
    separator: true,
    parse: function (l, s, r) {
      var x = l[l.length - 1]
      l = l.slice(0, -1)
      l.push([s, x, unwrap(r[0])])
      l.push.apply(l, r.slice(1))
      return l
    }
  }

  t["<="] = {
    order: "right",
    parse: function (l, s, r) {
      return [s, unwrap(l), unwrap(r)]
    }
  }

  t["|"] = {
    separator: true,
    vertical: true,
    parse: function (l, s, r) {
      l.push([s].concat(r[0].map(unwrap)))
      l.push.apply(l, r.slice(1))
      return l
    }
  }

  // TODO: add in whitespace, comments, and strings to "Customizable syntax.rst"
  t[" "] = t["\n"] = {
    delimiter: true,
    whitespace: true,
    tokenize: function (c, o, push) {
      o.read()
    }
  }

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

  t["#"] = {
    delimiter: true,
    whitespace: true,
    tokenize: function (c, o, push) {
      o.read()
      if (o.peek() === "|") {
        tokenizeComment(o)
        o.read()
      } else {
        while (o.has() && o.peek() !== "\n") {
          o.read()
        }
      }
    }
  }

  // TODO: change it so tokenize receives a symbol rather than a string?
  t["\""] = {
    delimiter: true,
    endAt: "\"",
    // TODO: enrich
    tokenize: function (q, o, push) {
      var a = []
        , c
      o.read()
      push(enrich(new n.Symbol(q), o))
      while (o.peek() !== q) {
        if (o.has()) {
          c = o.peek()
          if (c === "\\") {
            o.read()
            c = o.read()
            if (c === "n") {
              a.push("\n")
            } else if (c === "t") {
              a.push("\t")
            } else if (c === "\"" || c === "@" || c === "\\") {
              a.push(c)
            } else {
              // TODO: a little hacky
              o.length = 2
              o.column -= 2
              throw new n.Error(o, "expected \\n \\t \\\" \\@ \\\\ but got \\" + c)
            }
          // TODO
          } else if (c === "@") {
            o.read()
            if (a.length) {
              push(enrich(new n.Wrapper(a.join("")), o))
            }
            a = []
            // TODO: should either make "foobar@"testing"" work or throw an error
            tokenize(o, push, q) // TODO q
            //"foo@((bar qux) corge)"
          } else {
            a.push(o.read())
          }
        } else {
          // TODO
          s.length = 1
          throw new n.Error(s, "missing ending \"")
        }
      }
      o.read()
      if (a.length) {
        push(enrich(new n.Wrapper(a.join("")), o))
      }
      push(enrich(new n.Symbol(q), o))
    },
    parse: function (l, s, r) {
      l.push([s].concat(r[0]))
      return l.concat(r.slice(1))
    }
  }

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

  function isDelimiter(o) {
    if (o.has()) {
      var c = o.peek()
      return t[c] && t[c].delimiter
    } else {
      return true
    }
  }

  var white

  function tokenizeNumOrSym(o) {
    var s = store(o)
      , r = []
      , b
    while (o.has() && /\d/.test(o.peek())) {
      r.push(o.read())
    }
    if (isDelimiter(o)) {
      return enrich(new n.Wrapper(+r.join("")), s, o)
    } else {
      while (o.has() && !isDelimiter(o)) {
        r.push(o.read())
      }
      r = r.join("")
      b = t[r]
      r = enrich(new n.Symbol(r), s, o)
      r.whitespace = white
      white = (b ? !!b.whitespace : false)
      return r
    }
  }

  function tokenize(o, push, end) {
    var c, s

    white = true

    while (o.has()) {
      c = o.peek()
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
      if (c === end) {
        break
      // TODO: multi-character tokenize and delimiter
      } else if (t[c] && t[c].tokenize) {
        // TODO: should probably pass in store somehow
        /*
        s = store(o)
        function (x) {
          push(enrich(x, s, o))
        }
        */
        t[c].tokenize(c, o, push)

        white = !!t[c].whitespace
      } else if (t[c] && t[c].delimiter) {
        // TODO: some small code duplication with tokenizeNumOrSym
        s = store(o)
        o.read()
        s = enrich(new n.Symbol(c), s, o)
        s.whitespace = white
        push(s)

        white = !!t[c].whitespace
      } else {
        push(tokenizeNumOrSym(o))
      }
    }
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
    return x instanceof n.Symbol && t[x.value] && t[x.value].separator
  }

  function isVertical(x) {
    return x instanceof n.Symbol && t[x.value] && t[x.value].vertical
  }

  function isEndAt(x) {
    return x instanceof n.Symbol && t[x.value] && t[x.value].endAt
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
            r.push(new Wrap(process(iter([braces(o)]), -1))) // TODO
          }*/
        } else {
          braces(o, r)
        }
      } else {
        throw new n.Error(x, "missing ending " + s)
      }
    }
    return new Wrap(process(iter(r), -1))
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
      x = until(o, t[x.value].endAt)
    }
    o.read()
    a.push(x)
  }

  function indent(o, x) {
    var a = []
      , y
      , b
      , r
    while (o.has()) {
      y = o.peek()
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
        a.push(new Wrap(unwrap(indent(o, o.peek()))))
      } else {
        break
      }
    }
    return process(iter(a), -1)
  }

  n.syntaxRules = t

  n.tokenize = function (s) {
    var r = []
    tokenize(stringBuffer(s), function (x) {
      r.push(x)
    })
    return r
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
