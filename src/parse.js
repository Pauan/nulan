var NULAN = (function (n) {
  "use strict"

  /**
   * Rules
   */
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

  function inert(start, end) {
    n.syntaxRules[end] = {
      delimiter: true,
      startAt: start
      /*parse: function (l, s, r) {
        throw new n.Error(s, "missing starting " + start)
      }*/
    }
  }

  n.syntaxRules = {
    "_": {
      priority: Infinity,
      parse: function (l, s, r) {
        return l.concat([[s]], r)
      }
    },

    // TODO why are these priority 90 ?
    "(": {
      priority: 90,
      delimiter: true,
      endAt: ")",
      indent: true,
      parse: function (l, s, r) {
        l.push(unwrap(r[0].map(unwrap)))
        return l.concat(r.slice(1))
      }
    },

    "[": {
      priority: 90,
      delimiter: true,
      endAt: "]",
      parse: function (l, s, r) {
        var y = r[0]
        if (s.whitespace) {
          return l.concat([[s].concat(y)], r.slice(1))
        } else {
          if (l.length === 0) {
            throw new n.Error(s, "nothing to the left")
          }
          var x = [n.enrich(new n.Symbol("."), s),
                   l[l.length - 1],
                   unwrap(y)]
          return l.slice(0, -1).concat([x], r.slice(1))
        }
      }
    },

    "{": {
      priority: 90,
      delimiter: true,
      endAt: "}",
      indent: true,
      parse: function (l, s, r) {
        var y = r[0]
          , a = [s]
        y.forEach(function (x) {
          if (x.length > 2) {
            for (var i = 0, iLen = x.length; i < iLen; i += 2) {
              a.push(pair(toString(x[i]), x[i + 1]))
            }
          } else {
            a.push(pair(toString(x[0]), x[1]))
          }
        })
        /*for (var i = 0, iLen = y.length; i < iLen; ++i) {
          a2 = []
          if (y[i] instanceof n.Symbol) {
            a2.push(n.enrich(new n.String(y[i].value), y[i].start, y[i].end))
          } else {
            a2.push(y[i])
          }
          ++i
          console.log(y[i])
          if (Array.isArray(y[i]) && !isSym(y[i][0], "=")) {
            ++i
          }
          a2.push(y[i])
          a.push(a2)
        }*/
        return l.concat([a], r.slice(1))
        //return l.concat([a], r.slice(1))
      }
    },

    ";": {
      priority: 90,
      delimiter: true,
      parse: function (l, s, r) {
        l.push([l.pop()])
        return l.concat(r)
      }
    },

    ":": {
      priority: 90, // TODO: does this need to be 90?
      delimiter: true,
      indent: true,
      parse: function (l, s, r) {
        return l.concat([r[0]], r.slice(1))
      }
    },

    // TODO: update "Customizable syntax.rst" with the new definition of "."
    ".": {
      priority: 90,
      delimiter: true,
      parse: function (l, s, r) {
        var x = l[l.length - 1]
          , y = r[0]
        if (x instanceof n.Number && y instanceof n.Number) {
          var i = (x.value + "." + y.value)
          s = n.enrich(new n.Number(+i), x, y)
        // TODO
        } else if (x === void 0) {

        } else {
          s = [s, x, toString(y)]
        }

        return l.slice(0, -1).concat([s], r.slice(1))
      }
    },

                    // TODO
    ",":  unary(80, true),
    "@":  unary(80, true),

    "~": {
      priority: 80,
      order: "right",
      // TODO handle escapes with \ as well
      tokenize: function (o) {
        var s = n.store(o)
          , a = [o.read()]
        if (o.peek() === "=") {
          a.push(o.read())
        }
        return [n.enrich(new n.Symbol(a.join("")), s, o)]
      },
      parse: function (l, s, r) {
        var y = r[0]
        return l.concat([[s, y]], r.slice(1))
      }
    },

    "*":  infix(70),
    "/":  infix(70),

    "+":  infix(60),
    "-":  infix(60),

    "<":  infix(50),
    "=<": infix(50),
    ">":  infix(50),
    ">=": infix(50),

    "==": infix(40),
    "|=": infix(40),
    "~=": infix(40),

    "&&": infix(30),

    "||": infix(20),

    "'": {
      priority: 80, // TODO: 10
      whitespace: true,
      delimiter: true,
      indent: true,
      parse: function (l, s, r) {
        l.push([s, unwrap(r[0])])
        return l.concat(r.slice(1))
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
      //priority: 10,
      indent: true,
      parse: function (l, s, r) {
        var x = l[l.length - 1]
        return l.slice(0, -1).concat([[s, x, unwrap(r[0])]], r.slice(1))
      }
    },

    "<=": {
      order: "right",
      //indent: true,
      parse: function (l, s, r) {
        return [[s, unwrap(l), unwrap(r)]]
      }
    },

    "|": {
      indent: true,
      vertical: true,
      parse: function (l, s, r) {
        return l.concat([[s].concat(r[0].map(unwrap))], r.slice(1))
      }
    },

    "\\": {
      delimiter: true,
      // TODO
      tokenize: function (o, info) {
        o.read()
        return [new n.Bypass(tokenizeOne(o, info))]
      }
    },

    "\"": {
      priority: Infinity, // TODO eh
      delimiter: true,
      endAt: "\"",
      tokenize: function (o, info, iter) {
        return tokenizeString(o, iter)
      },
      parse: function (l, s, r) {
        l.push([s].concat(r[0]))
        return l.concat(r.slice(1))
      }
    },

    "#": {
      //priority: 9001,
      //order: "right",
      delimiter: true,
      whitespace: true,
      endAt: "|#", // TODO: hacky, but it works
      tokenize: function (o) {
        var s = n.store(o)
        o.read()
        if (o.peek() === "|") {
          o.read()
          tokenizeComment(o, n.enrich({}, s, o))
          return []
        // TODO: hacky, but it works
        } else if (o.peek() === ">") {
          return tokenizeCommentDoc(o, s) // TODO fix this
        } else {
          while (o.has() && o.peek() !== "\n") {
            o.read()
          }
          return []
        }
        //return enrich(new ParseBypass(), s, o)
      },
      // TODO: hacky, but it works
      parse: function (l, s, r) {
        l.push([s].concat(r[0]))
        return l.concat(r.slice(1))
      }
    },

    "`": { delimiter: true }
  }

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
  inert("#|", "|#")


  /**
   * Stuff
   */
  function tokenizeCommentDoc(o, sFirst) {
    var seen, r = []

    o.read()

    var x = enrich(new n.Symbol("#"), sFirst, o)
    r.push(x)

    var s = store(o)

    while (o.has() && o.peek() !== "\n") {
      if (o.peek() === "`") {
        o.read()

        var s2 = store(o)

        r.push(new Bypass(unwrap(processUntil(o, "`"))))
        /*  function (x) {
          var s
          if ((s = n.vars[x.value])) {
            n.tokenUpdate(x, function (o) {
              o.box = n.boxes[s]
            })
          }
        } */
        if (o.peek() === "`") {
          o.read()
        } else {
          throw new n.Error(enrichL(s2, 1), "missing ending `")
        }
      } else {
        o.read()
      }
    }
    // TODO
    o = enrichL(o, 2)
    r.push(enrich(new n.Symbol("|#"), o.start, o.end))
    return r
  }

  function tokenizeComment(o, sFirst) {
    while (true) {
      if (!o.has()) {
        throw new n.Error(sFirst, "missing ending |#")
      }
      var c = o.peek()
      if (c === "|") {
        o.read()
        if (o.peek() === "#") {
          o.read()
          break
        }
      } else if (c === "#") {
        var sNew = n.store(o)
        o.read()
        if (o.peek() === "|") {
          o.read()
          tokenizeComment(o, n.enrich({}, sNew, o))
        }
      } else {
        o.read()
      }
    }
  }

  /*function iterStoreText(o) {
    var a = []
      , f = o.read
    //o = Object.create(o)
    o.textValue = a
    o.read = function () {
      var x = f.call(this)
      a.push(x)
      return x
    }
    return o
  }*/

  function tokenizeBrackets(o) {
    var stack = []
      , r     = []
      , x

    while (o.has()) {
      x = o.peek()
      r.push(x)
      if (x instanceof n.Symbol) {
        if (stack.length && x.value === stack[stack.length - 1]) {
          stack.pop()
        } else if ((x = n.syntaxRules[x.value]) && x.endAt != null) {
          stack.push(x.endAt)
        }
      }
      if (stack.length === 0) {
        break
      } else {
        o.read()
      }
    }

    return iterator(r)
  }

  function readIndentedString(i, o) {
    var c, s = n.store(o)
    while (i-- && o.has()) {
      if ((c = o.read()) !== " ") {
        throw new n.Error(n.enrich({}, s, o), "expected space but got " + c)
      }
    }
  }

  function tokenizeString(o, iter) {
    var s = n.store(o)
      , q = o.read()
      , r = [new n.Symbol(q)]
      , a = []

    var sFirst = s
      , sNext  = n.store(o)

    while (true) {
      if (o.has()) {
        var c = o.peek()
        if (c === q) {
          o.read()

          r[0] = n.enrich(r[0], sFirst, o)
          if (a.length) {
            r.push(n.enrich(new n.String(a.join("")), s, o))
          }
          r.push(n.enrich(new n.Symbol(q), sFirst, o))

          if (r.length === 3 && r[1] instanceof n.String) {
            return [r[1]]
          } else {
            return r
          }
        } else if (c === "\\") {
          ;(function (s) {
            o.read()
            c = o.read()
            if (c === "\n") {
              readIndentedString(sFirst.start.column, o)
            } else if (c === "r") {
              a.push("\r")
            } else if (c === "n") {
              a.push("\n")
            } else if (c === "t") {
              a.push("\t")
            } else if (c === "\"" || c === "@" || c === "\\") {
              a.push(c)
            } else {
              throw new n.Error(n.enrich({}, s, o), "expected \\ \\r \\n \\t \\\" \\@ \\\\ but got \\" + c)
            }
          })(n.store(o))
        } else if (c === "@") {
          //r[0] = enrich(r[0], s, o)
          /*n.tokenUpdate(enrich({}, s, o), function (o) {
            o.type = "string"
          })*/

          if (a.length) {
            r.push(n.enrich(new n.String(a.join("")), s, o))
            a = []
          }

          o.read()
          r.push(new n.Bypass(n.parse(tokenizeBrackets(iter))))
          s = n.store(o)
        } else if (c === "\n") {
          a.push(o.read())
          readIndentedString(sFirst.start.column, o)
        } else {
          a.push(o.read())
        }
      } else {
        throw new n.Error(n.enrich({}, sFirst, sNext), "missing ending \"")
      }
    }

    /*
                  // TODO: ew
    n.tokenUpdate(enrich({}, { line: r[0].start.line, column: r[0].start.column + 1 },
                             { line: r[0].end.line,   column: r[0].end.column - 1 }),
                  function (o) {
                    o.type = "string"
                  })*/
    //r[0] = enrich(r[0], s, o)
    /*n.tokenUpdate(enrich({}, s, o), function (o) {
      o.type = "string"
    })*/
  }

  function pair(x, y) {
    if (y == null) {
      return [x]
    } else {
      return [x, y]
    }
  }

  function toString(x) {
    if (x instanceof n.Symbol) {
      return n.enrich(new n.String(x.value), x)
    } else {
      return x
    }
  }


  /**
   * Core
   */
  function isDelimiter(o) {
    if (o.has()) {
      var c = o.peek()
      return (c = n.syntaxRules[c]) && c.delimiter
    } else {
      return true
    }
  }

  function tokenizeNumOrSym(o, info) {
    var s = n.store(o)
      , r = []
      , x
    while (o.has() && /^[0-9]$/.test(o.peek())) {
      r.push(o.read())
    }
    if (isDelimiter(o)) {
      r = +r.join("")
      x = n.enrich(new n.Number(r), s, o)
      x.whitespace = info.whitespace
      info.whitespace = false
    } else {
      //var escaped
      while (true) {
        if (isDelimiter(o)) {
          break
        /*} else if (o.peek() === "\\") {
          escaped = true
          o.read()
          r.push(o.read())*/
        } else {
          r.push(o.read())
        }
      }
      r = r.join("")
      x = n.enrich(new n.Symbol(r), s, o)
      x.whitespace = info.whitespace
      info.whitespace = (n.syntaxRules[r]
                          ? !!n.syntaxRules[r].whitespace
                          : false)
      /*if (escaped) {
        x = new n.Bypass(x)
      }*/
    }
    return x
  }

  function isVertical(x, y) {
    return y instanceof n.Symbol && y.value === x.value && y.start.column === x.start.column
  }

  function isSym(x, y) {
    return x instanceof n.Symbol && x.value === y
  }

  function vertical(x, y, o, first, stack) {
    var a = []
    // TODO code duplication
    if (y.indent) {
      a.push(parse(o, x, stack, null))
    } else {
      a = a.concat(parse(o, x, stack, null))
    }
    while (o.has() && isVertical(x, o.peek())) {
      if (y.indent) {
        a.push(parse(o, o.read(), stack, null))
      } else {
        a = a.concat(parse(o, o.read(), stack, null))
      }
    }
    return a
  }

  // TODO: multi-character tokenize and delimiter
  function tokenizeOne(o, info) {
    var c = o.peek()
      , x = n.syntaxRules[c]
      , y
    if (x && x.delimiter) {
      var s = n.store(o)
      o.read()
      y = n.enrich(new n.Symbol(c), s, o)
      y.whitespace = info.whitespace
      info.whitespace = true // !!x.whitespace
      return y
    } else {
      return tokenizeNumOrSym(o, info)
    }
  }

  function tokenize(o, info) {
    var a = []
      , i = 0

    function init() {
      while (i >= a.length && o.has()) {
        var x, c = o.peek()
        // TODO: multi-character tokenize and delimiter
        if ((x = n.syntaxRules[c]) && x.tokenize) {
          a = x.tokenize(o, info, iter)
          info.whitespace = !!x.whitespace
        } else {
          a = [tokenizeOne(o, info)]
        }
        i = 0
      }
    }

    var iter = {
      /*location: function () {
        return {
          text: o.text,
          start: {
            line: o.line,
            column: o.column - 1
          },
          end: o
        }
      },*/
      peek: function () {
        init()
        return a[i]
      },
      read: function () {
        init()
        return a[i++]
      },
      has: function () {
        init() // TODO is there a better way...?
        return i < a.length || o.has()
      }
    }

    return iter
  }

  // Heavily modified Pratt parser, designed for lists of symbols rather than expressions
  function parse(o, first, stack, i) {
    var l = []
      , y

    if (first instanceof n.Bypass) {
      first = first.value
    }

    //var l = getLeft(o, first)
    while (o.has()) {
      var x = o.peek()
      if (x instanceof n.Bypass) {
        o.read()
        l.push(x.value)
      } else {
        if (stack.length && isSym(x, stack[stack.length - 1].endAt)) {
          /*stack.pop()
          o.read()*/
          break
        }
        /*if (stack.length) {
          var last = stack[stack.length - 1]
          if (isSym(x, last.endAt)) {
            //o.read()
            break
          }
        }*/
        if (/*first === null || */x.start.line === first.start.line) {
          if (x instanceof n.Symbol && (y = n.syntaxRules[x.value])) {
            if (y.startAt != null) {
              checkStack(stack, o)
              throw new n.Error(x, "missing starting " + y.startAt)
            }
            var pri = y.priority || 0
            if (i === null || pri > i) {
              if (y.parse != null) {
                o.read()
                /*if (y.escape) {
                  l.push(o.read())
                }*/
                var r = []
                if (y.order === "right") {
                  --pri
                }
                if (y.endAt != null) {
                  //var iStack = stack.length
                  stack.push({ symbol: x, endAt: y.endAt })
                  //if (y.indent) {
                  var a = []
                  while (true) {
                    if (o.has()) {
                      if (isSym(o.peek(), y.endAt)) {
                        stack.pop()
                        o.read()
                        break
                      }
                    } else {
                      break
                    }
                    /*if (y.indent) {
                      a.push(parse(o, o.peek(), stack, null))
                    } else {*/
                    if (y.indent) {
                      a.push(parse(o, o.peek(), stack, null))
                    } else {
                      a = a.concat(parse(o, o.peek(), stack, null)) // TODO use null for first ?
                    }
                    //}
                    /*if (o.has() && isSym(o.peek(), y.endAt)) {
                      console.log(y.endAt)
                      stack.pop()
                      o.read()
                      break
                    }*/
                    /*if (stack.length === iStack) {
                      break
                    }*/
                  }
                  r.push(a)
                  /*while (o.has()) {
                    if (y.indent) {

                    } else {
                      a = a.concat(parse(o, o.peek(), stack, null))
                    }
                  }*/
                  /*} else {
                    r.push(parse(o, null, stack, null))
                  }*/
                }
                if (y.vertical) {
                  r.push(vertical(x, y, o, first, stack))
                } else if (y.indent && y.endAt == null) {
                  r.push(parse(o, o.peek(), stack, null))
                }
                r = r.concat(parse(o, first, stack, pri))
                /*if (l.length === 0 && r.length === 0) {
                  return [x]
                } else {*/
                l = y.parse(l, x, r)
                //}
              } else {
                throw new n.Error(x, "\"" + x.value + "\" has a syntax rule but doesn't have a parse function")
                //l = [x].concat(l, r) // TODO test this
              }
            } else {
              break
            }
          } else {
            o.read()
            l.push(x)
          }
        } else if (x.start.column > first.start.column) {
          l.push(unwrap(parse(o, o.peek(), stack, null)))
        } else {
          break
        }
      }
    }

    return l
  }

  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }

  function checkStack(stack, o) {
    if (stack.length) {
      var last = stack[stack.length - 1]
      if (o.has()) {
        var x = o.peek()
        throw new n.Error(x, "expected " + last.endAt + " but got " + x.value)
      } else {
        throw new n.Error(last.symbol, "missing ending " + last.endAt)
      }
    }
  }

  // Converts any array-like object into an iterator
  function iterator(s) {
    var i = 0
    return {
      peek: function () {
        return s[i]
      },
      read: function () {
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
  // This is useful for error messages
  function stringBuffer(s) {
    var re = /([^\n]*)(?:\n|$)/g
      , o  = { line: 1, column: 1 }
    return {
      text: reMatch(re, s),
      start: o,
      end: o,
      peek: function () {
        return this.text[o.column - 1]
      },
      read: function () {
        var x = this.text[o.column - 1]
        if (o.column >= this.text.length) {
          var y = reMatch(re, s)
          // TODO: a little bit hacky
          if (y === "") {
            ++o.column
            this.read = function () {
              return this.text[o.column - 1]
            }
          } else {
            this.text = y
            o.column = 1
            ++o.line
          }
        } else {
          ++o.column
        }
        return x
      },
      has: function () {
        return o.column <= this.text.length
      }
    }
  }

  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    // tests that `o` is an object
    if (Object(o) === o) {
      /*n.tokenUpdate(o, function (o) {
        o.type = "error"
      })*/
      var b1 = o.start && o.start.line != null
        , b2 = o.start && o.start.column != null
      if (o.text != null || b1 || b2) {
        var iOffset = 0
        a.push("\n")
        if (o.text != null) {
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
            a.push("line ", o.start.line)
          }
          if (b1 && b2) {
            a.push(", ")
          }
          if (b2) {
            a.push("column ", o.start.column)
          }
          a.push(")")
        }
        if (o.text != null && b2) {
          // TODO: make it work for multi-line tokens
          a.push("\n ", new Array(o.start.column - iOffset + 1).join(" "),
                        new Array((o.end.column - o.start.column) + 1).join("^"))
        }
      }
      this.text  = o.text
      this.start = o.start
      this.end   = o.end
    }
    this.originalMessage = s
    this.message = a.join("")
  }
  n.Error.prototype = new Error()
  n.Error.prototype.constructor = n.Error
  n.Error.prototype.name = "NULAN.Error"

  n.store = function (o) {
    return { text:  o.text
           , start: { line:   o.start.line
                    , column: o.start.column }
           , end:   { line:   o.end.line
                    , column: o.end.column } }
  }

  n.enrich = function (x, y, z) {
    x.text = y.text
    if (y.start != null) {
      x.start = { line:   y.start.line
                , column: y.start.column }
    }

    if (z != null) {
      y = z
    }

    if (y.end != null) {
      x.end = { line:   y.end.line
              , column: y.end.column }
    }
    return x
  }

  n.Bypass = function (x) {
    this.value = x
  }

  n.Symbol = function (x) {
    this.value = x
  }

  n.Number = function (x) {
    this.value = x
  }

  n.String = function (x) {
    this.value = x
  }

  n.tokenize = function (x) {
    return tokenize(stringBuffer(x), { whitespace: true })
  }

  // TODO not sure if this should be in here or nulan.js
  n.print = function (x) {
    if (x instanceof n.Number) {
      return "" + x.value
    } else if (x instanceof n.Bypass) {
      return n.print(x.value)
                                        // TODO ew
    } else if (x instanceof n.Symbol || (n.Box != null && x instanceof n.Box)) {
      return x.value
      /*return x.value.replace(/./g, function (s) {
        var y
        if ((y = n.syntaxRules[s]) && y.delimiter) {
          return "\\" + s
        } else {
          return s
        }
      })*/
    } else if (x instanceof n.String) {
      return "\"" + x.value + "\"" // TODO
    } else if (Array.isArray(x)) {
      return "(" + x.map(n.print).join(" ") + ")"
    } else {
      return "" + x
    }
  }

  n.parse = function (o) {
    var stack = []
    var x = unwrap(parse(o, o.peek(), stack, null))
    //console.log(o.peek())
    //o.read()
    //checkStack(stack, o)
    checkStack(stack, o)
    return x
  }

  return n
})(NULAN || {})
