var NULAN = (function (n) {
  "use strict";

  n.tokenInfo = {}

  n.tokenUpdate = function (o, f) {
    if (o.start) {
      var s = o.start.line + ":" + o.start.column
        , x = n.tokenInfo[s]
      if (!x) {
        x = n.tokenInfo[s] = {}
      }
      x.start = o.start
      x.end   = o.end
      if (f) {
        f(x)
      }
    }
  }


  // TODO: maybe move this into NULAN.js?
  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Error#Custom_Error_Types
  n.Error = function (o, s) {
    var a = [s]
    if (o instanceof Object) {
      n.tokenUpdate(o, function (o) {
        o.type = "error"
      })
      var b1 = o.start && ("line"   in o.start)
        , b2 = o.start && ("column" in o.start)
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
        if (o.text && b2) {
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


  ;(function () {
    var reserved = {}

    /*
    Object.getOwnPropertyNames(window).forEach(function (s) {
      if (!Object.getOwnPropertyDescriptor(window, s).writable) {
        console.log(s)
      }
    })
    */

    // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
    ;("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with " +
      "class enum export extends import super " +
      "implements interface let package private protected public static yield " +
      "null true false " +
      "undefined NaN Infinity " + // TODO: document isn't writable either, but should probably be handled in a different way
      "arguments").split(" ").forEach(function (s) {
      reserved[s] = true
    })

    var to = new RegExp("^([0-9])|([a-z])\\-([a-z])|[^$a-zA-Z0-9]", "g")

    n.mangle = function (s) {
      // ((scope === "local" || mode === "run") && )
      // (s === "boxes" || s === "values")
      // TODO
      if (reserved[s]/* || (scope === "local" && mode === "compile" && s === "n")*/) {
        return "_" + s
      } else {
        return s.replace(to, function (s, s1, s2, s3) {
          if (s1) {
            return "_" + s1
          } else if (s2) {
            return s2 + s3.toLocaleUpperCase()
          } else {
            return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
          }
        })
      }
    }

    // mangle("50fooBar-qux")

    var from = new RegExp("_([0-9]*)_|^_([a-z0-9])|([a-z])([A-Z])", "g")

    n.unmangle = function (s) {
      return s.replace(from, function (_, s, s1, s2, s3) {
        if (s1) {
          return s1
        } else if (s2) {
          return s2 + "-" + s3.toLocaleLowerCase()
        } else {
          return s === "" ? "_" : String.fromCharCode(s)
        }
      })
    }
/*
    validJS = function (x) {
      if (typeof x === "string") {
        // TODO: code duplication with mangle
        x = x.replace(/([a-z])\-([a-z])/g, function (_, s1, s2) {
          return s1 + s2.toLocaleUpperCase()
        })
        if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)) {
          return x
        }
      }
    }*/
  })()


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


  //function ParseBypass() {}

  function Bypass(x) { this.value = x }


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
      startAt: start,
      parse: function (l, s, r) {
        throw new n.Error(s, "missing starting " + start)
      }
    }
  }

  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }

  // TODO: ugh I wish I could do this natively in JS
  function pair(a) {
    var r = []
    for (var i = 0, iLen = a.length - 1; i < iLen; i += 2) {
      r.push([a[i], a[i + 1]])
    }
    return r
  }

  function builtin(x, sOther) {
    var s = x.value
    return s[0] === "&"
             ? "builtin"
             : s[s.length - 1] === "!"
                 ? "variable-3"
                 : sOther
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
      separator: "|",
      endAt: "]",
      parse: function (l, s, r) {
        if (s.whitespace) {
          l.push([s].concat(pair(r[0]).map(function (x) {
            if (x[0] instanceof n.Symbol) {
              var val = n.mangle(x[0].value)
              n.tokenUpdate(x[0], function (o) {
                o.type = builtin(x[0], "property")
                o.value = val
              })
              return [enrich(new n.Wrapper(val), x[0].start, x[0].end), x[1]]
            } else {
              return x
            }
          })))
        } else {
          var x = l[l.length - 1]
          l = l.slice(0, -1)
                  // TODO: does this need to enrich?
          l.push([enrich(new n.Symbol("."), s.start, s.end), x, unwrap(r[0])])
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
      indent: true,
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
            i = enrich(new n.Wrapper(+i), x.start, y.end)
            //i.length = x.length + y.length + 1
            n.tokenUpdate(i, function (o) {
              o.override = true // TODO: replace this with priority
              o.type = "number"
            })
            return i
          } else if (y instanceof n.Symbol) {
            var val = n.mangle(y.value)
            n.tokenUpdate(y, function (o) {
              o.type = builtin(y, "property")
              o.value = val
            })
            return [s, x, enrich(new n.Wrapper(val), y.start, y.end)]
          // TODO
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
      indent: true,
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
      indent: true,
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
      indent: true,
      vertical: true,
      parse: function (l, s, r) {
        l.push([s].concat(r[0].map(unwrap)))
        l.push.apply(l, r.slice(1))
        return l
      }
    },

    "#": {
      //priority: 9001,
      //order: "right",
      delimiter: true,
      whitespace: true,
      endAt: "|#", // TODO: hacky, but it works
      tokenize: function (o) {
        var s = store(o)
        o.read()
        if (o.peek() === "|") {
          o.read()
          tokenizeComment(o, s)
          return []
        // TODO: hacky, but it works
        } else if (o.peek() === ">") {
          return tokenizeCommentDoc(o, s)
        } else {
          while (o.has() && o.peek() !== "\n") {
            o.read()
          }
          n.tokenUpdate(enrich({}, s, o), function (o) {
            o.type = "comment"
          })
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

  // TODO: add in whitespace, comments, and strings to "Customizable syntax.rst"
  n.syntaxRules[" "] = n.syntaxRules["\n"] = {
    delimiter: true,
    whitespace: true,
    tokenize: function (o) {
      var s = store(o)
      o.read()
      /*n.tokenUpdate(enrich({}, s, o), function (o) {
        o.type = "symbol"
        o.syntax = true
      })*/
      return []
    }
  }

  inert("(", ")")
  inert("[", "]")
  inert("{", "}")
  inert("#|", "|#")

  function tokenizeCommentDoc(o, sFirst) {
    var seen, r = []

    o.read()

    var x = enrich(new n.Symbol("#"), sFirst, o)
    r.push(x)

    n.tokenUpdate(x, function (o) {
      o.type = "hidden"
    })

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
    n.tokenUpdate(enrich({}, s, o), function (o) {
      o.type = "comment"
    })
    // TODO
    o = enrichL(o, 2)
    r.push(enrich(new n.Symbol("|#"), o.start, o.end))
    return r
  }

  function tokenizeComment(o, sFirst) {
    var c
      , sNew
    while (true) {
      if (!o.has()) {
        // TODO: a teensy bit hacky
        //sFirst.length = 2
        throw new n.Error(enrichL(sFirst, 2), "missing ending |#")
      }
      c = o.peek()
      if (c === "|") {
        o.read()
        if (o.peek() === "#") {
          o.read()
          n.tokenUpdate(enrich({}, sFirst, o), function (o) {
            o.type = "comment"
          })
          break
        }
      } else if (c === "#") {
        sNew = store(o)
        o.read()
        if (o.peek() === "|") {
          // TODO: a teensy bit hacky
          //++sNew.column
          o.read()
          tokenizeComment(o, sNew)
        }
      } else {
        o.read()
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


  // TODO: change the stringBuffer to follow the normal interface for enriched objects
  function enrich(x, start, end) {
    x.text  = start.text
    x.start = { line:   start.line
              , column: start.column }
    x.end   = { line:   end.line
              , column: end.column }
    return x
  }

  n.enrich = function (x, start, end) {
    x.text = start.text
    if ("start" in start) {
      x.start = { line:   start.start.line
                , column: start.start.column }
    }
    if ("end" in end) {
      x.end   = { line:   end.end.line
                , column: end.end.column }
    }
    return x
  }

  function enrichL(o, i) {
    return enrich({}, o, { line:   o.line
                         , column: o.column + i })
  }

  function store(o) {
    return { text:   o.text
           , line:   o.line
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

  function processUntil(oOrig, s) {
    var r = []
      , c

    var o = tokenize(oOrig)
    while (o.has()) {
      c = o.peek()
      /*if (c instanceof n.Symbol) {
        f(c)
      }*/
      r.push(c)
      if (oOrig.peek() === s) {
        break
      } else {
        o.read()
      }
    }

    //var temp = []
    r = iter(r)
    // TODO: use something other than braces?
    //braces(r, r.peek(), temp)
    //return process(iter(temp), -1)
    return indent(r, r.peek())
  }

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
    r = iter(r)
    // TODO: use something other than braces?
    braces(r, r.peek(), temp)
    return process(iter(temp), -1)
  }

  function iterStoreText(o) {
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
  }

  function tokenizeString(o) {
    var s = store(o)
      , q = o.read()
      , r = [new n.Symbol(q)]
      , a = []
      , c

    o = iterStoreText(o)

    var sFirst = s

    while (true) {
      if (o.has()) {
        c = o.peek()
        if (c === q) {
          s = o.textValue.join("")
          o.read()

          r[0] = enrich(r[0], sFirst, o)
          n.tokenUpdate(r[0], function (o) {
            o.type = "string"
            o.value = s
          })

          if (a.length) {
            r.push(enrich(new n.Wrapper(a.join("")), sFirst, o))
          }
          r.push(enrich(new n.Symbol(q), sFirst, o)) // TODO

          if (r.length === 3 && r[1] instanceof n.Wrapper) {
            return r[1]
          } else {
            return r
          }
        } else if (c === "\\") {
          o.read()
          c = o.read()
          if (c === "\n") {
            // TODO: code duplication
            var i = sFirst.column
            while (i--) {
              o.read()
            }
          } else if (c === "r") {
            a.push("\r")
          } else if (c === "n") {
            a.push("\n")
          } else if (c === "t") {
            a.push("\t")
          } else if (c === "\"" || c === "@" || c === "\\") {
            a.push(c)
          } else {
            //o.length = 2
            // TODO: a little hacky
            o.column -= 2
            throw new n.Error(enrichL(o, 2), "expected \\ \\r \\n \\t \\\" \\@ \\\\ but got \\" + c)
          }
        } else if (c === "@") {
          //r[0] = enrich(r[0], s, o)
          /*n.tokenUpdate(enrich({}, s, o), function (o) {
            o.type = "string"
          })*/

          s = store(o) // TODO

          o.read()

          n.tokenUpdate(enrich({}, s, o), function (o) {
            o.syntaxRule = n.syntaxRules[c] // TODO: shouldn't this ALWAYS be treated as syntax?
            o.type = "symbol"
          })

          if (a.length) {
            r.push(enrich(new n.Wrapper(a.join("")), s, o))
          }
          a = []

          r.push(new Bypass(unwrap(processOne(o))))
          s = store(o)
        } else if (c === "\n") {
          a.push(o.read())
          var i = sFirst.column
          while (i--) {
            o.read()
          }
        } else {
          a.push(o.read())
        }
      } else {
        // TODO: is there a way to get rid of this?
        throw new n.Error(enrichL(sFirst, 1), "missing ending \"")
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

      n.tokenUpdate(r, function (o) {
        o.type = "number"
      })
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

      n.tokenUpdate(r, function (o) {
        o.type = builtin(r, "symbol")
        o.syntaxRule = b //!!(b && (b.parse || b.tokenize)) // TODO: should this include b.tokenize?
      })
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

          n.tokenUpdate(last, function (o) {
            o.type = builtin(last, "symbol")
            o.delimiter = true
            o.syntaxRule = x //!!(x.parse || x.tokenize) // TODO: should this include x.tokenize?
          })
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

    //l.tap()

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

  // TODO: code duplication
  function isSeparator(x) {
    return x instanceof n.Symbol && (x = n.syntaxRules[x.value]) && x.separator
  }

  function isIndent(x) {
    return x instanceof n.Symbol && (x = n.syntaxRules[x.value]) && x.indent
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

  function until(o, x, s) {
    var first = true
      , sep   = isSeparator(x)
      , r     = []
      , y
      , z
    while (true) {
      if (o.has()) {
        y = o.peek()
        //console.info(y.value, sep)
        if (isSym(y, s)) {
          break
        /*} else if (sep && isSym(y, sep)) {
          o.read()
          r.push(r)
          r = []*/
        } else if (isIndent(y)) {
          //if (s === ")") { // TODO
          z = until(o, o.read(), s)
          r.push(y)
          if (z.value.length !== 0) {
            r.push(z)
          }
          /*} else {
            r.push(y)
            o.read()
            r.push(new Bypass(process(iter([braces(o)]), -1))) // TODO
          }*/
        /*} else if (first) {
          first = false
          o.read()
          z = o.peek()
          if (isSym(z, s)) {
            r.push(y)
            break
          } else {
            braces(o, y, r)
          }*/
        } else {
          braces(o, o.peek(), r)
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

  function braces(o, x, a) {
    /*if (x instanceof ParseBypass) {
      o.read()
      //braces(o, o.peek(), a)
    } else {*/
    if (isEndAt(x)) {
      a.push(x)
      x = until(o, o.read(), n.syntaxRules[x.value].endAt)
    }
    o.read()
    /*if (x instanceof ParseBypass) {
      braces(o, o.peek(), a)
    } else {*/
    a.push(x)
    //}
    //}
  }

  function indent(o, x) {
    var a = []
      , y
      , b
      , r
      //, z
    /*if (x instanceof ParseBypass) {
      o.read()
      x = o.peek()
    }*/
    /*while (x instanceof ParseBypass) {
      o.read()
      x = o.peek()
    }*/
    while (o.has()) {
      y = o.peek()
      /*if (y instanceof ParseBypass) {
        //console.info(x.line, y.line)
        if (y.line !== x.line && y.column <= x.column) {
          //console.info("HIYA", y)
          break
        } else {
          o.read()
          continue
        }
      }*/
      //z = o.location()
      //console.log("indent", y.line, z.line, y.column, z.column)
      if (y.start.line === x.start.line) {
        /*if (y instanceof ParseBypass) {
          console.log(y)
          o.read()
          console.log(o.peek())
        } else {*/
        b = isIndent(y)
        if (isVertical(y)) {
          r = []
          while (o.has() && isSym(o.peek(), y.value) && o.peek().start.column === y.start.column) {
            if (b) {
              // TODO: should probably also do this if it's not indent
              // TODO: why doesn't this work...?!
              y = o.peek()
              o.read()
              x = o.peek()
              if (x.start.line === y.start.line || x.start.column > y.start.column) {
                r.push(indent(o, x))
              } else {
                break
              }
            } else {
              r.push.apply(r, indent(o, o.read()))
            }
          }
          a.push(y, r)
        } else if (b) {
          o.read()
          a.push(y, indent(o, o.peek()))
        } else {
          braces(o, o.peek(), a)
        }
        //}
      } else if (y.start.column > x.start.column) {
        a.push(new Bypass(unwrap(indent(o, o.peek()))))
      } else {
        break
      }
    }
    //a.tap()
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
        f(null, unwrap(indent(o, o.peek())), x.start.line, o.last.start.line)
      } catch (e) {
        f(e, null, x.start.line, o.last.start.line)
      }
    }
  }

  n.parse = function (s, f) {
    n.parseRaw(n.tokenize(s), f)
  }

  return n
})(NULAN || {})
