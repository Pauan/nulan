define(["../lib/util/buffer", "./data", "./box"], function (buffer, data, box) {
  "use strict";
  
  var indentStack = []
  
  function pushIndent(i) {
    var r = []
    while (indentStack.length) {
      var last = indentStack[indentStack.length - 1]
      if (i > last) {
        indentStack.push(i)
        r.push(new data.ParseIndent())
        break
      } else {
        indentStack.pop()
        r.push(new data.ParseDedent())
      }
    }
    // TODO code duplication
    if (indentStack.length === 0) {
      indentStack.push(i)
      r.push(new data.ParseIndent())
    }
    return r
  }

  function indent(o) {
    //var s = o.position()
    var i = 0
    while (o.peek() === " ") {
      o.read()
      ++i
    }
    
    // Ignore lines that contain only whitespace
    if (o.peek() === "\n") {
      return []
    } else {
      return pushIndent(i)
    }
  }
  
  function deindent() {
    var r = []
    while (indentStack.length) {
      indentStack.pop()
      r.push(new data.ParseDedent())
    }
    return r
  }
  
  function isDelimiter(o, info) {
    if (o.has()) {
      var c = o.peek()
      if (c === " " || c === "\n") {
        return true
      } else {
        var x = box.getSyntax(c)
        return x !== null && x.delimiter
      }
    } else {
      return true
    }
  }
  
  function isWhitespace(s) {
    if (s === " " || s === "\n") {
      return true
    } else {
      var x = box.getSyntax(s)
      if (x !== null) {
        return !!x.whitespace
      } else {
        return false
      }
    }
  }
  
  // TODO probably get rid of this
  function tokenizeCommentDoc(o, sFirst) {
    var seen, r = []

    o.read()

    var x = new data.Symbol("#")
    x.loc = o.loc(sFirst, o.position()) // TODO check this
    r.push(x)

    var s = o.position()

    while (o.has() && o.peek() !== "\n") {
      if (o.peek() === "`") {
        o.read()

        var s2 = o.position()

        r.push(new data.ParseBypass(data.unwrap(processUntil(o, "`"))))
        /*  function (x) {
          var s
          if ((s = vars[x.value])) {
            tokenUpdate(x, function (o) {
              o.box = boxes[s]
            })
          }
        } */
        if (o.peek() === "`") {
          o.read()
        } else {
          throw new data.Error(enrichL(s2, 1), "missing ending `")
        }
      } else {
        o.read()
      }
    }
    // TODO
    o = enrichL(o, 2)
    x = new data.Symbol("|#")
    x.loc = data.loc(o, o)
    r.push(x)
    return r
  }
  
  function comment(o) {
    var s = o.position()
    o.read()
    if (o.peek() === "|") {
      o.read()
      var x = {}
      x.loc = o.loc(s, o.position())
      commentBlock(o, x)
      return []
    /*
    // TODO: hacky, but it works
    } else if (o.peek() === ">") {
      return tokenize.commentDoc(o, s) // TODO fix this
    */
    } else {
      while (o.has() && o.peek() !== "\n") {
        o.read()
      }
      return []
    }
    //return tokenize.enrich(new ParseBypass(), s, o)
  }

  function commentBlock(o, sFirst) {
    while (true) {
      if (!o.has()) {
        throw new data.Error(sFirst, "missing ending |#")
      }
      var c = o.peek()
      if (c === "|") {
        o.read()
        if (o.peek() === "#") {
          o.read()
          break
        }
      } else if (c === "#") {
        var sNew = o.position()
        o.read()
        if (o.peek() === "|") {
          o.read()
          var x = {}
          x.loc = o.loc(sNew, o.position())
          commentBlock(o, x)
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
      if (x instanceof Symbol) {
        if (stack.length && x.value === stack[stack.length - 1]) {
          stack.pop()
        } else if ((x = syntaxRules[x.value]) && x.endAt != null) {
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
    ++i
    while (i-- && o.has()) {
      var s = o.position()
      var c = o.read()
      if (c !== " ") {
        var x = {}
        x.loc = o.loc(s, o.position())
        throw new data.Error(x, "expected space but got " + c)
      }
    }
  }

  function string(o, info) {
    var s = o.position()
      , q = o.read()
      , r = [new data.Symbol(q)]
      , a = []
      , x

    var sFirst = s
      , sNext  = o.position()

    while (true) {
      if (o.has()) {
        var c = o.peek()
        if (c === q) {
          o.read()

          r[0].loc = o.loc(sFirst, o.position())
          if (a.length) {
            x = new data.String(a.join(""))
            x.loc = o.loc(s, o.position())
            r.push(x)
          }
          x = new data.Symbol(q)
          x.loc = o.loc(sFirst, o.position())
          r.push(x)

          if (r.length === 3 && r[1] instanceof data.String) {
            return [r[1]]
          } else {
            return r
          }
        } else if (c === "\\") {
          ;(function (s) {
            o.read()
            var c = o.read()
            if (c === "\n") {
              readIndentedString(sFirst.column, o)
            } else if (c === "r") {
              a.push("\r")
            } else if (c === "n") {
              a.push("\n")
            } else if (c === "t") {
              a.push("\t")
            } else if (c === q || c === "@" || c === "\\") {
              a.push(c)
            } else {
              var x = {}
              x.loc = o.loc(s, o.position())
              throw new data.Error(x, "expected \\ \\r \\n \\t \\" + q + " \\@ \\\\ but got \\" + c)
            }
          })(o.position())
        /* TODO
        } else if (c === "@") {
          //r[0] = enrich(r[0], s, o)
          //tokenUpdate(enrich({}, s, o), function (o) {
          //  o.type = "string"
          //})

          if (a.length) {
            x = new data.String(a.join(""))
            x.loc = o.loc(s, o.position())
            r.push(x)
            a = []
          }

          o.read()
          r.push(new data.ParseBypass(parse1(tokenizeBrackets(info))))
          s = o.position()*/
        } else if (c === "\n") {
          a.push(o.read())
          readIndentedString(sFirst.column, o)
        } else {
          a.push(o.read())
        }
      } else {
        x = {}
        x.loc = o.loc(sFirst, sNext)
        throw new data.Error(x, "missing ending " + q)
      }
    }

    /*
                  // TODO: ew
    tokenUpdate(enrich({}, { line: r[0].start.line, column: r[0].start.column + 1 },
                             { line: r[0].end.line,   column: r[0].end.column - 1 }),
                  function (o) {
                    o.type = "string"
                  })*/
    //r[0] = enrich(r[0], s, o)
    /*tokenUpdate(enrich({}, s, o), function (o) {
      o.type = "string"
    })*/
  }
  
  // TODO code duplication with string
  function symbol(o, info) {
    var s = o.position()
      , q = o.read()
      , r = []
      , x
    
    var sNext = o.position()

    while (true) {
      if (o.has()) {
        var c = o.peek()
        if (c === q) {
          o.read()
          x = new data.Symbol(r.join(""))
          x.loc = o.loc(s, o.position())
          return [new data.ParseBypass(x)]
        } else if (c === "\\") {
          ;(function (s) {
            o.read()
            var c = o.read()
            // TODO is this correct ?
            if (c === "\n") {

            } else if (c === "r") {
              r.push("\r")
            } else if (c === "n") {
              r.push("\n")
            } else if (c === "t") {
              r.push("\t")
            } else if (c === q || c === "\\") {
              r.push(c)
            } else {
              var x = {}
              x.loc = o.loc(s, o.position())
              throw new data.Error(x, "expected \\ \\r \\n \\t \\" + q + " \\\\ but got \\" + c)
            }
          })(o.position())
        // TODO should this readIndentedString ?
        } else if (c === "\n") {
          r.push(o.read())
        } else {
          r.push(o.read())
        }
      } else {
        x = {}
        x.loc = o.loc(s, sNext)
        throw new data.Error(x, "missing ending " + q)
      }
    }
  }

  function numOrSym(o, info) {
    var s = o.position()
      , r = []
      , x
    while (o.has() && /^[0-9]$/.test(o.peek())) {
      r.push(o.read())
    }
    if (isDelimiter(o, info)) {
      r = +r.join("")
      x = new data.Number(r)
      x.loc = o.loc(s, o.position())
      x.whitespace = info.whitespace
      info.whitespace = false
    } else {
      //var escaped
      while (true) {
        if (isDelimiter(o, info)) {
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
      x = new data.Symbol(r)
      x.loc = o.loc(s, o.position())
      x.whitespace = info.whitespace
      info.whitespace = isWhitespace(r)
      /*if (escaped) {
        x = new Bypass(x)
      }*/
    }
    return x
  }
  
  // TODO: multi-character tokenize and delimiter
  function one(o, info) {
    if (!o.has()) {
      // TODO better error message; probably use SyntaxError
      throw new Error("unexpected end of input")
    }
    if (isDelimiter(o, info)) {
      var s = o.position()
        , y = new data.Symbol(o.read())
      y.loc = o.loc(s, o.position())
      y.whitespace = info.whitespace
      info.whitespace = true // TODO !!x.whitespace ?
      return y
    } else {
      return numOrSym(o, info)
    }
  }

  function tokenize1(o, info) {
    var a = indent(o)
      , i = 0

    function init() {
      if (i >= a.length && !o.has()) {
        a = deindent()
        i = 0
      }
      while (i >= a.length && o.has()) {
        var c = o.peek()
          , x
        if (c === " ") {
          o.read()
          a = []
          info.whitespace = true
        } else if (c === "\n") {
          o.read()
          a = indent(o)
          info.whitespace = true
        // TODO: multi-character tokenize and delimiter
        } else if ((x = box.getSyntax(c)) !== null) {
          var s1 = o.position()
          if (x.tokenize != null) {
            a = x.tokenize(o, info)
            info.whitespace = !!x.whitespace // TODO shouldn't this always be applied even if it doesn't have a tokenize method ?
          } else {
            a = [one(o, info)]
          }
          if (x.indent) {
            //var s2 = o.position()
            // TODO ew
            while (o.peek() === " ") {
              o.read()
              info.whitespace = true
            }
            if (o.peek() !== "\n") {
              a = a.concat(pushIndent(o.position().column))
              // TODO is this correct ?
              //indentStack.push(o.position().column)
              //a.push(new data.ParseIndent())

              /*x = {}
              x.loc = o.loc(s1, s2)
              throw new data.Error(x, "missing expression on the right side")*/
            }
          }
        } else {
          a = [one(o, info)]
        }
        i = 0
      }
    }

    info.iterator = {
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

    return info.iterator
  }
  
  function tokenize(x, filename) {
    return tokenize1(new buffer.Buffer(x, filename), { whitespace: true })
  }
  
  return {
    tokenize: tokenize,
    one: one,
    comment: comment,
    string: string,
    symbol: symbol,
  }
})
