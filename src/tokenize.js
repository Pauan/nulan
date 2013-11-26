define(["../lib/util/buffer", "./data", "./box", "./error", "../lib/util/iter"], function (buffer, data, box, error, iter) {
  "use strict";
  
  function isDelimiter(o, info) {
    if (o.has()) {
      var c = o.peek()
      /*if (c === " " || c === "\n") {
        return true
      } else {*/
      var x = box.getSyntax(c)
      return x !== null && x.delimiter
      //}
    } else {
      return true
    }
  }
  
  function isWhitespace(s) {
    /*if (s === " " || s === "\n") {
      return true
    } else {*/
    var x = box.getSyntax(s)
    if (x !== null) {
      return !!x.whitespace
    } else {
      return false
    }
    //}
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
        error(sFirst, "missing ending |#")
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

  function tokenizeBrackets(o, info) {
    var stack = []
      , r     = []
    while (o.has()) {
      // TODO is this correct ?
      r = r.concat(getNext(o, info, stack))
      if (stack.length === 0) {
        break
      }
    }
    return r
  }

  function readIndentedString(i, o) {
    ++i
    while (i-- && o.has()) {
      var s = o.position()
      var c = o.read()
      // TODO don't hardcore " " ?
      if (c !== " ") {
        var x = {}
        x.loc = o.loc(s, o.position())
        error(x, "expected space but got " + c)
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
            a = []
          }
          x = new data.Symbol(q)
          x.loc = o.loc(sFirst, o.position())
          r.push(x)

          /*if (r.length === 3 && r[1] instanceof data.String) {
            return [r[1]]
          } else {
            
          }*/
          return r
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
              error(x, "expected \\ \\r \\n \\t \\" + q + " \\@ \\\\ but got \\" + c)
            }
          })(o.position())
        } else if (c === "@") {
          //r[0] = enrich(r[0], s, o)
          //tokenUpdate(enrich({}, s, o), function (o) {
          //  o.type = "string"
          //})

          // TODO code duplication
          if (a.length) {
            x = new data.String(a.join(""))
            x.loc = o.loc(s, o.position())
            r.push(x)
            a = []
          }

          o.read()
          r.push(new data.ParseStart())
          r = r.concat(tokenizeBrackets(o, info))
          r.push(new data.ParseEnd())
          //r.push(new data.ParseBypass(parse1(tokenizeBrackets(info))))
          s = o.position()
        } else if (c === "\n") {
          a.push(o.read())
          readIndentedString(sFirst.column, o)
        } else {
          a.push(o.read())
        }
      } else {
        x = {}
        x.loc = o.loc(sFirst, sNext)
        error(x, "missing ending " + q)
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
              error(x, "expected \\ \\r \\n \\t \\" + q + " \\\\ but got \\" + c)
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
        error(x, "missing ending " + q)
      }
    }
  }

  function numOrSym(o, info) {
    var s = o.position()
      , r = []
      , x
    while (o.has() && !isDelimiter(o, info)) {
      r.push(o.read())
    }
    r = r.join("")
    if (/^[0-9]+$/.test(r)) {
      x = new data.Number(+r)
      x.whitespace = info.whitespace
      info.whitespace = false
    } else {
      x = new data.Symbol(r)
      x.whitespace = info.whitespace
      info.whitespace = isWhitespace(r)
    }
    x.loc = o.loc(s, o.position())
    return x
  }
  
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
  
  function findIndent(o, info) {
    // TODO don't hardcode space and newline ?
    while (o.has() && (o.peek() === " " || o.peek() === "\n")) {
      o.read()
      info.whitespace = true
    }
    return o.position()
  }

  // TODO hacky
  // TODO is this correct ?
  function popEndAt(y, endAt) {
    var i = 0
    while (endAt.length && i < y.length) {
      var last = endAt[endAt.length - 1]
      if (data.boxOrSym(y[i]) && y[i].value === last) {
        endAt.pop()
      }
      ++i
    }
  }
  
  function getNext(o, info, endAt) {
    var c = o.peek()
      , x = box.getSyntax(c)
      , y
    // TODO: multi-character tokenize and delimiter
    if (x !== null) {
      if (x.tokenize != null) {
        y = x.tokenize(o, info)
      } else {
        y = [one(o, info)]
      }
      if (x.endAt != null) {
        endAt.push(x.endAt)
      }
      info.whitespace = !!x.whitespace
    } else {
      y = [one(o, info)]
    }
    popEndAt(y, endAt)
    return y
  }

  // TODO multi-character tokenize and delimiter
  function tokenize1(o) {
    var a = []
    
    var endAt = []
      , info  = { whitespace: true }
      , start = findIndent(o, info)
    
    while (o.has()) {
      var next = findIndent(o, info)
      if (start.line === next.line   ||
          next.column > start.column ||
          endAt.length) {
        a = a.concat(getNext(o, info, endAt))
      } else {
        break
      }
    }
    
    return a
  }
  
  function tokenize(x, filename) {
    var o = new buffer.Buffer(x, filename)

    var oIter = {}
    oIter[iter.iterator] = function () {
      return this
    }
    oIter.next = function () {
      if (!o.has()) {
        throw new iter.StopIteration()
      } 
      return tokenize1(o)
    }
    return oIter
  }
  
  return {
    tokenize: tokenize,
    //one: one,
    comment: comment,
    string: string,
    symbol: symbol,
  }
})
