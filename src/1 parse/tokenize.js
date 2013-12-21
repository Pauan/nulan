define(["../../lib/util/buffer", "../../lib/util/iter", "../0 data/number", "../0 data/symbol", "../0 data/box", "../util/print"], function (a, b, c, d, e, f) {
  "use strict";
  
  var Buffer        = a.Buffer
    , iterator      = b.iterator
    , StopIteration = b.StopIteration
    , Number        = c.Number
    , Symbol        = d.Symbol
    , getSyntax     = e.getSyntax
    , isBoxOrSym    = e.isBoxOrSym
    , error         = f.error

  function isDelimiter(o, info) {
    if (o.has()) {
      var c = o.peek()
      /*if (c === " " || c === "\n") {
        return true
      } else {*/
      var x = getSyntax(c)
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
    var x = getSyntax(s)
    if (x !== null) {
      return !!x.whitespace
    } else {
      return false
    }
    //}
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
      x = new Number(+r)
      x.whitespace = info.whitespace
      info.whitespace = false
    } else {
      x = new Symbol(r)
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
        , y = new Symbol(o.read())
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
      if (isBoxOrSym(y[i]) && y[i].value === last) {
        endAt.pop()
      }
      ++i
    }
  }
  
  function getNext(o, info, endAt) {
    var c = o.peek()
      , x = getSyntax(c)
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
    var o = new Buffer(x, filename)

    var oIter = {}
    oIter[iterator] = function () {
      return this
    }
    oIter.next = function () {
      // TODO a little hacky
      var x = []
      while (x.length === 0) {
        if (!o.has()) {
          throw new StopIteration()
        }
        x = tokenize1(o)
      }
      return x
    }
    return oIter
  }

  return {
    tokenize: tokenize
  }
})