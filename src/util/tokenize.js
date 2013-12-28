define(["./print", "./data", "./util", "../1 parse/tokenize"], function (a, b, c, d) {
  "use strict";

  var error            = a.error
    , Symbol           = b.Symbol
    , String           = b.String
    , ParseStart       = b.ParseStart
    , ParseEnd         = b.ParseEnd
    , unwrap           = c.unwrap
    , tokenizeBrackets = d.tokenizeBrackets

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

  function readIndentedString(q, i, o) {
    ++i
    while (i-- && o.has()) {
      var c = o.peek()
      // TODO don't hardcore " " ?
      if (c === q) {
        break
      } else {
        var s = o.position()
        o.read()
        if (c !== " ") {
          var x = {}
          x.loc = o.loc(s, o.position())
          error(x, "expected space but got " + c)
        }
      }
    }
  }

  function string(o, info) {
    var s = o.position()
      , q = o.read()
      , r = [new Symbol(q)]
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
            x = new String(a.join(""))
            x.loc = o.loc(s, o.position())
            r.push(x)
            a = []
          }
          x = new Symbol(q)
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
              readIndentedString(q, sFirst.column, o)
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
            x = new String(a.join(""))
            x.loc = o.loc(s, o.position())
            r.push(x)
            a = []
          }

          o.read()
          r.push(new ParseStart())
          r = r.concat(tokenizeBrackets(o, info))
          r.push(new ParseEnd())
          //r.push(new data.ParseBypass(parse1(tokenizeBrackets(info))))
          s = o.position()
        } else if (c === "\n") {
          a.push(o.read())
          readIndentedString(q, sFirst.column, o)
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
/*
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
  }*/
  
  
  // TODO not really specific to tokenize
  function missingLeft(s) {
    error(s, "missing expression on the left side of ", [s])
  }
  
  function missingRight(s) {
    error(s, "missing expression on the right side of ", [s])
  }

  function unary(o) {
    if (o == null) {
      o = {}
    }
    if (o.associativity == null) {
      o.associativity = "right"
    }
    if (o.parse == null) {
      o.parse = function (l, s, r) {
        if (r.length === 0) {
          missingRight(s)
        } else {
          var y = r[0]
          // TODO is this correct ?
          if (o.indent) {
            return l.concat([[s, unwrap(y)]], r.slice(1))
          } else {
            return l.concat([[s, y]], r.slice(1))
          }
        }
      }
    }
    return o
  }

  function infix(o) {
    if (o == null) {
      o = {}
    }
    if (o.parse == null) {
      o.parse = function (l, s, r) {
        var y = r[0]
        if (l.length === 0) {
          missingLeft(s)
          //return [[s, y]].concat(r.slice(1))
        } else if (r.length === 0) {
          missingRight(s)
        } else {
          var x = l[l.length - 1]
          return l.slice(0, -1).concat([[s, x, y]], r.slice(1))
        }
      }
    }
    return o
  }
  
  function whitespace(x) {
    if (x == null) {
      x = {}
    }
    if (x.delimiter == null) {
      x.delimiter = true
    }
    if (x.whitespace == null) {
      x.whitespace = true
    }
    if (x.tokenize == null) {
      x.tokenize = function (o) {
        o.read()
        return []
      }
    }
    return x
  }

  return {
    comment: comment,
    string: string,
    
    missingLeft: missingLeft,
    missingRight: missingRight,
    unary: unary,
    infix: infix,
    whitespace: whitespace,
  }
})