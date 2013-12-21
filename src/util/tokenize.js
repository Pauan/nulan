define(["../data", "../error"], function (data, error) {
  "use strict";

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

  return {
    comment: comment,
    string: string,
  }
})