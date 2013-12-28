define(["../util/data", "../util/util", "../util/print"], function (a, b, c) {
  "use strict";

  var ParseBypass = a.ParseBypass
    , arrayToIter = b.arrayToIter
    , unwrap      = b.unwrap
    , getSyntax   = b.getSyntax
    , error       = c.error

  function parseArray(o, isUnwrap) {
    var y = parse1(arrayToIter(o.read()), null)
    if (isUnwrap) {
      return unwrap(y)
    } else {
      return y
    }
  }

  // Heavily modified Pratt parser, designed for lists of symbols rather than expressions of tokens
  function parse1(o, pri) {
    var l = []
    while (o.has()) {
      var x = o.peek()
      if (x instanceof ParseBypass) {
        o.read()
        l.push(x.value)
      } else if (Array.isArray(x)) {
        l.push(parseArray(o, true))
      } else {
        var y = getSyntax(x)
        if (y !== null) {
          var pri2 = y.priority
          if (pri2 == null) {
            pri2 = 0
          }
          if (pri === null || pri2 > pri) {
            if (y.associativity === "right") {
              --pri2
            }
            if (y.parse != null) {
              o.read()
              var r = []
              if (y.endAt != null || y.vertical) {
                r.push(parseArray(o, false))
              }
              r = r.concat(parse1(o, pri2))
              l = y.parse(l, x, r)
            } else {
              error(x, [x], " has a syntax rule but doesn't have a parse function")
            }
          } else {
            break
          }
        } else {
          o.read()
          l.push(x)
        }
      }
    }
    return l
  }

  function parse(x) {
    var o = arrayToIter(x)
    return unwrap(parse1(o, null))
  }

  return {
    parse: parse,
  }
})