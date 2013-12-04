define(["./data", "./box", "./error"], function (data, box, error) {
  "use strict";
  
  // TODO generic
  function arrayToIter(x) {
    var i = 0
    return {
      has: function () {
        return i < x.length
      },
      peek: function () {
        return x[i]
      },
      // TODO super hacky
      unpeek: function () {
        return x[i - 2]
      },
      read: function () {
        return x[i++]
      }
    }
  }

  function isVertical(x, y) {
    return data.boxOrSym(y) && y.value === x.value && y.loc.start.column === x.loc.start.column
  }
  
  function unwrap(x) {
    if (x instanceof data.ParseBypass) {
      return x.value
    } else {
      return x
    }
  }
  
  function indent1(o, start, end) {
    var l = []
    start = unwrap(start)
    while (o.has()) {
      var x = o.peek()
        , v = unwrap(x)
      if (end != null && data.isSym(x, end)) {
        break
      } else if (x instanceof data.ParseEnd) {
        break
      } else if (x instanceof data.ParseStart) {
        o.read()
        while (true) {
          if (o.has()) {
            if (o.peek() instanceof data.ParseEnd) {
              o.read()
              break
            } else {
              // TODO is this correct ?
              l.push(indent1(o, o.peek(), null))
            }
          } else {
            throw new Error("invalid indentation")
          }
        }
      } else if (start === null || v.loc.start.line === start.loc.start.line) {
        o.read()
        l.push(x)
        var y = box.getSyntax(x)
        if (y !== null) {
          if (y.startAt != null) {
            if (end == null) {
              error(x, "missing starting " + y.startAt)
            } else {
              error(x, "expected " + end + " but got " + x.value)
            }
          }
          if (y.endAt != null) {
            var a = []
            while (true) {
              if (o.has()) {
                if (data.isSym(o.peek(), y.endAt)) {
                  break
                } else {
                  if (y.indent === "right") {
                    a.push(data.unwrap(indent1(o, o.peek(), y.endAt)))
                  } else if (y.indent === "left") {
                    a.push(data.unwrap(indent1(o, o.unpeek(), y.endAt)))
                  } else {
                    a = a.concat(indent1(o, null, y.endAt))
                  }
                }
              } else {
                error(x, "missing ending " + y.endAt)
              }
            }
            l.push(a)
            l = l.concat(indent1(o, o.read(), end))
          } else if (y.vertical) {
            var a = []
            while (true) {
              if (y.indent === "right") {
                a.push(data.unwrap(indent1(o, o.peek(), end)))
              } else if (y.indent === "left") {
                a.push(data.unwrap(indent1(o, o.unpeek(), end)))
              } else {
                a = a.concat(indent1(o, null, end))
              }
              if (o.has() && isVertical(x, o.peek())) {
                o.read()
              } else {
                break
              }
            }
            l.push(a)
          } else {
            if (y.indent === "right") {
              l.push(data.unwrap(indent1(o, o.peek(), end)))
            } else if (y.indent === "left") {
              l.push(data.unwrap(indent1(o, o.unpeek(), end)))
            } else {
              l = l.concat(indent1(o, start, end))
            }
          }
        }
      } else if (v.loc.start.column > start.loc.start.column) {
        l.push(data.unwrap(indent1(o, x, end)))
      } else {
        break
      }
    }
    return l
  }
  
  function parseArray(o, unwrap) {
    var y = parse1(arrayToIter(o.read()), null)
    if (unwrap) {
      return data.unwrap(y)
    } else {
      return y
    }
  }
  
  // Heavily modified Pratt parser, designed for lists of symbols rather than expressions of tokens
  function parse1(o, pri) {
    var l = []
    while (o.has()) {
      var x = o.peek()
      if (x instanceof data.ParseBypass) {
        o.read()
        l.push(x.value)
      } else if (Array.isArray(x)) {
        l.push(parseArray(o, true))
      } else {
        var y = box.getSyntax(x)
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
  
  function indent(x) {
    var o = arrayToIter(x)
    return indent1(o, o.peek(), null)
  }

  function parse(x) {
    var o = arrayToIter(x)
    return data.unwrap(parse1(o, null))
  }

  return {
    parse: parse,
    indent: indent,
  }
})
