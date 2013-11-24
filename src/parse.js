define(["./data", "./box", "./error"], function (data, box, error) {
  "use strict";

  function isVertical(x, y) {
    return y instanceof data.Symbol && y.value === x.value && y.loc.start.column === x.loc.start.column
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
        , y
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
              l.push(indent1(o, o.peek(), null))
            }
          } else {
            throw new Error("invalid indentation")
          }
        }
      } else if (start === null || v.loc.start.line === start.loc.start.line) {
        o.read()
        l.push(x)
        if (x instanceof data.Symbol && (y = box.getSyntax(x.value)) !== null) {
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
                  if (y.indent) {
                    a.push(data.unwrap(indent1(o, o.peek(), y.endAt)))
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
              if (y.indent) {
                // TODO needs to check o.has()
                a.push(data.unwrap(indent1(o, o.peek(), end)))
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
            if (y.indent) {
              // TODO needs to check o.has()
              l.push(data.unwrap(indent1(o, o.peek(), end)))
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
    var x = o.array[o.index]
    ++o.index
    var y = parse1({ array: x, index: 0 }, null)
    if (unwrap) {
      return data.unwrap(y)
    } else {
      return y
    }
  }
  
  // Heavily modified Pratt parser, designed for lists of symbols rather than expressions of tokens
  function parse1(o, pri) {
    var l = []
    while (o.index < o.array.length) {
      var x = o.array[o.index]
        , y
      if (x instanceof data.ParseBypass) {
        ++o.index
        l.push(x.value)
      } else if (Array.isArray(x)) {
        l.push(parseArray(o, true))
      } else if (x instanceof data.Symbol && (y = box.getSyntax(x.value)) !== null) {
        var pri2 = y.priority
        if (pri2 == null) {
          pri2 = 0
        }
        if (pri === null || pri2 > pri) {
          if (y.associativity === "right") {
            --pri2
          }
          if (y.parse != null) {
            ++o.index
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
        ++o.index
        l.push(x)
      }
    }
    return l
  }
  
  function indent(o) {
    return indent1(o, o.peek(), null)
  }

  function parse(x) {
    return data.unwrap(parse1({ array: x, index: 0 }, null))
  }

  return {
    parse: parse,
    indent: indent,
  }
})
