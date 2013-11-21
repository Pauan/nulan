define(["./data", "./box"], function (data, box) {
  "use strict";

  function isVertical(x, y) {
    return y instanceof data.Symbol && y.value === x.value && y.loc.start.column === x.loc.start.column
  }

  function vertical(x, y, o, stack) {
    var a = []
    // TODO code duplication
    if (y.indent) {
      a.push(parse1(o, stack, null))
    } else {
      a = a.concat(parse1(o, stack, null))
    }
    while (o.has() && isVertical(x, o.peek())) {
      if (y.indent) {
        o.read()
        a.push(parse1(o, stack, null))
      } else {
        o.read()
        a = a.concat(parse1(o, stack, null))
      }
    }
    return a
  }

  // Heavily modified Pratt parser, designed for lists of symbols rather than expressions
  // TODO simulates an iterator using an object with an `array` and `index` property; should either use actual iterators, or something else
  function parse1(o, pri1) {
    var l = []

    while (o.index < o.array.length) {
      var x = o.array[o.index]
        , y
      if (x instanceof data.ParseBypass) {
        ++o.index
        l.push(x.value)
      } else if (Array.isArray(x)) {
        ++o.index
        l.push(parse1({ array: x, index: 0 }, null))
      } else if (x instanceof data.Symbol && (y = box.getSyntax(x.value)) !== null) {
        var pri2 = y.priority || 0
        if (pri1 === null || pri2 > pri1) {
          ++o.index
          if (y.associativity === "right") {
            --pri2
          }
          l = y.parse(l, x, parse1(o, pri2))
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
  
  function unwrap(x) {
    if (x instanceof data.ParseBypass) {
      return x.value
    } else {
      return x
    }
  }
  
  // TODO endAt syntax can't include things the same priority or lower
  // e.g. [foo & bar] where & is the same priority or lower than [
  function parse1(l, o, x, end) {
    o.read()
    l.push(x)
    var y
    if (x instanceof data.Symbol && (y = box.getSyntax(x.value)) !== null) {
      if (y.startAt != null) {
        if (end == null) {
          throw new data.Error(x, "missing starting " + y.startAt)
        } else {
          //checkStack(stack, o)
          throw new data.Error(x, "expected " + end + " but got " + x.value)
        }
      }
      if (y.parse != null) {
        if (y.endAt != null) {
          var r = []
          while (true) {
            if (o.has()) {
              if (data.isSym(o.peek(), y.endAt)) {
                o.read()
                //start = unwrap(o.peek())
                break
              } else {
                if (y.indent) {
                  r.push(data.unwrap(indent(o, o.peek(), y.endAt)))
                } else {
                  ;(function (end) {
                    while (o.has()) {
                      var x = o.peek()
                      if (end != null && data.isSym(x, end)) {
                        break
                      } else {
                        indent2(r, o, x, end)
                      }
                    }
                  })(y.endAt)
                  //indent1(r, o, o.peek(), y.endAt)
                  //r = r.concat(indent(o, o.peek(), y.endAt))
                }
              }
            } else {
              throw new data.Error(x, "missing ending " + y.endAt)
            }
          }
          l.push(r)
          // TODO why doesn't this work ?
          //l = l.concat(indent(o, o.peek(), end))
          indent1(l, o, o.peek(), end)
        } else {
          if (y.indent) {
            l.push(data.unwrap(indent(o, o.peek(), end)))
          }
        }
      } else {
        throw new data.Error(x, data.print(x) + " has a syntax rule but doesn't have a parse function")
      }
    }
  }
  
  function indent1(l, o, start, end) {
    start = unwrap(start)
    while (o.has()) {
      var x = o.peek()
        , v = unwrap(x)
      if (v.loc.start.line === start.loc.start.line) {
        if (end != null && data.isSym(x, end)) {
          break
        } else {
          indent2(l, o, x, end)
        }
      } else if (v.loc.start.column > start.loc.start.column) {
        l.push(data.unwrap(indent(o, o.peek(), null)))
      } else {
        break
      }
    }
  }
  
  function indent(o, start, end) {
    var l = []
    indent1(l, o, start, end)
    return l
  }

  function parse(o) {
    var x = indent(o, o.peek(), null)
    //return x
    //console.log(data.print(x))
    return data.unwrap(parse1({ array: x, index: 0 }, null))
  }

  return parse
})
