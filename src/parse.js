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
  function parse1(o, end, i) {
    var l = []

    //var l = getLeft(o, first)
    while (o.has()) {
      var x = o.peek()
      if (x instanceof data.ParseBypass) {
        o.read()
        l.push(x.value)
      } else if (x instanceof data.ParseIndent) {
        o.read()
        l.push(data.unwrap(parse1(o, end, i)))
      } else if (x instanceof data.ParseDedent) {
        o.read()
        break
      } else {
        if (end != null && data.isSym(o.peek(), end)) {
          break
        }
        var y
        if (x instanceof data.Symbol && (y = box.getSyntax(x.value)) !== null) {
          var pri = y.priority || 0
          if (i === null || pri > i) {
            o.read()
            var r = []
            if (y.endAt != null) {
              var a = []
              while (true) {
                if (o.has()) {
                  console.log(o.peek())
                  if (data.isSym(o.peek(), y.endAt)) {
                    o.read()
                    break
                  }
                } else {
                  break
                }
                /*if (y.indent) {
                  a.push(parse1(o, stack, null))
                } else {
                  
                }*/
                a = a.concat(parse1(o, y.endAt, null))
              }
              r.push(a)
            }
            r = r.concat(parse1(o, end, pri))
            l = y.parse(l, x, r)
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

  function checkStack(stack, o) {
    if (stack.length) {
      var last = stack[stack.length - 1]
      if (o.has()) {
        var x = o.peek()
        throw new data.Error(x, "expected " + last.endAt + " but got " + x.value)
      } else {
        throw new data.Error(last.symbol, "missing ending " + last.endAt)
      }
    }
  }

  function parse(o) {
    console.assert(o.peek() instanceof data.ParseIndent)
    o.read()
    var x = data.unwrap(parse1(o, null, null))
    //console.log(o.peek())
    //o.read()
    //checkStack(stack, o)
    return x
  }

  return parse
})
