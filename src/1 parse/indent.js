define(["../0 data/symbol", "../0 data/array", "../0 data/parse", "../0 data/box", "../util/print"], function (a, b, c, d, e) {
  "use strict";

  var isSymbol    = a.isSymbol
    , unwrap      = b.unwrap
    , arrayToIter = b.arrayToIter
    , ParseBypass = c.ParseBypass
    , ParseStart  = c.ParseStart
    , ParseEnd    = c.ParseEnd
    , getSyntax   = d.getSyntax
    , isBoxOrSym  = d.isBoxOrSym
    , error       = e.error

  function isVertical(x, y) {
    return isBoxOrSym(y) && y.value === x.value && y.loc.start.column === x.loc.start.column
  }
  
  function unwrapBypass(x) {
    if (x instanceof ParseBypass) {
      return x.value
    } else {
      return x
    }
  }
  
  function indent1(o, start, end) {
    var l = []
    start = unwrapBypass(start)
    while (o.has()) {
      var x = o.peek()
        , v = unwrapBypass(x)
      if (end != null && isSymbol(x, end)) {
        break
      } else if (x instanceof ParseEnd) {
        break
      } else if (x instanceof ParseStart) {
        o.read()
        while (true) {
          if (o.has()) {
            if (o.peek() instanceof ParseEnd) {
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
        var y = getSyntax(x)
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
                if (isSymbol(o.peek(), y.endAt)) {
                  break
                } else {
                  if (y.indent === "right") {
                    a.push(unwrap(indent1(o, o.peek(), y.endAt)))
                  } else if (y.indent === "left") {
                    a.push(unwrap(indent1(o, o.unpeek(), y.endAt)))
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
                a.push(unwrap(indent1(o, o.peek(), end)))
              } else if (y.indent === "left") {
                a.push(unwrap(indent1(o, o.unpeek(), end)))
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
              l.push(unwrap(indent1(o, o.peek(), end)))
            } else if (y.indent === "left") {
              l.push(unwrap(indent1(o, o.unpeek(), end)))
            } else {
              l = l.concat(indent1(o, start, end))
            }
          }
        }
      } else if (v.loc.start.column > start.loc.start.column) {
        l.push(unwrap(indent1(o, x, end)))
      } else {
        break
      }
    }
    return l
  }

  function indent(x) {
    var o = arrayToIter(x)
    return indent1(o, o.peek(), null)
  }

  return {
    indent: indent,
  }
})