var NINO = (function (n) {
  "use strict";

  var statements = {}

  ;("break continue debugger do for for-in function-statement " + // if
    "return switch throw try var while with")
    .split(" ").forEach(function (s) { statements[s] = true })

  function flatten1(x) {
    // TODO: is there a faster way?
    var r = []
    return r.concat.apply(r, x)
  }

  // TODO: make this non-recursive?
  function isStatement(x) {
    if (Array.isArray(x)) {
      if (statements[x[0]]) {
        return true
      } else {
        // TODO
        //return false
        return x.some(isStatement)
      }
    } else {
      return false
    }
  }

  function blockToExpression(x) {
    return x.map(expression).reduce(function (x, y) {
      return [",", x, y]
    })
  }

  function expression(x) {
    var y
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "function":
      case "function-statement":
        return [x[0], x[1], x[2], block(x[3])]

      case "if":
        y = expression(x[1])
        if (x[2].some(isStatement) || x[3].some(isStatement)) {
          return [x[0], y,
                   block(x[2]),
                   block(x[3])]
        } else {
          if (x[2].length) {
            if (x[3].length) {
              return ["?:", y,
                       blockToExpression(x[2]),
                       blockToExpression(x[3])]
            } else {
              return ["&&", y, blockToExpression(x[2])]
            }
          } else {
            return ["empty"]
          }
        }

      case "while":
        y = expression(x[1])
        if (booleans[y[0]]) {
          return toBoolean(y)
                   ? [x[0], ["boolean", "true"], block(x[2])]
                   : blockToExpression(x[2]) // TODO
        } else {
          return [x[0], y, block(x[2])]
        }

/*
      ->
        | var i = 0
        | if i
            i + 10
            i + 20


var foo

foo <=
| 5
| 10
| 20

*/

      case "try":
        return [x[0], block(x[1])].concat(x.slice(2).map(function (x) {
          if (x[0] === "catch") {
            return [x[0], x[1], block(x[2])]
          } else if (x[0] === "finally") {
            return [x[0], block(x[1])]
          }
        }))

      default:
        return x.map(expression)
      }
    } else {
      return x
    }
  }

  function block(x) {
    function loop(x) {
      var y
      switch (x[0]) {

      case ",":
        return loop(x[1]).concat(loop(x[2]))

      case "=":
        y = loop(x[2])
        y.push([x[0], x[1], y.pop()])
        return y

      case "return":
        y = loop(x[1])
        y.push.apply(y, returns([x[0], y.pop()]))
        return y

      default:
        return [expression(x)]
      }
    }
    return flatten1(x.map(loop))/*.filter(function (x) {
      return !constants[x[0]]
    })*/
  }

  function returns(x) {
    var z, y = x[1]

    switch (y[0]) {
    case "if":
      z = y[2]
      if (z.length) {
        z.push.apply(z, returns([x[0], z.pop()]))
      }

      z = y[3]
      if (z.length) {
        z.push.apply(z, returns([x[0], z.pop()]))
      }
      return [y]

    case "while":
      return [y]

    case "try":
      z = y[1]
      z[z.length - 1] = returns([x[0], z[z.length - 1]])
      return [y]

    case "throw":
      return [y]

    case "var":
      z = y[1]
      z = ["name", z[z.length - 1][0]]
      return [y, [x[0], z]]

    default:
      return [x]
    }
  }

  n.transform = function (a) {
    return block(a)
  }

  return n
})(NINO || {})
