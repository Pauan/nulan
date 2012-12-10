var NINO = (function (n) {
  "use strict";

  function flatten1(x) {
    // TODO: is there a faster way?
    var r = []
    return r.concat.apply(r, x)
  }

  function expression(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "function":
      case "function-statement":
        return [x[0], x[1], x[2], block(x[3])]

      case "if":
        return [x[0], x[1],
                 block(x[2]),
                 block(x[3])]

      case "while":
        return [x[0], x[1], block(x[2])]

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
        return [].concat(loop(x[1]), loop(x[2]))

      case "=":
        y = loop(x[2])
        y[y.length - 1] = [x[0], x[1], y[y.length - 1]]
        return y

      case "return":
        y = loop(x[1])
        y.push.apply(y, returns([x[0], y.pop()]))
        return y

      default:
        return [expression(x)]
      }
    }
    return flatten1(x.map(loop))
  }

  function returns(x) {
    var z, y = x[1]

    switch (y[0]) {
    case "if":
      z = y[2]
      z.push.apply(z, returns([x[0], z.pop()]))

      z = y[3]
      z.push.apply(z, returns([x[0], z.pop()]))
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
