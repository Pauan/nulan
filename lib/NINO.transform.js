var NINO = (function (n) {
  "use strict";

  function isPure(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "eempty":
      case "name":
      case "number":
      case "boolean":
      case "null":
      case "regexp":
      case "string":
        return true

      case ".":
      case "!":
      case "~":
      case "u+":
      case "u-":
      case "typeof":
      case "void":
        return isPure(x[1])

      case "[]":
      case "*":
      case "/":
      case "%":
      case "+":
      case "-":
      case "<<":
      case ">>":
      case ">>>":
      case "<":
      case "<=":
      case ">":
      case ">=":
      case "in":
      case "instanceof":
      case "==":
      case "!=":
      case "===":
      case "!==":
      case "&":
      case "^":
      case "|":
      case "&&":
      case "||":
      case ",":
        return isPure(x[1]) && isPure(x[2])

      case "?:":
        return isPure(x[1]) && isPure(x[2]) && isPure(x[3])

      case "array":
        return x[1].every(isPure)

      case "object":
        return x[1].every(function (x) {
          return isPure(x[1])
        })

      default:
        return false
      }
    } else {
      return false
    }
  }

  // TODO: make this non-recursive?
  function isStatement(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "break":
      case "continue":
      case "debugger":
      case "do":
      case "for":
      case "for-in":
      case "function-statement":
      //case "if":
      case "return":
      case "switch":
      case "throw":
      case "try":
      case "var":
      case "while":
      case "with":
      case:
        return true

      default:
        return x.some(isStatement)
      }
    } else {
      return false
    }
  }

  function flatten1(x) {
    // TODO: is there a faster way?
    var r = []
    return r.concat.apply(r, x)
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


# if/switch/try/with
  if (throw foo)

  throw foo
  if undefined


  return if ...
           ...
           ...

  if ...
    return ...
    return ...

  return ... ? ... : ...


# while/for/for..in/do..while
  return while () {}

  while () {}
  return void 0

# debugger
  return debugger

  return debugger

# var/throw/break/continue/return
  foo (bar 2) (return;)

  foo (bar 2) (return;)


  return throw 5

  throw 5
  return


  foo (bar 1) (throw 5)

  foo (bar 1) (-> (throw 5))()


  foo (var foo = 10) 2 (bar 1)

  var foo = 10
  foo foo 2 (bar 1)


  foo (bar 1) 2 (var foo = 10)

  var foo
  foo (bar 1) 2 (foo <= 10)


  ". [] new call * / % + - << >> >>> < <= > >= in instanceof == != === !== & ^ | && || ?: = += -= *= /= %= <<= >>= >>>= &= ^= |= ,".split(" ")


  n.transform = function (a) {
    return block(a)
  }

  return n
})(NINO || {})
