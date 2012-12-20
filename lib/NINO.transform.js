var NINO = (function (n) {
  "use strict";

  function isPure(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "id":
      /*case "break":
      case "continue":
      case "debugger":
      case "return":
      case "throw":
      case "var":*/
      case "new":
      case "call":
      case "++":
      case "--":
      case "delete":
      case "=":
      case "+=":
      case "-=":
      case "*=":
      case "/=":
      case "%=":
      case "<<=":
      case ">>=":
      case ">>>=":
      case "&=":
      case "^=":
      case "|=":
        return false

      default:
        return true
        //return x.every(isPure)
      }
    } else {
      return true
    }
/*
      switch (x[0]) {
      case "empty":
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
      }*/
  }

/*
  // TODO: make this non-recursive?
  function isStatement(x) {
    if (Array.isArray(x)) {
      switch (x[0]) {
      case "break":
      case "continue":
      case "debugger":
      case "do-while":
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
  }*/

  function blockToExpression(x) {
    return x.map(expression).reduce(function (x, y) {
      return [",", x, y]
    })
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

/*
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
*/

  /*function last(a, f) {
    if (a.length) {
      a.push(f(a.pop()))
    }
    return a
  }

  function expressionInner(x, r, f) {
    var y
    switch (x[0]) {
    case "if":
      last(x[2], f)
      last(x[3], f)
      r.push([x[0], x[1], x[2], x[3]])
      break

    case "try":
      last(x[1], f)
      r.push([x[0],
              x[1],
              x[2].map(function (x) {
                last(x[1], f)
                return [x[0], x[1]]
              }),
              x[3]])
      break

    case "var":
      y = x[1]
      y = y[y.length - 1][0]
      r.push(x)
      r.push(f(["name", y]))
      break

    case ",":
      r.push(x[1])
      r.push(f(x[2]))
      break

    default:
      r.push(f(x))
    }
  }

  function expression(x, r) {
    switch (x[0]) {
    case "var":
      x[1].forEach(function (y) {
        expressionInner(y[1], r, function (z) {
          return [x[0], [[y[0], z]]]
        })
      })
      break

    case "return":
      expressionInner(x[1], r, function (y) {
        return [x[0], y]
      })
      break

    default:
      r.push(x)
    }
  }*/



                 // TODO: would it be faster to replace this with an object?
  //var constant = /^(?:empty|name|number|boolean|null|regexp|string)$/
  //  , statement = /^(?:break|continue|debugger|do-while|for|for-in|function-statement|return|switch|throw|try|var|while|with)$/

  var counter = 0

  function uniq() {
    return "_" + counter++
  }

  function isConstant(x) {
    switch (x[0]) {
    case "empty":
    case "name":
    case "number":
    case "boolean":
    case "null":
    case "regexp":
    case "string":
      return true

    default:
      return false
    }
  }

  function toSSA(x, aSSA) {
    var a, r
    if (!Array.isArray(x) || isConstant(x)) {
      return x
    } else {
      switch (x[0]) {
      case "function":
      case "function-statement":
        return [x[0], x[1], x[2], block(x[3])]
/*
      case "if":
      case "try":
      case "for":
      case "while":
      case "switch":
      case "array":
      case "object":*/

      case "var":
        var r = [x[0]]
        x[1].forEach(function (x) {
          var a = [x[0]]
          aSSA.push({ expr: a, list: r })
          aSSA.push({ expr: expression(x[1], aSSA), list: a })
        })
        return r

      case "new":
      case "call":
        r = [x[0]]
        aSSA.push({ expr: expression(x[1], aSSA)
                  , list: r })
        a = []
        aSSA.push({ expr: a, list: r })
        x[2].forEach(function (x) {
          aSSA.push({ expr: expression(x, aSSA)
                    , list: a })
        })
        return r

      case ",":
        aSSA.push({ expr: expression(x[1], aSSA), list: aSSA })
        //aSSA.push({ expr: expression(x[2], aSSA), list: aSSA })
        return expression(x[2], aSSA)

      default:
        r = [x[0]]
        for (var i = 1, iLen = x.length; i < iLen; ++i) {
          aSSA.push({ expr: expression(x[i], aSSA)
                    , list: r })
        }
        return r
      }
    }
  }

/*
"call" foo {1 2 ("call" bar {3})}

1 2 [] "call" bar [] 3
[] "call" foo []

var _1 = foo
var _2 = 1
var _3 = 2
var _4 = bar
var _5 = 3
var _6 = {"call" _4 {_5}}
{"call" _1 {_2 _3 _6}}


"call" foo {{"call" bar {{"," 50 {"call" test {3}}}}} {"throw" 5} {"call" qux {10}}}

"call" bar {{"call" test {3}}}
"throw" 5


"throw" {"throw" 5}
*/

  function stopAt(a, f) {
    var r = []
    for (var i = 0, iLen = a.length; i < iLen; ++i) {
      r.push(a[i])
      if (f(a[i])) {
        break
      }
    }
    return r
  }

  function statementize(x, r) {
    var aSSA = []

    var e = expression(x, aSSA)
      , a = []
      , i, x, u
      , seen, halt

    console.log("EXPRESSION", e)
    console.log("SSA", aSSA)

    aSSA = stopAt(aSSA, function (x) {
      switch (x.expr[0]) {
      case "break":
      case "continue":
      case "return":
      case "throw":
        return true
      }
    })

    i = aSSA.length
    while (i) {
      x = aSSA[--i]
      switch (x.expr[0]) {
      case "break":
      case "continue":
      case "return":
      case "throw":
        seen = true
        halt = true
        a.push({ expr: x.expr, list: r })
        break

      case "debugger":
      case "do-while":
      case "for":
      case "for-in":
      case "while":
        seen = true
        a.push({ expr: ["empty"], list: x.list })
        a.push({ expr: x.expr, list: x.list })
        break

      // case "if":
      case "switch": // TODO
      case "try":
      case "with":
        seen = true
        break

      case "function-statement":
        seen = true
        a.push({ expr: ["name", y[1]], list: x.list })
        a.push({ expr: x.expr, list: r })
        break

      case "var":
        seen = true
        a.push({ expr: ["name", y[1][y[1].length - 1][0]], list: x.list })
        a.push({ expr: x.expr, list: r })
        break

      default:
        if (seen) {
          if (halt) {
            if (isPure(x.expr)) {
              a.push(x)
            } else {
              a.push({ expr: x.expr, list: r })
            }
          } else {
            u = uniq()
            a.push({ expr: ["name", u], list: x.list })
            a.push({ expr: ["var", [[u, x.expr]]], list: r })
          }
        } else {
          a.push(x)
        }
      }
      /*if (statement.test(x[1][0])) {
        seen = true
      }*/
    }

    //console.log("END", a)

    i = a.length
    while (i) {
      x = a[--i]
      x.list.push(x.expr)
      //x[1].push(x[0])
    }

    //if (!halt) {
      r.push(e)
    //}

    console.log("END", r)
  }

  function block(x) {
    var r = []
    x.forEach(function (x) {
      statementize(x, r)
    })
    return r
  }

/*
statementize(["call", ["name", "foo"],
                      ["number", "1"],
                      ["number", "2"],
                      ["number", "3"],
                      ["var", ["x", ["call", ["name", "bar"],
                                             ["number", "20"]]]],
                      ["number", "4"],
                      ["number", "5"]], r)

var r = []
statementize(["call", ["name", "foo"],
                      ["number", "1"],
                      ["number", "2"],
                      ["number", "3"],
                      ["call", ["name", "bar"],
                               ["number", "10"]],
                      ["var", ["x", ["number", "20"]]]], r)
r

statementize(["call", ["name", "foo"],
                      ["number", "1"],
                      ["number", "2"],
                      ["number", "3"],
                      ["call", ["name", "bar"],
                               ["throw", ["number", "10"]]],
                      ["throw", ["number", "20"]],
                      ["call", ["name", "qux"],
                               ["call", ["name", "nou"],
                                        ["number", "30"]]],
                      ["call", ["name", "yes"],
                               ["throw", ["number", "50"]]],
                      ["call", ["name", "corge"],
                               ["number", "40"]]])
*/
/*
  foo(1, 2, 3, bar(10), throw 20)

  var a = call bar 10
  var b = throw 20
  call foo 1 2 3 a b
*/
/*
  foo(1, 2, 3, bar(10), throw 10, qux(20), throw 30, corge(40))

  var a = call bar 10
  var b = throw 10
  var c = call qux 20
  var d = throw 30
  war e = call corge 40
  call foo 1 2 3 a b c d (call corge 40)
*/

  // ("," ("," ("," a b) c) d)

  // return (var {{"a" 5}})
  // return ("," ("," (var {{"a" 5}}) 10) 20)

  // var {{"a" (if 1 {(throw 2)} {3})}}
  // (if 1 )

  // var {{"a" (try {foo} {} {} {{a {b c}} {d {e f}}} {})}}
  // switch foo {{a {}} {d {e f}}} {}

  // var {{"a" (try {a b c} {{"e" {foo}}} {d e f g}) }}
  // try {a b (var {{"a" c}})} {{"e" {(var {{"a" foo}})}}} {d e f g}
  // try {a b c} {} {d e f g}
/*
case "var":
case "new":
case "call":


case "id":
case "++":
case "--":
case "delete":
case "=":
case "+=":
case "-=":
case "*=":
case "/=":
case "%=":
case "<<=":
case ">>=":
case ">>>=":
case "&=":
case "^=":
case "|=":
*/
  /*function block(a, r) {
    if (Array.isArray(a)) {
      a.forEach(function (x) {
        var y
        switch (x[0]) {
        case "function":
        case "function-statement":
          r.push([x[0], x[1], x[2], block(x[3], [])])
          break
// if (var {{"foo" 1}}) {2} {}
// call foo {(call bar {1}) (var {{"qux" 2}}) 3}
        case "if":
          y = expression(x[1], r)
          if (x[2].length || x[3].length) {
            if (isStatement(x[1]) || x[2].some(isStatement) || x[3].some(isStatement)) {
              r.push([x[0], y,
                       block(x[2], []),
                       block(x[3], [])])
            } else if (x[2].length) {
              if (x[3].length) {
                r.push(["?:", y,
                         blockToExpression(x[2]),
                         blockToExpression(x[3])])
              } else {
                r.push(["&&", y, blockToExpression(x[2])])
              }
            } else if (x[3].length) {
              r.push(["&&", ["!", y], blockToExpression(x[3])])
            }
          } else {
            r.push(y)
          }
          break

        case "while":
          r.push([x[0], expression(x[1], r), block(x[2], [])])
          break

        case "try":
          r.push([x[0],
                  block(x[1], []),
                  x[2].map(function (x) {
                    return [x[0], block(x[1], [])]
                  }),
                  block(x[3], [])])
          break

        case ",":
          block([x[1]], r)
          block([x[2]], r)
          break

        case "return":
          r.push([x[0], expression(x[1], r)])
          break

        default:
          expression(x, r)
        }
      })
      return r
    } else {
      return a
    }
  }*/


  n.transform = function (a) {
    return block(a)
  }

  return n
})(NINO || {})
