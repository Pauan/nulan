define(["../options", "../util/data", "../util/print"], function (options, a, b) {
  "use strict";

  var String = a.String
    , Number = a.Number
    , Symbol = a.Symbol
    , Op     = a.Op
    , error  = b.error

  var indent   = 0
    , priority = 0
    , inside   = { loop: false, function: false }
    , statements

  var ops = {}

  function isComplex(x) {
    return x instanceof Op
  }

  // TODO Unicode support
  // TODO it should check reserved
  // TODO move name-related stuff into a separate module
  function isValidJS(x) {
    return /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)
  }

  function jsProp(x) {
    if (x instanceof String && isValidJS(x.value)) {
      return x.value
    } else {
      return null
    }
  }

  /*function propToString(x) {
    if (x instanceof data.String || x instanceof data.Number) {
      return x
    } else if (x instanceof data.Symbol) {
      return new data.String(x.value)
    } else {
      error(x, "expected number, string, or variable but got ", [x])
    }
  }*/

  function resetPriority(i, f) {
    var old = priority
    priority = i
    try {
      return f()
    } finally {
      priority = old
    }
  }

  function withPriority(i, f) {
    var old = priority
    return resetPriority(i, function () {
      var r = f()
      if (old > priority) {
        return "(" + r + ")"
      } else {
        return r
      }
    })
  }

  function withIndent(i, f) {
    var old = indent
    indent = i
    try {
      return f()
    } finally {
      indent = old
    }
  }

  function withInside(s, f) {
    var old = inside[s]
    inside[s] = true
    try {
      return f()
    } finally {
      inside[s] = old
    }
  }

  function space() {
    if (options.minified) {
      return ""
    } else {
      return new Array(indent + 1).join(" ")
    }
  }

  function minify(s, s2) {
    if (options.minified) {
      return s2 || ""
    } else {
      return s
    }
  }

  function get(s) {
    if (s in ops) {
      return ops[s]
    } else {
      throw new Error("unknown operator: " + s)
    }
  }


  function block(x) {
    return withIndent(indent + 2, function () {
      return space() + compileFn(x)
    })
  }

  function compile(x) {
    if (x instanceof Op) {
      var s = get(x.name).compile(x)
      /*if (x.isUseless) {
        if (/\n/.test(s)) {
          options.warn("useless expression:\n" + space() + s)
        } else {
          options.warn("useless expression: " + s)
        }
      }*/
      return s
    } else if (x instanceof Symbol) {
      if (!isValidJS(x.value)) {
        throw new Error("expected valid JavaScript identifier but got: " + x.value)
      }
      return "" + x.value
    } else if (x instanceof Number) {
      return "" + x.value
    } else if (x instanceof String) {
      return "\"" + x.value.replace(/[\\"]/g, "\\$&").replace(/\n/g, "\\n") + "\""
    } else {
      error(x, "[compile.js] unknown data type: ", [x])
    }
  }

  function isOp(x, s) {
    return x instanceof Op && x.name === s
  }

  function compileFn(x) {
    if (isOp(x, "object") || isOp(x, "function")) {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
    }
  }

  function statement(x) {
    if (x instanceof Op) {
      var y = get(x.name)
      if (y.statement != null) {
        y.statement(x)
      } else {
        statements.push(y.expression(x))
      }
    // TODO better checking
    // TODO hacky
    } else if (isComplex(x) || (x instanceof String && x.value === "use strict")) {
      statements.push(x)
    } else {
      // TODO options.warn(x)
    }
  }

  function expression(x) {
    if (x instanceof Op) {
      return get(x.name).expression(x)
    } else {
      return x
    }
  }

  function pushNewline(a, i, len) {
    if (i !== len/* && !isOp(x, "var-function")*/) {
      a.push(new Op("\n", []))
    }
  }

  function blockStatement(x) {
    var old = statements
    statements = []
    try {
      statement(x)
      // TODO do the same optimization for statementTop
      var a = []
        , r = []
      var len = statements.length - 1
      statements.forEach(function (x, i) {
        if (isOp(x, "var")) {
          x.args.forEach(function (x) {
            if (x instanceof Op) {
              var y = x.args[1]
              if (isOp(y, "object") && !options.minified) {
                if (r.length) {
                  pushNewline(a, i, len)
                  a.push(new Op("var", r))
                  r = []
                }
                pushNewline(a, i, len)
                a.push(new Op("var", [x]))
                pushNewline(a, i, len)
              } else {
                r.push(x)
              }
            } else {
              r.push(x)
            }
          })
        } else {
          if (r.length) {
            pushNewline(a, i, len)
            a.push(new Op("var", r))
            pushNewline(a, i, len)
            r = []
          }
          a.push(x)
        }
      })
      if (r.length) {
        pushNewline(a, 0, 1)
        a.push(new Op("var", r))
      }
      return new Op(";", a)
    } finally {
      statements = old
    }
  }

  function braces(x) {
    return "{" + minify("\n") + block(x) + minify("\n") + space() + "}"
  }

  function pushSequence(x) {
    if (x instanceof Op && x.name === ",") {
      // TODO
      if (x.args.length) {
        for (var i = 0; i < x.args.length - 1; ++i) {
          statement(pushSequence(x.args[i])) // TODO check if this is correct
        }
        return x.args[x.args.length - 1]
      } else {
        return x
      }
    } else {
      return x
    }
  }

  function unary(s) {
    return {
      statement: function (x) {
        x.args[0] = pushSequence(x.args[0])
        statements.push(expression(x))
      },
      expression: function (x) {
        x.args = x.args.map(expression)
        return x
      },
      compile: function (x) {
        return s + compile(x.args[0])
      }
    }
  }

  function infix(s, pri) {
    return {
      statement: function (x) {
        x.args[0] = pushSequence(x.args[0])
        statements.push(expression(x)) // TODO
      },
      expression: function (x) {
        x.args = x.args.map(expression)
        return x
      },
      compile: function (x) {
        return withPriority(pri, function () {
          // TODO should this be compileFn ?
          return compileFn(x.args[0]) + minify(" ") + s + minify(" ") + compile(x.args[1])
        })
      }
    }
  }

  function noSemicolon(x) {
    if (x instanceof Op) {
      return !!get(x.name).noSemicolon
    } else {
      return false
    }
  }


  ops["."] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(85, function () {
        return resetPriority(0, function () { // TODO check this, also tests
          var y = jsProp(x.args[1])
          if (y !== null) {
            x = x.args[0]
                                      // TODO not sure how efficient this is...
                                      // TODO does this work correctly for non-number literals?
            if (x instanceof Number && Math.round(x.value) === x.value) {
              x = compile(x) + "."
            } else {
              x = compile(x)
            }
            return x + "." + y
          } else {
            // TODO shouldn't this be compileFn ?
            return compile(x.args[0]) + "[" + compile(x.args[1]) + "]"
          }
        })
      })
    }
  }

  ops["call"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(80, function () {
        return compileFn(x.args[0]) + "(" +
               // TODO: don't hardcode 6
               resetPriority(6, function () {
                 return x.args.slice(1).map(compile).join("," + minify(" "))
               }) + ")"
      })
    }
  }

  ops["object"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        var r = []
        withIndent(indent + 2, function () {
          x.args.forEach(function (x) {
            if (isOp(x, "=")) {
              var y = jsProp(x.args[0])
              console.assert(x.args[0] instanceof String)
              if (y === null) {
                y = compile(x.args[0])
              }
              r.push(minify("\n") + space() +
                     y + ":" + minify(" ") +
                     compile(x.args[1]))
            } else {
              console.assert(x instanceof String)
              var y = jsProp(x)
              if (y === null) {
                y = compile(x)
              }
              r.push(minify("\n") + space() + y + ":" + minify(" ") + compile(x))
            }
          })
        })
        if (r.length) {
          return "{" + r.join(",") + minify("\n") + space() + "}"
        } else {
          return "{}"
        }
      })
    }
  }

  ops["array"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      // TODO don't hardcode 6
      return resetPriority(6, function () {
        return "[" + x.args.map(compile).join("," + minify(" ")) + "]"
      })
    }
  }

  ops["empty"] = {
    statement: function (x) {},
    expression: function (x) {
      return x
    },
    compile: function (x) {
      return "void 0"
    }
  }

  ops["?:"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(15, function () {
        return minify("(") + compileFn(x.args[0]) + minify(" ") + "?" + minify(" ") +
               compile(x.args[1])   + minify(" ") + ":" + minify(" ") +
               compile(x.args[2]) + minify(")")
      })
    }
  }

  ops["if"] = {
    statement: function (x) {
      x.args[0] = expression(x.args[0])
      if (x.args.length > 1) {
        x.args[1] = blockStatement(x.args[1])
      }
      if (x.args.length > 2) {
        x.args[2] = blockStatement(x.args[2])
      }
      statements.push(x) // TODO
    },
    expression: function (x) {
      return expression(new Op("?:", x.args))
    },
    compile: function (x) {
      return "if" + minify(" ") + "(" + compile(x.args[0]) + ")" + minify(" ") + braces(x.args[1]) + minify(" ") + "else" + minify(" ") + braces(x.args[2])
    }
  }

  function statementVar(x) {
    x.args.forEach(function (x) {
      if (isOp(x, "=") && isOp(x.args[1], "function")) {
        statements.push(new Op("var-function", x.args))
      } else {
        statements.push(new Op("var", [x]))
      }
    })
  }

  ops["var"] = {
    statement: function (x) {
      x.args = x.args.map(expression)
      statementVar(x)
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      var last = x.args[x.args.length - 1]
      if (isOp(last, "=")) {
        last = last.args[0]
      }
      statementVar(x)
      return last
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var assigns = []
          , empties = []

        var max = 0
        x.args.forEach(function (x) {
          if (isOp(x, "=")) {
            var k = x.args[0]
            if (!(k instanceof Symbol)) {
              error(k, "expected symbol but got: ", [k])
            }
            max = Math.max(max, k.value.length)
            assigns.push(x)
          } else {
            empties.push(x)
          }
        })

        if (assigns.length) {
          assigns = assigns.map(function (x) {
            var k = x.args[0]
              , v = x.args[1]
            // TODO code duplication
            if (isOp(v, "object") || isOp(v, "function")) {
              return compile(k) +
                     minify(" ") + "=" + minify(" ") +
                     compile(v)
            } else {
              return compile(k) + minify(new Array(max - k.value.length + 1).join(" ")) +
                     minify(" ") + "=" + minify(" ") +
                     compile(v)
            }
          })
        }
        if (empties.length) {
          assigns.push(empties.map(compile).join("," + minify(" ")))
        }

        //var s = minify(x.isInline ? " " : "\n" + space() + "    ")
        return "var " + assigns.join("," + minify("\n" + space() + "    "))
      })
    }
  }

  ops["\n"] = {
    noSemicolon: true,
    expression: function (x) {
      return x
    },
    compile: function () {
      return ""
      //return minify("\n")
    }
  }

  ops[";"] = {
    statement: function (x) {
      x.args.forEach(function (x, i) {
        statement(x)
      })
    },
    compile: function (x) {
      var r   = []
        , len = x.args.length - 1
      x.args.forEach(function (x, i) {
        r.push(compileFn(x))
        if (i !== len) {
          if (options.minified && !noSemicolon(x)) {
            r.push(";")
          }
          r.push(minify("\n") + space())
        }
      })
      return r.join("")
    }
  }

  ops[","] = {
    statement: ops[";"].statement,
    expression: function (x) {
      var r = []
      var len = x.args.length - 1
      x.args.forEach(function (x, i) {
        x = expression(x)
        if (i === len || isComplex(x)) {
          r.push(x)
        }
      })
      x.args = r
      if (x.args.length === 1) {
        return x.args[0]
      } else {
        return x
      }
    },
    compile: function (x) {
      return withPriority(5, function () {
        return x.args.map(function (x, i) {
          if (i === 0) {
            return compileFn(x)
          } else {
            return compile(x)
          }
        }).join("," + minify(" "))
      })
    }
  }

  ops["return"] = unary("return ")

  ops["+"]      = infix("+", 60)
  ops["-"]      = infix("-", 60)
  ops["=="]     = infix("==", 45)
  ops["="]      = infix("=", 10)

  ops["null"] = {
    expression: function (x) {
      return x
    },
    compile: function (x) {
      return "null"
    }
  }

  ops["function"] = {
    expression: function (x) {
      x.args[0].args = x.args[0].args.map(expression)
      x.args[1]      = blockStatement(x.args[1])
      return x
    },
    compile: function (x) {
      return resetPriority(0, function () {
        return withInside("function", function () {
          var args = x.args[0].args.map(compile).join("," + minify(" "))
          return "function" + minify(" ") + "(" + args + ")" + minify(" ") + braces(x.args[1])
        })
      })
    }
  }

  ops["var-function"] = {
    noSemicolon: true,
    /*
    // TODO is this correct ?
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },*/
    compile: function (x) {
      console.assert(isOp(x.args[1], "function"))
      var y = x.args[1]
      return resetPriority(0, function () {
        return withInside("function", function () {
          var args = y.args[0].args.map(compile).join("," + minify(" "))
          return "function " + compile(x.args[0]) + "(" + args + ")" + minify(" ") + braces(y.args[1])
        })
      })
    }
  }


  function expressionTop(r) {
    return new Op(";", r.map(function (x) {
      return expression(x)
    }))
  }

  function compileTop(x) {
    return compileFn(x)
  }

  function statementTop(r) {
    return blockStatement(new Op(",", r))
  }

  return {
    expression: expressionTop,
    statement: statementTop,
    compile: compileTop,
  }
})
