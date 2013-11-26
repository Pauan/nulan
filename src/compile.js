define(["./data", "./box", "./error"], function (data, box, error) {
  "use strict";
  
  var priority   = 0
    , statements = []
  
  function compileFn(x) {
    //var x = unwrap(w)
    if (x.op === "function" || x.op === "object") {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
    }
  }
  
  function get(s) {
    if (s in ops) {
      return ops[s]
    } else {
      throw new Error("unknown operator: " + s)
    }
  }
  
  function space() {
    return "" // TODO
    if (opt.minified) {
      return ""
    } else {
      return new Array((indent * 2) + 1).join(" ")
    }
  }
  
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
  
  function minify(s, s2) {
    return s // TODO
    if (opt.minified) {
      return s2 || ""
    } else {
      return s
    }
  }
  
  function unary(s) {
    return {
      expression: function (x) {
        x.args = x.args.map(expression)
        return x
      },
      compile: function (x) {
        return s + compile(x.args[0])
      }
    }
  }
  
  function infix(s) {
    return {
      expression: function (x) {
        x.args = x.args.map(expression)
        return x
      },
      compile: function (x) {
        return compile(x.args[0]) + " " + s + " " + compile(x.args[1])
      }
    }
  }
  
  var ops = {}
  
  ops["."] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return compile(x.args[0]) + "." + x.args[1].value
    }
  }
  
  ops["call"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return withPriority(80, function () {
               // TODO compileFn
        return compile(x.args[0]) + "(" +
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
        x.args.forEach(function (x) {
          if (x.name === "=") {
            r.push(minify("\n") + space() +
                   compile(x.args[0]) + ":" + minify(" ") +
                   compile(x.args[1]))
          }
        })
        if (r.length) {
          return "({" + r.join(",") + minify("\n") + space() + "})"
        } else {
          return "({})"
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
    statement: function (x) {
      statements.push(expression(x)) // TODO
    },
    expression: function (x) {
      return new data.Op("void", [new data.Number(0)])
    }
  }
  
  ops["var"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var s = minify(x.isInline ? " " : "\n" + space() + "    ")
        return "var " + x.args.map(compile).join("," + s)
      })
    }
  }
  
  ops[";"] = {
    compile: function (x) {
      return x.args.map(compile).join(";\n")
    }
  }
  
  ops["void"] = unary("void ")
  ops["return"] = unary("return ")
  ops["+"]    = infix("+")
  
  ops[","] = {
    statement: function (x) {
      x.args.forEach(function (x) {
        statement(x)
      })
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return compile(new data.Op(";", x.args))
      return withPriority(5, function () {
        return x.args.map(function (x, i) {
          /*if (i === 0) {
            return compileFn(x)
          } else {*/
            return compile(x)
          //}
        }).join("," + minify(" "))
      })
    }
  }
  
  ops["function"] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return "(function(" + x.args[0].args.map(compile).join(", ") + "){" + compile(x.args[1]) + "})"
    }
  }
  
  ops["="] = {
    expression: function (x) {
      x.args = x.args.map(expression)
      return x
    },
    compile: function (x) {
      return compile(x.args[0]) + " = " + compile(x.args[1])
    }
  }
  
  function compile(x) {
    if (x instanceof data.Op) {
      return get(x.name).compile(x)
    } else if (x instanceof data.Box) {
      return x.value
    } else if (x instanceof data.Symbol) {
      return x.value
    } else if (x instanceof data.Number) {
      return "" + x.value
    } else if (x instanceof data.String) {
      return "\"" + x.value.replace(/[\\"]/g, "\\$&").replace(/\n/g, "\\n") + "\""
    } else {
      error(x, "unknown data type: " + x)
    }
  }
  
  function statement(x) {
    if (x instanceof data.Op) {
      var y = get(x.name)
      if (y.statement != null) {
        y.statement(x)
      } else {
        statements.push(y.expression(x))
      }
    } else {
      statements.push(x)
    }
  }
  
  function expression(x) {
    if (x instanceof data.Op) {
      return get(x.name).expression(x)
    } else {
      return x
    }
  }
  
  function expressionTop(r) {
    return r.map(function (x) {
      return expression(x)
    })
  }
  
  // TODO use compileFn if I change this to accept a single argument rather than an array
  function compileTop(r) {
    return compile(new data.Op(";", r))
  }
  
  function statementTop(r) {
    var old = statements
    statements = []
    try {
      // TODO use iter module ?
      r.forEach(function (x) {
        statement(x)
      })
      return statements
    } finally {
      statements = old
    }
  }
  
  return {
    expression: expressionTop,
    statement: statementTop,
    compile: compileTop,
  }
})
