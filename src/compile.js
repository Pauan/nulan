define(["./data", "./error", "./options"], function (data, error, options) {
  "use strict";

  var indent   = 0
    , priority = 0
    , inside   = { loop: false, function: false }
    , statements
  
  var ops = {}

  // TODO Unicode support
  // TODO it should check reserved
  // TODO move name-related stuff into a separate module
  function isValidJS(x) {
    return /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)
  }
  
  function jsProp(x) {
    if (x instanceof data.String && isValidJS(x.value)) {
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
      return new Array((indent * 2) + 1).join(" ")
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
    return withIndent(indent + 1, function () {
      return space() + compileFn(x)
    })
  }
  
  function compile(x) {
    if (x instanceof data.Op) {
      var s = get(x.name).compile(x)
      /*if (x.isUseless) {
        if (/\n/.test(s)) {
          options.warn("useless expression:\n" + space() + s)
        } else {
          options.warn("useless expression: " + s)
        }
      }*/
      return s
    } else if (x instanceof data.Symbol) {
      if (!isValidJS(x.value)) {
        throw new Error("expected valid JavaScript identifier but got: " + x.value)
      }
      return "" + x.value
    } else if (x instanceof data.Number) {
      return "" + x.value
    } else if (x instanceof data.String) {
      return "\"" + x.value.replace(/[\\"]/g, "\\$&").replace(/\n/g, "\\n") + "\""
    } else {
      error(x, "[compile.js] unknown data type: ", [x])
    }
  }
  
  function compileFn(x) {
    if (x.name === "function" || x.name === "object") {
      return "(" + compile(x) + ")"
    } else {
      return compile(x)
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
  
  function blockStatement(x) {
    var old = statements
    statements = []
    try {
      statement(x)
      // TODO do the same optimization for statementTop
      var a = []
        , r = []
      statements.forEach(function (x) {
        if (x instanceof data.Op && x.name === "var") {
          r = r.concat(x.args)
        } else {
          if (r.length) {
            a.push(new data.Op("var", r))
            r = []
          }
          a.push(x)
        }
      })
      if (r.length) {
        a.push(new data.Op("var", r))
      }
      // TODO hacky
      if (x.name === "top") {
        return new data.Op("top", a)
      } else {
        return new data.Op(";", a)
      }
    } finally {
      statements = old
    }
  }
  
  function compileSeparator(s) {
    return function (x) {
      var r   = []
        , len = x.args.length - 1
      x.args.forEach(function (x, i) {
        r.push(compileFn(x))
        if (i !== len) {
          //if (!x.noSemicolon) {
            r.push(";")
          //}
          r.push(minify(s) + space())
        }
      })
      return r.join("")
    }
  }
  
  function braces(x) {
    return "{" + minify("\n") + block(x) + minify("\n") + space() + "}"
  }
  
  // TODO move into builtins.js probably
  /*function argumentError(x, min, max) {
    var len = x.args.length
    if ((min != null && len < min) || (max != null && len > max)) {
      if (max == null) {
        throw opt.error(x, "expected at least " + min + " argument but got " + len)
      } else if (min == null) {
        throw opt.error(x, "expected at most " + max + " argument but got " + len)
      }

      if (min === max) {
        throw opt.error(x, "expected " + min + " arguments but got " + len)
      } else {
        throw opt.error(x, "expected " + min + " to " + max + " arguments but got " + len)
      }
    }
  }*/
  
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
  
  function infix(s, pri) {
    return {
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
            if (x instanceof data.Number && Math.round(x.value) === x.value) {
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
        withIndent(indent + 1, function () {
          x.args.forEach(function (x) {
            if (x.name === "=") {
              var y = jsProp(x.args[0])
              console.assert(x.args[0] instanceof data.String)
              if (y === null) {
                y = compile(x.args[0])
              }
              r.push(minify("\n") + space() +
                     y + ":" + minify(" ") +
                     compile(x.args[1]))
            } else {
              console.assert(x instanceof data.String)
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
      statements.push(x)
    },
    expression: function (x) {
      return expression(new data.Op("?:", x.args))
    },
    compile: function (x) {
      return "if" + minify(" ") + "(" + compile(x.args[0]) + ")" + minify(" ") + braces(x.args[1]) + minify(" ") + "else" + minify(" ") + braces(x.args[2])
    }
  }
  
  ops["var"] = {
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      var last = x.args[x.args.length - 1]
      if (last instanceof data.Op && last.name === "=") {
        last = last.args[0]
      }
      statements.push(x)
      return last
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var assigns = []
          , empties = []

        var max = 0
        x.args.forEach(function (x) {
          if (x instanceof data.Op && x.name === "=") {
            var k = x.args[0]
            if (!(k instanceof data.Symbol)) {
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
            return compile(k) + minify(new Array(max - k.value.length + 1).join(" ")) +
                     minify(" ") + "=" + minify(" ") +
                     compile(x.args[1])
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
  
  ops["top"] = {
    statement: function (x) {
      x.args.forEach(function (x) {
        statement(x)
      })
    },
    expression: function (x) {
      return expression(new data.Op(",", x.args))
    },
    compile: compileSeparator("\n\n")
  }

  ops[";"] = Object.create(ops["top"])
  ops[";"].compile = compileSeparator("\n")

  ops[","] = {
    statement: ops["top"].statement,
    expression: function (x) {
      x.args = x.args.map(expression)
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

  
  function expressionTop(r) {
    return new data.Op(";", r.map(function (x) {
      return expression(x)
    }))
  }

  function compileTop(x) {
    return compileFn(x)
  }
  
  function statementTop(r) {
    var old = statements
    statements = []
    try {
      // TODO use iter module ?
      r.forEach(function (x) {
        statement(x)
      })
      return new data.Op("top", statements)
    } finally {
      statements = old
    }
  }
  
  function withModule(m, f) {
    var name    = (m.name == null ? null : new data.String(m.name))
    var imports = new data.Op("array", m.imports)
    var args    = new data.Op(",", m.arguments)
    var exports = []
    for (var s in m.exports) {
      exports.push(new data.Op("=", [new data.Symbol(s), m.exports[s]]))
    }
    exports = new data.Op("return", [new data.Op("object", exports)])
    return f(name, imports, args, exports)
  }
  
  function moduleTop(m, r) {
    return withModule(m, function (name, imports, args, exports) {
      var a = []
      a.push(new data.Symbol("define"))
      if (name !== null) {
        a.push(name)
      }
      a.push(imports)
      a.push(new data.Op("function", [args, new data.Op("top", [new data.String("use strict")].concat(r, [exports]))]))
      return statementTop([new data.Op("call", a)])
    })
  }
  
  function requireTop(m, r) {
    return withModule(m, function (name, imports, args, exports) {
      var a = []
      a.push(new data.Symbol("require"))
      if (name !== null) {
        error(m, "expected no module name but got ", [name])
      }
      a.push(imports)
      a.push(new data.Op("function", [args, new data.Op("top", [new data.String("use strict")].concat(r))]))
      return statementTop([new data.Op("call", a)])
    })
  }
  
  return {
    expression: expressionTop,
    statement: statementTop,
    compile: compileTop,
    module: moduleTop,
    require: requireTop,
  }
})
