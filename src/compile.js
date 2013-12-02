define(["./data", "./error", "./options"], function (data, error, options) {
  "use strict";

  var indent   = 0
    , priority = 0
    , inside   = { loop: false, function: false }
    , statements
  
  var ops = {}
  
  var reserved = {}
  
  // https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Reserved_Words
  ;("break case catch continue debugger default delete do else finally for function if in instanceof new return switch this throw try typeof var void while with " +
    "class enum export extends import super " +
    "implements interface let package private protected public static yield " +
    "null true false").split(" ").forEach(function (s) {
    reserved[s] = true
  })

  // TODO Unicode support
  function isValidJS(x) {
    return /^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)
  }
    
  function mangle(s) {
    if (reserved[s]) {
      return "_" + s
    } else {
      s = options.mangle(s)
      if (!isValidJS(s)) {
        throw new Error("expected valid JavaScript identifier but got: " + s)
      }
      return s
    }
  }
  
  function unmangle(s) {
    var x = /^_(.*)$/.exec(s)
    console.log(x)
    if (x && reserved[x[1]]) {
      return x[1]
    } else {
      return options.unmangle(s)
    }
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
      // TODO hacky
      if (x.name === "top") {
        return new data.Op("top", statements)
      } else {
        return new data.Op(";", statements)
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
  
  ops["var"] = {
    statement: function (x) {
      x.args = x.args.map(expression)
      statements.push(x)
    },
    expression: function (x) {
      x.args = x.args.map(expression)
      var last = x.args[x.args.length - 1]
      if (last.name === "=") {
        last = last.args[0]
      }
      statements.push(x)
      return last
    },
    compile: function (x) {
      return resetPriority(0, function () { // TODO test this
        var s = minify("\n" + space() + "    ")
        //var s = minify(x.isInline ? " " : "\n" + space() + "    ")
        return "var " + x.args.map(compile).join("," + s)
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
  ops["="]      = infix("=", 10)
  
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
