var NINO = (function (n) {
  "use strict";

  var forms = {}

  var withPrecedence, resetPrecedence

  ;(function () {
    var precedence = 0

    withPrecedence = function (i, f) {
      var old = precedence
      precedence = i
      var r = f()
      precedence = old
      if (old > i) {
        return "(" + r + ")"
      } else {
        return r
      }
    }

    resetPrecedence = function (i, f) {
      var old = precedence
      precedence = i
      var r = f()
      precedence = old
      return r
    }
  })()

  var spaces,     compile,         compileStatement
    , withIndent, blockStatements, indentBlock

  ;(function () {
    var inBlock
      , indent = 0

    spaces = function () {
      return new Array(indent + 1).join(" ")
      /*if (inBlock) {

      } else {
        return ""
      }*/
    }

    function compileExpr(a) {
      if (!forms[a[0]]) {
        throw new Error(a[0])
      } else {
        //var old = inBlock
        // This delays the inBlock by 1 call to compile
        /*if (inBlock) {
          inBlock = 0
        } else if (inBlock === 0) {
          inBlock = false
        }*/
        var r = forms[a[0]].apply(null, a.slice(1))
        //inBlock = old
        return r
      }
    }

    compile = function (a) {
      inBlock = false
      return compileExpr(a)
    }

    compileStatement = function (a) {
      var r = (inBlock && (a[0] === "function" || a[0] === "object")
                ? "(" + compileExpr(a) + ")"
                : compileExpr(a))
      return r
    }

    withIndent = function (f) {
      var old = indent
      indent += 2

      var r = f()

      indent = old
      return r
    }

    blockStatements = function (a) {
      inBlock = true
      return a.map(function (x) {
        return spaces() + compileStatement(x)
      }).join(";\n")
    }

    indentBlock = function (a) {
      return withIndent(function () {
        return blockStatements(a)
      })
    }
  })()

  function statement(s) {
    forms[s] = function (x) {
      return s + " " + compile(x)
    }
  }

  function unary(i, s, sAlt) {
    sAlt = sAlt || s
    forms[s] = function (x) {
      return withPrecedence(i, function () {
        return sAlt + compile(x)
      })
    }
  }

  function binary(i, s, sAlt) {
    sAlt = sAlt || " " + s + " "
    forms[s] = function (x, y) {
      return withPrecedence(i, function () {
        return compileStatement(x) + sAlt + compile(y)
      })
    }
  }

  function trinary(i, s, sL, sR) {
    forms[s] = function (x, y, z) {
      return withPrecedence(i, function () {
        return compileStatement(x) + sL + compile(y) + sR + compile(z)
      })
    }
  }

  function objectName(s) {
    // TODO: this check is incorrect
    //       see http://stackoverflow.com/questions/1661197/valid-characters-for-javascript-variable-names
    if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(s)) {
      return s
    } else {
      return forms["string"](s)
    }
  }

  function block(x) {
    if (x.length === 0) {
      return "{}"
    } else {
      return "{\n" + indentBlock(x) + "\n" + spaces() + "}"
    }
  }

  function oneOrMany(x, colon) {
    if (x.length === 1) {
      return compile(x[0]) + (colon ? ";" : "")
    } else {
      return block(x)
    }
  }

  /*function nary(i, s) {
    forms[s] = function () {
      var a = arguments
      return withPrecedence(i, function () {
        return [].map.call(a, compile).join(s + " ")
      })
    }
  }*/

  // Primitives
  forms["empty"] = function () {}
  forms["name"] = forms["number"] = forms["boolean"] = forms["id"] = function (x) {
    return x
  }
  forms["null"] = function () {
    return "null"
  }
  forms["regexp"] = function (x) {
    return "/" + x.replace(/[\/\\]/g, "\\$&") + "/"
  }
  forms["string"] = function (x) {
    return "\"" + x.replace(/["\\]/g, "\\$&") + "\""
  }

  // Statements
  statement("break")
  statement("continue")
  statement("debugger")
  statement("return")
  statement("throw")
  forms["var"] = function (a) {
    // Precedence is 1 higher than the precedence for ","
    return resetPrecedence(6, function () {
      var r = a.map(function (a) {
        if (a.length === 1) {
          return a[0]
        } else {
          return a[0] + " = " + compile(a[1])
        }
      })
      return "var " + r.join("\n" + spaces() + "  , ")
    })
  }
  forms["if"] = function (x, y, z) {
    console.log(x, y, z)
    return "if (" + compile(x) + ") " +
             (y.length
               ? oneOrMany(y, z.length)
               : ";") +
             (z.length
               ? " else " + oneOrMany(z)
               : "")
  }
  forms["try"] = function (x) {
    return "try " + block(x) + [].slice.call(arguments, 1).map(function (a) {
      if (a[0] === "catch") {
      } else if (a[0] === "finally") {
        return " finally " + block(a[1])
      }
    })
  }

  // Expressions
  forms["new"] = function (x, a) {
    // TODO: code duplication with forms["call"]
    return withPrecedence(85, function () {
      return "new " + compile(x) + "(" + a.map(compile).join(", ") + ")"
    })
  }
  forms["."] = function (x, y) {
    return withPrecedence(85, function () {
      if (Array.isArray(x) && x[0] === "number" && /^\d+$/.test(x[1])) {
        return compile(x) + "."
      } else {
        return compileStatement(x)
      }
    }) + "." + y
  }
  forms["[]"] = function (x, y) {
    return withPrecedence(85, function () {
      return compileStatement(x) + "[" + compile(y) + "]"
    })
  }
  forms["call"] = function (x, a) {
    return withPrecedence(80, function () {
      return compileStatement(x) + "(" + a.map(compile).join(", ") + ")"
    })
  }
  unary(75, "++")
  unary(75, "--")
  unary(70, "!")
  unary(70, "~")
  unary(70, "u+", "+")
  unary(70, "u-", "-")
  unary(70, "typeof", "typeof ")
  unary(70, "void", "void ")
  unary(70, "delete", "delete ")
  binary(65, "*")
  binary(65, "/")
  binary(65, "%")
  binary(60, "+")
  binary(60, "-")
  binary(55, "<<")
  binary(55, ">>")
  binary(55, ">>>")
  binary(50, "<")
  binary(50, "<=")
  binary(50, ">")
  binary(50, ">=")
  binary(50, "in", " in ")
  binary(50, "instanceof", " instanceof ")
  binary(45, "==")
  binary(45, "!=")
  binary(45, "===")
  binary(45, "!==")
  binary(40, "&")
  binary(35, "^")
  binary(30, "|")
  binary(25, "&&")
  binary(20, "||")
  trinary(15, "?:", " ? ", " : ")
  binary(10, "=")
  binary(10, "+=")
  binary(10, "-=")
  binary(10, "*=")
  binary(10, "/=")
  binary(10, "%=")
  binary(10, "<<=")
  binary(10, ">>=")
  binary(10, ">>>=")
  binary(10, "&=")
  binary(10, "^=")
  binary(10, "|=")
  forms["array"] = function (a) {
    // Precedence is 1 higher than the precedence for ","
    return resetPrecedence(6, function () {
      return "[" + a.map(compile).join(", ") + "]"
    })
  }
  forms["object"] = function (a) {
    // Precedence is 1 higher than the precedence for ","
    return resetPrecedence(6, function () {
      if (a.length) {
        return "{\n" + withIndent(function () {
                         return a.map(function (a) {
                           return spaces() + objectName(a[0]) + ": " + compile(a[1])
                         })
                       }).join(",\n") + "\n" + spaces() + "}"
      } else {
        return "{}"
      }
    })
  }
  binary(5, ",", ", ")
  forms["function"] = forms["function-statement"] = function (name, args, body) {
    return resetPrecedence(0, function () {
      return "function " + name + "(" + args.join(", ") + ") " + block(body)
    })
  }

  n.compile = function (a) {
    return blockStatements(a)
  }

  return n
})(NINO || {})
