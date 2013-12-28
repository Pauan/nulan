define(["../../lib/util/buffer", "../../lib/util/key"], function (a, b) {
  "use strict";

  var Error = a.Error
    , Key   = b.Key

  var isPrint   = Key("print?")
    , isComplex = Key("complex?")

  var mode   = "normal"
    , indent = 0

  function withIndent(i, f) {
    var old = indent
    indent = i
    try {
      return f()
    } finally {
      indent = old
    }
  }
  
  function withMode(x, f) {
    var old = mode
    mode = x
    try {
      return f()
    } finally {
      mode = old
    }
  }
  
  function spaces(i) {
    if (i == null) {
      i = indent
    }
    return new Array(i + 1).join(" ")
  }

  function isComplex1(x) {
    // TODO
    return Array.isArray(x) ||
           //(Array.isArray(x) && !isSymbol(x[0], "\"")) ||
           // TODO use `object.isObject` from the "object" module
           (Object(x) === x && x[isComplex])
  }

  // TODO move into util/iter
  function partitionWhile(a, f) {
    var l = []
    for (var i = 0; i < a.length; ++i) {
      if (f(a[i])) {
        l.push(a[i])
      } else {
        return [l, a.slice(i)]
      }
    }
    return [l, []]
  }

  function nonComplex(i, a) {
    var x = partitionWhile(a, function (x) {
      return !isComplex1(x)
    })

    var l = x[0]
      , r = x[1]

    l = l.reduce(function (x, y) {
      return withIndent(indent + i + x.length, function () {
        if (x.length === 0) {
          return print(y)
        } else {
          return x + " " + print(y)
        }
      })
    }, "")

    withIndent(indent + 2, function () {
      r = r.map(function (x) {
        return "\n" + spaces() + print(a[i])
      })
    })

    return l + r.join("")
  }

  function printNormal(l, a, r) {
    var s = nonComplex(l.length, a)
    return l + s + r
  }

  function printSpecial(l, a, r) {
    var s = nonComplex(l.length + 1, a)
    if (s === "") {
      return l + r
    } else {
      return l + " " + s + " " + r
    }
  }

  function replaceString(s, x) {
    return x.replace(new RegExp("\\\\" + s, "g"), "\\$&").replace(/\n/g, "$&" + spaces(indent) + " ")
  }

  /*function printString(s, x) {
    var r = []
    for (var i = 1, iLen = x.length; i < iLen; ++i) {
      if (x[i] instanceof String) {
        r.push(replaceString(s, x[i].value))
      } else {
        r.push("@(")
        r.push(print(x[i], indent + r.join("").length + 1))
        r.push(")")
      }
    }
    return s + r.join("") + s
  }*/

  function printObject(x) {
    var max = 0
      , r   = []
    //withIndent(0, function () {
    for (var s in x) {
      var s2 = print(s) // 0
      max = Math.max(max, s2.length)
      r.push([ s2, x[s] ])
    }
    //})
    return withIndent(indent + 2, function () {
      return "{ " + r.map(function (x, i) {
        var s = x[0] + new Array((max - x[0].length) + 1).join(" ") + " = "
        return withIndent(indent + s.length, function () {
          return s + print(x[1])
        })
      }).join("\n" + spaces()) + " }"
    })
  }


  function print(x, i) {
    if (i == null) {
      i = indent
    }
    return withIndent(i, function () {
      // TODO use `object.isObject` from the "object" module
      if (Object(x) === x) {
        if (isPrint in x) {
          return x[isPrint](x, i)

        } else if (Array.isArray(x)) {
          if (mode === "simple") {
            return printNormal("(", x, ")")

          /*} else if (mode === "array") {
            return printSpecial("[", x, "]")*/

          } else if (mode === "normal") {
            /*if (isSymbol(x[0], "\"")) {
              return printString("\"", x)
            } else if (isSymbol(x[0], "{")) {
              return printSpecial("{", x.slice(1), "}")
            } else if (isSymbol(x[0], "[")) {
              return printSpecial("[", x.slice(1), "]")
            } else {*/
              return printSpecial("[", x, "]")
            //}
          }
        
        } else if (x instanceof RegExp) {
          return "#(re " + x + ")"
        
        } else if (x instanceof Date) {
          if (mode === "simple") {
            return "#(date " + (+x) + ")"
          } else {
            return "#(date " + print("" + x) + ")"
          }

        } else if (typeof x === "function") {
          if (x.name == null || x.name === "") {
            return "#(function)"
          } else {
            return "#(function " + x.name + ")"
          }

        } else {
          // TODO handle recursive objects ?
          return printObject(x)
        }
      } else if (typeof x === "string") {
        return "\"" + replaceString("\"", x) + "\""

      } else if (typeof x === "number" || x === true || x === false) {
        return "" + x

      } else if (x == null) {
        return "()"

      } else {
        throw new Error(x, "unknown data type: " + x)
      }
    })
  }

  function simple(x, i) {
    return withMode("simple", function () {
      return print(x, i)
    })
  }
  
  
  function error2(a) {
    return a.reduce(function (x, y) {
      if (Array.isArray(y)) {
        return x + print(y[0], x.length)
      } else {
        return x + y
      }
    }, "")
  }

  function error1(a) {
    // TODO
    return withMode("simple", function () {
      return new Error(a[0], error2([].slice.call(a, 1)))
    })
  }

  function error() {
    throw error1(arguments)
  }

  function warn() {
    console.warn("warning: " + error1(arguments).message)
  }

  /*function array(x, i) {
    return withMode("array", function () {
      return print(x, i)
    })
  }*/

  return {
    isPrint:   isPrint,
    isComplex: isComplex,
    print:     print,
    simple:    simple,
    //array:     array,
    error:     error,
    warn:      warn,
  }
})