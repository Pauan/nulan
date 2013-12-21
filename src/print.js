define(["../../lib/util/buffer", "../lib/util/name"], function (a, b) {
  "use strict";

  var Error = a.Error
    , Name  = b.Name

  var isPrint   = new Name()
    , isComplex = new Name()

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
    return new Array(i + 1).join(" ")
  }

  function isComplex1(x) {
    return //(Array.isArray(x) && !isSymbol(x[0], "\"")) ||
           // TODO use `object.isObject` from the "object" module
           (Object(x) === x && x[isComplex])
  }

  function nonComplex(iFirst, a, i) {
    var r = []
      , x = a[i]
    ++i
    var s = print(x, iFirst)
    r.push(s)
    var iTemp = iFirst + s.length + 1
    if (!isComplex1(x)) {
      while (i < a.length) {
        if (isComplex1(a[i])) {
          break
        } else {
          var s = print(a[i], iTemp)
          r.push(s)
          iTemp += s.length + 1
        }
        ++i
      }
      r = [r.join(" ")]
    }
    while (i < a.length) {
      r.push("\n" + spaces(indent + 2) + print(a[i], indent + 2))
      ++i
    }
    return r.join("")
  }

  function printNormal(x, i) {
    if (x.length > i) {
      return nonComplex(indent + 1, x, i)
    } else {
      return ""
    }
  }

  function printSpecial(x, i) {
    if (x.length > i) {
      return " " + nonComplex(indent + 2, x, i) + " "
    } else {
      return ""
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
    return "{ " + r.map(function (x, i) {
      var s = x[0] + new Array((max - x[0].length) + 1).join(" ") + " = "
      s += print(x[1], indent + 2 + s.length)
      if (i === 0) {
        return s
      } else {
        return spaces(indent + 2) + s
      }
    }).join("\n") + " }"
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
            return "(" + printNormal(x, 0) + ")"

          /*} else if (mode === "array") {
            return "[" + printSpecial(x, 0) + "]"*/

          } else if (mode === "normal") {
            /*if (isSymbol(x[0], "\"")) {
              return printString("\"", x)
            } else if (isSymbol(x[0], "{")) {
              return "{" + printSpecial(x, 1) + "}"
            } else if (isSymbol(x[0], "[")) {
              return "[" + printSpecial(x, 1) + "]"
            } else {*/
              return "[" + printSpecial(x, 0) + "]"
            //}
          }
        
        } else if (x instanceof RegExp) {
          return "#(re " + x + ")"
        
        } else if (x instanceof Date) {
          if (mode === "simple") {
            return "#(date " + (+x) + ")"
          } else {
            return "#(date " + print("" + x, i) + ")"
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
        console.log(x.length)
        return x + print(y[0], x.length)
      } else {
        return x + y
      }
    }, "")
  }

  function error1(a) {
    return new Error(a[0], error2([].slice.call(a, 1)))
  }

  function error() {
    throw parse(arguments)
  }

  function warn() {
    console.warn(parse(arguments).message)
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