define(["./data"], function (data) {
  "use strict";

  var indent
  
  function spaces(s) {
    return new Array(indent + 1).join(" ") + s
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
  
  function isComplex(x) {
    return (Array.isArray(x) && !data.isSym(x[0], "\"")) ||
           x instanceof data.Op
  }
  
  function nonComplex(mode, iFirst, a, i) {
    var r = []
      , x = a[i]
    ++i
    var iTemp = withIndent(iFirst, function () {
      var s = print1(mode, x)
      r.push(s)
      return indent + s.length + 1
    })
    if (!isComplex(x)) {
      while (i < a.length) {
        if (isComplex(a[i])) {
          break
        } else {
          withIndent(iTemp, function () {
            var s = print1(mode, a[i])
            r.push(s)
            iTemp += s.length + 1
          })
        }
        ++i
      }
      r = [r.join(" ")]
    }
    withIndent(indent + 2, function () {
      while (i < a.length) {
        r.push("\n" + spaces(print1(mode, a[i])))
        ++i
      }
    })
    return r.join("")
  }
  
  function printNormal(mode, x, i) {
    if (x.length > i) {
      return nonComplex(mode, indent + 1, x, i)
    } else {
      return ""
    }
  }
  
  function printSpecial(mode, x, i) {
    if (x.length > i) {
      return " " + nonComplex(mode, indent + 2, x, i) + " "
    } else {
      return ""
    }
  }
  
  function replaceString(x) {
    return x.replace(/[\\"]/g, "\\$&").replace(/\n/g, "$&" + spaces(" "))
  }

  function printString(mode, x) {
    var r = []
    for (var i = 1, iLen = x.length; i < iLen; ++i) {
      if (x[i] instanceof data.String) {
        r.push(replaceString(x[i].value))
      } else {
        r.push("@(")
        withIndent(indent + r.join("").length + 1, function () {
          r.push(print1(mode, x[i]))
        })
        r.push(")")
      }
    }
    return "\"" + r.join("") + "\""
  }
  
  function printObject(mode, x) {
    var r = [new data.Symbol("{")]
    for (var s in x) {
      r.push([ new data.Symbol("="),
               (typeof s === "string"
                 ? new data.Symbol(s)
                 : s),
               x[s] ])
    }
    return print1(mode, r)
  }
  
  // TODO RegExp ?
  // TODO Date ?
  function print1(mode, x) {
    if (Array.isArray(x)) {
      if (mode === "simple") {
        return "(" + printNormal(mode, x, 0) + ")"

      } else if (mode === "array") {
        return "[" + printSpecial(mode, x, 0) + "]"

      } else if (mode === "normal") {
        if (data.isSym(x[0], "\"")) {
          return printString(mode, x)
        } else if (data.isSym(x[0], "{")) {
          return "{" + printSpecial(mode, x, 1) + "}"
        } else if (data.isSym(x[0], "[")) {
          return "[" + printSpecial(mode, x, 1) + "]"
        } else {
          return "(" + printNormal(mode, x, 0) + ")"
        }
      }

    } else if (x instanceof data.Symbol) {
      if (/^[0-9]+$|^[0-9]+\.[0-9]+$|[\(\)\{\}\[\]\.\\\'" \n]/.test(x.value)) {
        return "'" + x.value.replace(/['\\]/g, "\\$&").replace(/\n/g, "\\n") + "'"
      } else {
        return x.value
      }

    } else if (typeof x === "string") {
      return "\"" + replaceString(x) + "\""
    } else if (x instanceof data.String) {
      return "\"" + replaceString(x.value) + "\""
    
    } else if (typeof x === "number") {
      return "" + x
    } else if (x instanceof data.Number) {
      return "" + x.value
      
    } else if (x instanceof data.ParseStart) {
      return "["
    } else if (x instanceof data.ParseStart) {
      return "]"
    } else if (x instanceof data.ParseBypass) {
      return print1(mode, x.value)

    } else if (x instanceof data.Op) {
      return print1(mode, [new data.Symbol(x.name)].concat(x.args))
      //var s = "#(op " + x.name + " "
      //return s + printNormal(x.args, s.length) + ")"
    
    } else if (x instanceof data.Box) {
      if (x.value != null) {
        return "#(box " + x.id + " " + x.value + ")"
      } else {
        return "#(box " + x.id + ")"
      }

    } else if (typeof x === "function") {
      if (x.name == null) {
        return "#(function)"
      } else {
        return "#(function " + x.name + ")"
      }
    
    } else if (x === true || x === false) {
      return "" + x

    } else if (x == null) {
      return "()"
    
    // TODO handle recursive objects ?
    // TODO use `object.isObject` from the "object" module
    } else if (Object(x) === x) {
      return printObject(mode, x)

    } else {
      throw new data.Error(x, "unknown data type: " + x)
    }
  }
  
  function makePrint(mode) {
    return function (x, i) {
      if (i == null) {
        i = 0
      }
      return withIndent(i, function () {
        return print1(mode, x)
      })
    }
  }
  
  var normal = makePrint("normal")
    , simple = makePrint("simple")
    , array  = makePrint("array")
  
  return {
    normal: normal,
    simple: simple,
    array: array,
  }
})
