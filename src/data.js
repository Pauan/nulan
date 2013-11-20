define(["../lib/util/buffer"], function (buffer) {
  "use strict";
  
  var Error = buffer.Error
  
  function Module() {
    this.imports = []
    this.exports = {}
  }
  
  function Op(s, a) {
    this.name = s
    this.args = a
  }
  Op.prototype.toString = function () {
    return "(op " + this.name + " " + this.args.join(" ") + ")"
  }
  
  function isSym(x, y) {
    return x instanceof Symbol && x.value === y
  }
  
  var indent = 0
  
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
  
  function printNormal(x) {
    if (x.length > 0) {
      var r = []
      var i = 1
      withIndent(indent + 1, function () {
        r.push(print(x[0]))
        while (i < x.length) {
          if (Array.isArray(x[i])) {
            break
          } else {
            r.push(print(x[i]))
          }
          ++i
        }
        r = [r.join(" ")]
      })
      withIndent(indent + 2, function () {
        while (i < x.length) {
          r.push("\n" + spaces(print(x[i])))
          ++i
        }
      })
      return r.join("")
    } else {
      return ""
    }
  }
  
  function printSpecial(x) {
    if (x.length > 1) {
      var r = [print(x[1])]
      var i = 2
      if (!Array.isArray(x[1])) {
        while (i < x.length) {
          if (Array.isArray(x[i])) {
            break
          } else {
            r.push(print(x[i]))
          }
          ++i
        }
        r = [r.join(" ")]
      }
      while (i < x.length) {
        r.push("\n" + spaces(print(x[i])))
        ++i
      }
      return " " + r.join("") + " "
    } else {
      return ""
    }
  }
  
  // TODO move into a separate module
  function print(x) {
    if (Array.isArray(x)) {
      if (isSym(x[0], "{")) {
        return withIndent(indent + 2, function () {
          return "{" + printSpecial(x) + "}"
        })
      } else if (isSym(x[0], "[")) {
        return withIndent(indent + 2, function () {
          return "[" + printSpecial(x) + "]"
        })
      } else {
        return "(" + printNormal(x) + ")"
      }
    // TODO move this to Symbol.prototype.toString ?
    } else if (x instanceof Symbol) {
      if (/^[0-9]+$|^[0-9]+\.[0-9]+$|[\(\)\{\}\[\]\.\\\'" \n]/.test(x.value)) {
        return "'" + x.value.replace(/['\\]/g, "\\$&").replace(/\n/g, "\\n") + "'"
      } else {
        return x.value
      }
    } else if (x instanceof String) {
      return "\"" + x.value.replace(/[\\"]/g, "\\$&").replace(/\n/g, "$& ") + "\""
    } else if (x instanceof Number) {
      return "" + x.value
    } else if (x instanceof ParseIndent) {
      return "<"
    } else if (x instanceof ParseDedent) {
      return ">"
    } else if (x instanceof ParseBypass) {
      return print(x.value)
    } else {
      return "" + x
    }
  }
  
  // TODO generic
  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }
  
  function loc(x, y) {
    return {
      source: x.source,
      start: x.start,
      end: y.end
    }
  }
  
  function ParseIndent() {}
  function ParseDedent() {}
  function ParseBypass(x) {
    this.value = x
  }
  function Symbol(x) {
    this.value = x
  }
  function Number(x) {
    this.value = x
  }
  function String(x) {
    this.value = x
  }
  
  return {
    loc: loc,
    unwrap: unwrap,
    isSym: isSym,
    print: print,

    Error: Error,
    Op: Op,
    ParseIndent: ParseIndent,
    ParseDedent: ParseDedent,
    ParseBypass: ParseBypass,
    Symbol: Symbol,
    Number: Number,
    String: String,
  }
})
