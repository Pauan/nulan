var NUIT = (function (n) {
  "use strict";

  var indent = 2

  var complex = /(?:^[@>"#])|\n|^$/

  var invalid = /([\u0009\u000B\u000C\u0085\u00A0\u1680\u180E\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\u0000-\u0008\u000E-\u001F\u007F-\u0084\u0086-\u009F\uFDD0-\uFDEF\uFFFE\uFFFF])|(?:([\s\S]\uFEFF))/g

  function string(s, i) {
    if (invalid.test(s)) {
      s = s.replace(invalid, function (s, m1, m2) {
        var i = 0
        if (m2) {
          s = m2[0]
          i = 1
        }
        return s + "\\u(" + (m2 || m1).charCodeAt(i).toString(16).toUpperCase() + ")"
      })
      s = "\" " + s.replace(/\n/g, "\\\n" + spaces(i + 2))
    } else if (complex.test(s)) {
      s = "> " + s.replace(/\n/g, "\n" + spaces(i + 2))
    }
    return spaces(i) + s
  }

  function spaces(i) {
    return new Array(i + 1).join(" ")
  }

  function simple(s) {
    return !Array.isArray(s) && !complex.test(s) && !invalid.test(s)
  }

  // console.log(require("./lib/nuit.serialize.js").serialize("F\u0000OOBAR\uFEFF\nbarqux\nnou"))

  function serialize(x, o, i) {
    var r = []
    if (Array.isArray(x)) {
      r.push(spaces(i), "@")
      if (simple(x[0])) {
        r.push(x[0])
        // TODO: make multiline the default
        if (simple(x[1]) && !o.multiline) {
          r.push(" ", x[1])
          x = x.slice(2)
        } else {
          x = x.slice(1)
        }
      }
      x.forEach(function (x) {
        r.push("\n", serialize(x, o, i + indent))
      })
    } else {
      r.push(string(x, i))
    }
    return r.join("")
  }

  n.serialize = function (x, o) {
    o = o || {}
    return x.map(function (x) {
      return serialize(x, o, 0)
    }).join("\n\n")
  }

  return n
})(NUIT || {})
