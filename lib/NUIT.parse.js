var NUIT = (function (n) {
  "use strict";

  // TODO: not specific to NUIT
  function iterator(a) {
    var i = 0
    return {
      read: function () {
        return a[i++]
      },
      peek: function () {
        return a[i]
      }
    }
  }

  function normalize(s) {
    return s.replace(/(?:^\uFEFF)| +(?=\r\n|[\n\r]|$)/g, "").split(/(?:\r\n|[\n\r])+/g)
  }

  function indent(a) {
    return /^ */.exec(a.peek())[0].length
  }

  function parse(a, s, i) {
    var x, r, i2

    if (s[0] === "#") {
      while (a.peek()) {
        if (indent(a) <= i + 1) {
          return parse(a, a.peek(), i)
        }
        a.read()
      }
      return
    }

    if ((x = /^( *@)([^ ]*)( *)(.*)$/.exec(s))) {
      r = []
      x[2] && r.push(x[2])
      x[4] && r.push(parse(a, x[4], x[1].length + x[2].length + x[3].length))
      i2 = indent(a)
      if (i2 > i) {
        while (indent(a) === i2 && (x = a.read())) {
          r.push(parse(a, x, i2))
        }
      }
      return r
    }

    if ((x = /^( *>) ?(.*)$/.exec(s))) {
      r = []
      x[2] && r.push(x[2])
      //i = x[1].length
      ++i
      while (indent(a) > i && (x = a.read())) {
        r.push(x.slice(i + 1))
      }
      return r.join("\n")
    }

    if ((x = /^( *") ?(.*)$/.exec(s))) {
    }

    x = /^( *)(.+)/.exec(s)
    return x[2]
  }

  n.parse = function (s) {
    var a = iterator(normalize(s))
      , r = []
      , i = indent(a)
      , x
    while ((x = a.read())) {
      x = parse(a, x, i)
      x && r.push(x)
      if (indent(a) !== i) {
        throw new Error("invalid indentation: expected " + i + " but got " + indent(a))
      }
    }
    return r
  }

  return n
})(NUIT || {})
