// AVL Tree algorithm courtesy of waterhouse
// http://www.arclanguage.org/item?id=14181
var NULAN = (function (n) {
  "use strict";

  // These two can be replaced to provide custom "lt" and "is" behavior.
  // Unfortunately, because they're global, they affect all existing
  // trees as well. Making them per-tree was too complicated for too little
  // gain.
  n.lt = function (l, r) {
    return l < r
  }
  n.is = function (l, r) {
    return l === r
  }

  n.Tree = function () {}

  n.Tree.prototype = {
    toString: function () {
      var i = 0
      this.walk(function (o) {
        i = Math.max(i, ("" + o.key).length)
      })
      var a = []
      this.walk(function (o) {
        var spaces = (i - ("" + o.key).length) + 1
        spaces = new Array(spaces + 1).join(" ")
        a.push(o.key + spaces + o.value)
      })
      if (a.length) {
        return "{ " + a.join("\n  ") + " }"
      } else {
        return "{}"
      }
    },
    walk: function (f) {
      function anon(o) {
        if (o !== n.nil) {
          anon(o.l)
          f(o)
          anon(o.r)
        }
      }
      anon(this)
    },
    map: function (f) {
      var r = n.nil
      this.walk(function (o) {
        r = r.set(o.key, f(o))
      })
      return r
    },
    get: function (key, def) {
      var x = this
      if (x === n.nil) {
        if (def == null) {
          throw new Error("undefined: " + key)
        } else {
          return def
        }
      } else if (n.is(key, x.key)) {
        return x.value
      } else if (n.lt(key, x.key)) {
        return x.l.get(key, def)
      } else {
        return x.r.get(key, def)
      }
    },
    set: function (key, value) {
      var x = this
      if (x === n.nil) {
        return make(key, value, x, x)
      } else if (n.is(key, x.key)) {
        return make(key, value, x.l, x.r)
      } else if (n.lt(key, x.key)) {
        return bcons(x, x.l.set(key, value), x.r)
      } else {
        return bcons(x, x.l, x.r.set(key, value))
      }
    },
    rem: function (key) {
      var x = this
      if (x === n.nil) {
        return x // TODO: what's the best behavior for this?
      } else if (n.is(key, x.key)) {
        return merge(x.l, x.r)
      } else if (n.lt(key, x.key)) {
        return bcons(x, x.l.rem(key), x.r)
      } else {
        return bcons(x, x.l, x.r.rem(key))
      }
    },
    toJS: function () {
      var r = {}
      this.walk(function (o) {
        r[o.key] = o.value
      })
      return r
    }
  }

  n.toTree = function (o) {
    var r = n.nil
    for (var s in o) {
      if ({}.hasOwnProperty.call(o, s)) {
        r = r.set(s, o[s])
      }
    }
    return r
  }

  var nil = n.nil = new n.Tree()
  nil.l     = nil.r     = nil
  nil.count = nil.depth = 0

  function make(key, value, l, r) {
    var o   = new n.Tree()
    o.key   = key
    o.value = value
    o.l     = l
    o.r     = r
    o.count = l.count + r.count + 1
    o.depth = Math.max(l.depth, r.depth) + 1
    return o
  }

  function cons(x, l, r) {
    return make(x.key, x.value, l, r)
  }

  // Balanced cons
  function bcons(x, l, r) {
    if (l.depth > r.depth + 1) {
      if (l.l.depth > l.r.depth) {
        return cons(l,
                    l.l,
                    cons(x, l.r, r))
      } else {
        return cons(l.r,
                    cons(l, l.l,   l.r.l),
                    cons(x, l.r.r, r))
      }
    } else if (r.depth > l.depth + 1) {
      if (r.r.depth > r.l.depth) {
        return cons(r,
                    cons(x, l, r.l),
                    r.r)
      } else {
        return cons(r.l,
                    cons(x, l,     r.l.l),
                    cons(r, r.l.r, r.r))
      }
    } else {
      return cons(x, l, r)
    }
  }

  // Not a general merge: assumes [all of l] <= [all of r]
  function merge(l, r) {
    if (l === n.nil) {
      return r
    } else if (r === n.nil) {
      return l
    } else if (l.depth < r.depth) {
      return bcons(r, merge(l, r.l), r.r)
    } else {
      return bcons(l, l.l, merge(l.r, r))
    }
  }

  return n
})(NULAN || {})
