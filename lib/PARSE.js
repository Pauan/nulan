/**
 *  Top Down Operator Precedence parser; aka Pratt parser
 *
 *  I learned about the idea from Douglas Crockford (http://javascript.crockford.com/tdop/tdop.html)
 *
 *  Loosely based on the work of Fredrik Lundh (http://effbot.org/zone/simple-top-down-parsing.htm)
 *                                             (http://effbot.org/zone/tdop-index.htm)
 */
var PARSE = (function (n) {
  "use strict";

/*
  def parse(s, bind=0):
    left = s.next().nud(s)
    while bind < s.current.bind:
      left = s.next().led(left, s)
    return left
*/
  // The actual function that does the actual parsing
  // Yes, it's tiny
  // Yes, Pratt parsers are awesome
  //
  // Expects an iterator of tokens and an optional priority
  // Parses tokens until it finds one whose priority is lower than or equal to <i>
  // Returns the parsed expression
  n.expression = function (o, i) {
    i = i || 0

    if (o.has()) {
      var t = o.peek()
      o.read() // TODO

      if ("prefix" in t) {
        var l = t.prefix(o)

                                            // TODO: is this needed
        while (o.has() && (t = o.peek()) && "infix" in t && t.priority > i) {
          o.read() // TODO
          l = t.infix(o, l)
        }
        return l
      } else {
        throw new n.Error(t, "expected prefix operator but got " + t.name)
      }
    } else {
      throw new n.Error({}, "unexpected <EOF>")
    }
  }

  // This is so that you can use Object.create to create modified versions
  // of a token, but still have it be treated as equal to the original
  n.is = function (x, y) {
                      // TODO: test this
    return x === y || y.isPrototypeOf(x)
  }

  // Checks that the next token in the iterator is correct
  // If yes, it then removes it from the iterator
  // If not, or if the iterator is empty, it throws an error
  n.check = function (o, x) {
    if (o.has()) {
      var t = o.peek()
      if (n.is(t, x)) {
        o.read() // TODO
      } else {
        throw new n.Error(o, "expected " + x.name + " but got " + t.name)
      }
    } else {
      throw new n.Error(o, "expected " + x.name + " but got <EOF>")
    }
  }

/*
  // Expects an iterator of tokens
  // Returns a list of all the parsed expressions in the iterator
  n.parse = function (o) {
    var r = []
    while (o.has()) {
      r.push(n.expression(o))
    }
    return r
  }*/

  // Expects an iterator of tokens
  // Returns a single parsed expression
  // If there is more than one expression it throws an error
  n.parse = function (o) {
    var x = n.expression(o)
    if (o.has()) {
      x = o.peek()
      throw new n.Error(x, "expected <EOF> but got " + x.name)
    }
    return x
  }

  // Returns all the parsed expressions
  n.parseAll = function (o) {
    var r = []
    while (o.has()) {
      r.push(n.expression(o))
    }
    return r
  }

  function makeHelper(init) {
    return function (s, i, f) {
      if (!this[s]) {
        this[s] = {
          name: s,
          priority: i
        }
      }
      var x = this[s]
      init.call(x, s, i, f)
      return x
    }
  }

  n.Tokens = function () {}

  // Helper functions to make it easier to generate tokens
  // Example usage:
  //
  //   var t = new PARSE.Tokens()
  //   t.infix("*", 20, function (l, r) { return l * r })
  //   t.infix("/", 20, function (l, r) { return l / r })
  //   t.infix("+", 10, function (l, r) { return l + r })
  //   t.infix("-", 10, function (l, r) { return l - r })
  //
  n.Tokens.prototype = {
    // Returns a token that when parsed returns <x> unmodified
    literal: function (x) {
      return {
        name: x,
        prefix: function () {
          return x
        }
      }
    },

    prefix: makeHelper(function (s, i, f) {
      this.prefix = function (o) {
        return f(n.expression(o, i))
      }
    }),

    // Right associative
    prefixr: makeHelper(function (s, i, f) {
      this.prefix = function (o) {
        return f(n.expression(o, i - 1))
      }
    }),

    infix: makeHelper(function (s, i, f) {
      this.infix = function (o, l) {
        return f(l, n.expression(o, i))
      }
    }),

    // Right associative
    infixr: makeHelper(function (s, i, f) {
      this.infix = function (o, l) {
        return f(l, n.expression(o, i - 1))
      }
    }),

    postfix: makeHelper(function (s, i, f) {
      this.infix = function (o, l) {
        return f(l)
      }
    }),

    // Creates a token that throws an error when used
    // This is useful for placeholder tokens, like the ending pair of braces
    inert: function (s) {
      this[s] = {
        name: s,
        prefix: function () {
          throw new n.Error(this, "invalid token " + s)
        }
      }
    },

    // Creates tokens for braces
    // Example usage:
    //
    //   var t = new PARSE.Tokens()
    //   t.braces("(", ")")
    //   t.braces("[", "]")
    //   t.braces("{", "}")
    //
    braces: function (l, r) {
      var self = this
      self.inert(r)
      return self[l] = {
        name: l,
        prefix: function (o) {
          var x = n.expression(o)
          n.check(o, self[r])
          return x
        }
      }
    }
  }

  return n
})(PARSE || {})
