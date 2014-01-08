"use strict";

var a = require("../../lib/util/buffer")
  , b = require("../../lib/util/list")
  , c = require("../../lib/util/object")

var buffer   = a.buffer
  , loc      = a.loc
  , Error    = a.Error
  , toCons   = b.toCons
  , cons     = b.cons
  , car      = b.car
  , cdr      = b.cdr
  , nil      = b.nil
  , isString = c.isString
  , isNumber = c.isNumber


exports.Error = Error

function Symbol(x, loc) {
  if (loc == null) {
    loc = null
  }
  this.type = "Identifier"
  this.name = x
  this.loc  = loc
}
Symbol.prototype.toString = function () {
  return this.name
}
exports.Symbol = Symbol

function Literal(x, loc) {
  if (loc == null) {
    loc = null
  }
  this.type  = "Literal"
  this.value = x
  this.loc   = loc
}
Literal.prototype.toString = function () {
  if (isString(this.value)) {
    // TODO unprintable characters too
    return "\"" + this.value.replace(/[\\\"]/g, "\\$&") + "\""
  } else if (isNumber(this.value)) {
    return "" + this.value
  } else {
    return "" + this.value
  }
}
exports.Literal = Literal


var rules = {}

function isDelimiter(s) {
  return rules[s] != null && rules[s].delimiter
}

function tokenizeAtom(top, s, a) {
  var r = [s.value]

  var posFirst = s.loc
    , posLast  = posFirst

  if (!isDelimiter(s.value)) {
    while (true) {
      if (a === nil) {
        break
      } else {
        s = car(a)
        if (isDelimiter(s.value)) {
          break
        } else {
          r.push(s.value)
          posLast = s.loc
          a = cdr(a)
        }
      }
    }
  }

  r = r.join("")

  if (/^[0-9]+$/.test(r)) {
    s = new Literal(+r, loc(posFirst, posLast))
    //x.whitespace = info.whitespace
    //info.whitespace = false
  } else {
    s = new Symbol(r, loc(posFirst, posLast))
    //x.whitespace = info.whitespace
    //info.whitespace = isWhitespace(r)
  }

  top.push(s)
  return a
}


var ends = []

function endAt(s) {
  return function (top, token, a) {
    top.push(new Symbol(token.value, token.loc))
    ends.push(s)
    try {
      while (true) {
        if (a === nil) {
          throw new Error(token, "missing ending " + s)
        } else {
          var x = car(a)
          if (x.value === s) {
            top.push(new Symbol(x.value, x.loc))
            a = cdr(a)
            break
          } else {
            a = tokenize1(top, a)
          }
        }
      }
    } finally {
      ends.pop()
    }
    return a
  }
}

function startAt(sStart) {
  return function (top, token) {
    if (ends.length) {
      throw new Error(token, "expected " + ends[ends.length - 1] + " but got " + token.value)
    } else {
      throw new Error(token, "missing starting " + sStart)
    }
  }
}

function tokenizeString(s) {
  return null
}

function tokenizeWhitespace(top, s, a) {
  return a
}

function tokenizeComment1(token, a) {

}

function tokenizeComment(top, token, a) {
  if (a === nil) {
    return a
  } else {
    var x = car(a)
    if (x.value === "|") {
      return tokenizeComment1(token, cdr(a))
    } else {
      while (true) {
        if (a === nil) {
          break
        } else {
          x = car(a)
          a = cdr(a)
          if (x.value === "\n") {
            break
          }
        }
      }
      return a
    }
  }
}

function tokenize1(top, a) {
  if (a === nil) {
    return a
  } else {
    var s = car(a)
      , c = s.value

    if (rules[c] != null && rules[c].tokenize != null) {
      a = rules[c].tokenize(top, s, cdr(a))

    } else {
      a = tokenizeAtom(top, s, cdr(a))
    }

    return a
  }
}

function tokenize(x, filename) {
  var a = toCons(buffer(x, filename))
  var r = []
  while (a !== nil) {
    a = tokenize1(r, a)
  }
  return r
}
exports.tokenize = tokenize


function delimiter(s, f) {
  rules[s] = { delimiter: true, tokenize: f }
}

// except (?)
// _
// ->
// =
delimiter(" ",  tokenizeWhitespace)
delimiter("\n", tokenizeWhitespace)
delimiter("#",  tokenizeComment)
delimiter("\"", tokenizeString("\""))

delimiter(":")
delimiter(".")
delimiter("`")
delimiter(",")
delimiter("@")
delimiter("(")
delimiter(")")
delimiter("[")
delimiter("]")
delimiter("{")
delimiter("}")