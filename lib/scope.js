// Move to lib/utils ?
"use strict";

var key = require("./util/symbol")

var scopes = new key.Symbol()

var deleted = {}

function checkScopes(a) {
  if (a.length === 0) {
    throw new Error("must call push before calling has/get/set/del")
  }
}

function Scope(x) {
  this[scopes] = []
  if (x != null) {
    this[scopes].push(x)
  }
}
Scope.prototype.has = function (s) {
  var a = this[scopes]
    , i = a.length
  checkScopes(a)
  while (i--) {
    var x = a[i]
    if (s in x) {
      if (x[s] === deleted) {
        return false
      } else {
        return true
      }
    }
  }
  return false
}
Scope.prototype.get = function (s) {
  var a = this[scopes]
    , i = a.length
  checkScopes(a)
  while (i--) {
    var x = a[i]
    if (s in x) {
      if (x[s] === deleted) {
        return
      } else {
        return x[s]
      }
    }
  }
}
Scope.prototype.set = function (s, v) {
  var a = this[scopes]
  checkScopes(a)
  a[a.length - 1][s] = v
}
Scope.prototype.del = function (s) {
  this.set(s, deleted)
}
Scope.prototype.reset = function (x, f) {
  var old = this[scopes]
  this[scopes] = [x]
  try {
    return f()
  } finally {
    this[scopes] = old
  }
}
Scope.prototype.push = function (x, f) {
  this[scopes].push(x)
  try {
    return f()
  } finally {
    this[scopes].pop()
  }
}

exports.Scope = Scope