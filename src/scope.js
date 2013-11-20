// Move to lib/utils
define(["../lib/util/name"], function (name) {
  "use strict";
  
  var scopes = new name.Name()
  
  var deleted = {}
  
  function Scope() {
    this[scopes] = [{}]
  }
  Scope.prototype.has = function (s) {
    var a = this[scopes]
      , i = a.length
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
    a[a.length - 1][s] = v
  }
  Scope.prototype.del = function (s) {
    this.set(s, deleted)
  }
  Scope.prototype.push = function () {
    this[scopes].push({})
  }
  Scope.prototype.pop = function () {
    this[scopes].pop()
  }
  
  function make() {
    return new Scope()
  }
  
  return {
    make: make,
  }
})
