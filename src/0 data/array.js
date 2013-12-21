define([], function () {
  "use strict";
  
  // TODO generic
  function unwrap(x) {
    return x.length === 1 ? x[0] : x
  }
  
  // TODO generic
  function arrayToIter(x) {
    var i = 0
    return {
      has: function () {
        return i < x.length
      },
      peek: function () {
        return x[i]
      },
      // TODO super hacky
      unpeek: function () {
        return x[i - 2]
      },
      read: function () {
        return x[i++]
      }
    }
  }
  
  return {
    unwrap: unwrap,
    arrayToIter: arrayToIter,
  }
})