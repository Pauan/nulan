"use strict";


function isNaN(x) {
  return x !== x;
}

// TODO handle +-Infinity ?
// TODO handle numbers bigger than Number.MAX_VALUE and smaller than Number.MIN_VALUE ?
// TODO what about rounding ?
exports.fromStringImpl = function (Just) {
  return function (Nothing) {
    return function (str) {
      var num = +str;

      if (isNaN(num)) {
        return Nothing;

      } else {
        return Just(num);
      }
    };
  };
};


// The same as JavaScript's Number.prototype.toString except it never uses exponential notation
exports.toString = function (str) {

};
