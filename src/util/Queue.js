"use strict";

exports.fromStringImpl = function (snoc) {
  return function (empty) {
    return function (str) {
      var output = empty;

      var length = str.length;

      for (var i = 0; i < length; ++i) {
        output = snoc(output)(str[i]);
      }

      return output;
    };
  };
};
