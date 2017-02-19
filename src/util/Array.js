"use strict";

exports.interleave = function (value) {
  return function (a) {
    // TODO use new Array
    var output = [];

    var length = a.length;

    for (var i = 0; i < length; ++i) {
      if (i !== 0) {
        output.push(value);
      }

      output.push(a[i]);
    }

    return output;
  };
};
