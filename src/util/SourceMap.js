"use strict";

var $sourceMap = require("source-map");


function from(filename) {
  return function (line) {
    return function (column) {
      return function (s) {
        return new $sourceMap.SourceNode(line, column, filename, s);
      };
    };
  };
}


exports.fromStringImpl = from;
exports.fromArrayImpl = from;


exports.generateImpl = function (ret) {
  return function (filename) {
    return function (source) {
      return function (s) {
        var x = s.toStringWithSourceMap({ file: filename });
        // TODO is this correct ?
        return ret(x.code + "\n//# sourceMappingURL=" + source)("" + x.map);
      };
    };
  };
};
