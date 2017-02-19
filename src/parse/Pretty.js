"use strict";

// TODO make this faster ?
function getLastLine(s) {
  // TODO handle Windows/Mac newlines ?
  return /[^\n]*$/.exec(s)[0];
}

// TODO make this faster ?
function repeat(s, i) {
  return new Array(i + 1).join(s);
}

exports.indentLine = function (a) {
  var length = a.length;

  var output = "";

  for (var i = 0; i < length; ++i) {
    if (i === 0) {
      output = a[i];

    } else {
      var last = getLastLine(output);

      // TODO Windows/Mac line endings ?
      output += a[i].replace(/\n/g, "\n" + repeat(" ", last.length));
    }
  }

  return output;
};


exports.indentNewline = function (indent) {
  return function (value) {
    // TODO Windows/Mac line endings ?
    return value.replace(/\n/g, "\n" + indent);
  };
};
