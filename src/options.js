define(["./print"], function (print) {
  "use strict";
  return {
    minified: false,
    $eval: function (s) {},
    warn: function (s) {
      console.warn(print.array(s))
    },
    mangle: function (s) {
      return s.replace(/^[0-9]/, "_$&").replace(/[^$a-zA-Z0-9]/g, function (s) {
        return s === "_" ? "__" : "_" + s.charCodeAt(0) + "_"
      })
    },
    unmangle: function (s) {
      // |([a-z])([A-Z])
      return s.replace(/_([0-9]*)_|^_([0-9])/g, function (_, s, s1) {
        console.log(s1)
        if (s1) {
          return s1
        } else {
          return s === "" ? "_" : String.fromCharCode(s)
        }
      })
    }
  }
})
