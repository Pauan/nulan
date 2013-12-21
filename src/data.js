define([], function () {
  "use strict";

  var expression = new Name()
    , statement  = new Name()
    , compile    = new Name()

    , start  = new Name()
    , end    = new Name()
    , bypass = new Name()

  function loc(x, y) {
    return {
      source: x.source,
      start: x.start,
      end: y.end
    }
  }

  function MacexBypass(x) {
    this.value = x
  }
})