define(["./data", "./box"], function (data, box) {
  "use strict";
  
  function macex1(a) {
    var x = box.toBox(a[0])
    if (x instanceof box.Box && x.macex != null) {
      return x.macex(a)
    } else {
      return a.map(macex)
    }
  }

  function macex(x) {
    if (Array.isArray(x)) {
      return macex1(x)
    } else if (x instanceof box.Box) {
      if (x.get != null) {
        return x.get([x])
      } else {
        return x
      }
    } else if (x instanceof data.Symbol) {
      return macex(box.toBox(x))
    } else if (x instanceof data.Number || x instanceof data.String) {
      return x
    } else {
      throw new data.Error(x, "unexpected datatype: " + data.print(x))
    }
  }
  
  return macex
})
