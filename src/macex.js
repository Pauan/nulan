define(["./data", "./box", "./error"], function (data, box, error) {
  "use strict";
  
  function macex1(a) {
    var x = box.toBox(a[0])
    if (x instanceof data.Box && x.macex != null) {
      return x.macex(a)
    } else {
      if (a.length === 0) {
        return new data.Op("empty", [])
      } else {
        return new data.Op("call", a.map(macex))
      }
    }
  }

  function macex(x) {
    if (x == null) {
      return new data.Op("empty", [])
    } else if (Array.isArray(x)) {
      return macex1(x)
    } else if (x instanceof data.MacexBypass) {
      return x.value
    } else if (x instanceof data.Box) {
      if (x.get != null) {
        return x.get([x])
      } else {
        return x
        // TODO
        return new data.Op("call", [new data.Op(".", [new data.Symbol("box"),
                                                      new data.String("get")]),
                                    new data.Number(x.id)])
      }
    } else if (x instanceof data.Symbol) {
      return macex(box.toBox(x))
    } else if (x instanceof data.Number || x instanceof data.String) {
      return x
    } else if (typeof x === "number") {
      return new data.Number(x)
    } else if (typeof x === "string") {
      return new data.String(x)
    } else {
      error(x, "unexpected datatype: ", [x])
    }
  }
  
  return macex
})
