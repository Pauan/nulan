define(["./data", "./scope", "./error"], function (data, scope, error) {
  "use strict";
  
  var boxId = 0
  
  var boxes = scope.make()
    , vars  = scope.make()
  
  function withNewEnvironment(f) {
    var old1 = boxId
    boxes.push()
    vars.push()
    try {
      return f()
    } finally {
      boxId = old1
      boxes.pop()
      vars.pop()
    }
  }
  
  function make(x) {
    var o   = new data.Box()
    o.id    = boxId++
    o.value = x
    boxes.set(o.id, o)
    return o
  }
  
  function get(i) {
    console.assert(boxes.has(i))
    return boxes.get(i)
  }
  
  function toBox(x) {
    if (x instanceof data.Symbol) {
      if (vars.has(x.value)) {
        var y = vars.get(x.value)
        // TODO: not sure if this should enrich or not...
        //       it mostly affects macros:
        //
        //         $mac foo ->
        //           'sym "5"
        //         foo;
        y.loc = x.loc
        return y
        /*var o = Object.create(y)
        o.loc = x.loc
        return o*/
      } else {
        error(x, "undefined symbol: ", [x])
      }
    } else {
      return x
      //error(x, "expected box or symbol but got ", [x])
    }
  }
  
  function isBox(x, s) {
    x = toBox(x)
    console.assert(s instanceof data.Box)
    /*if (x instanceof data.Symbol) {
      
    } else if (!(x instanceof Box)) {
      error(x, "expected box or symbol but got ", [x])
    }*/
    return x instanceof data.Box && x.id === s.id
  }
  
  function check(x, y) {
    // TODO: use isSym ?
    if (!isBox(toBox(x), y)) {
      error(x, "expected ", [y], " but got ", [x])
    }
  }
  
  function set(x) {
    if (x instanceof data.Box) {
      return x
    } else if (x instanceof data.Symbol) {
      var o = make(x.value)
      o.loc = x.loc
      vars.set(x.value, o)
      return o
    } else {
      error(x, "expected symbol but got ", [x])
    }
  }
  
  // TODO not sure if this should be in here or not...
  function getSyntax(x) {
    // TODO ew
    if (typeof x === "string" && vars.has(x)) {
      x = vars.get(x)
    } else if (x instanceof data.Symbol && vars.has(x.value)) {
      x = vars.get(x.value)
    }
    if (x instanceof data.Box && data.syntax in x) {
      return x[data.syntax]
    } else {
      return null
    }
  }
  
  return {
    vars: vars,
    withNewEnvironment: withNewEnvironment,

    make: make,
    get: get,
    toBox: toBox,
    isBox: isBox,
    check: check,
    set: set,
    getSyntax: getSyntax,
  }
})
