define(["./data", "./scope"], function (data, scope) {
  "use strict";
  
  var boxId = 0
    , boxes = {}

  var vars = scope.make()
  
  function Box(x) {
    this.id    = boxId++
    this.value = x
    boxes[this.id] = this
  }
  Box.prototype.toString = function () {
    if (this.value != null) {
      return "#<box:" + this.id + ":" + this.value + ">"
    } else {
      return "#<box:" + this.id + ">"
    }
  }
  
  function get(i) {
    console.assert(i in boxes)
    return boxes[i]
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
        var o = Object.create(y)
        o.loc = x.loc
        return o
      } else {
        throw new data.Error(x, "undefined symbol")
      }
    } else {
      return x
      //throw new data.Error(x, "expected box or symbol but got " + data.print(x))
    }
  }
  
  function isBox(x, s) {
    console.assert(s instanceof Box)
    /*if (x instanceof data.Symbol) {
      
    } else if (!(x instanceof Box)) {
      throw new data.Error(x, "expected box or symbol but got " + data.print(x))
    }*/
    return x instanceof Box && x.id === s.id
  }
  
  function check(x, y) {
    // TODO: use isSym ?
    if (!isBox(toBox(x), y)) {
      throw new data.Error(x, "expected " + y + " but got " + data.print(x))
    }
  }
  
  function set(x) {
    if (x instanceof Box) {
      return x
    } else if (x instanceof data.Symbol) {
      var o = new Box(x.value)
      o.loc = x.loc
      vars.set(x.value, o)
      return o
    } else {
      throw new data.Error(x, "expected symbol but got " + data.print(x))
    }
  }
  
  // TODO not sure if this should be in here or not...
  function getSyntax(s) {
    if (vars.has(s)) {
      var x = vars.get(s)
      console.assert(x instanceof Box)
      if (x.syntax != null) {
        return x.syntax
      }
    }
    return null
  }
  
  return {
    vars: vars,

    Box: Box,
    get: get,
    toBox: toBox,
    isBox: isBox,
    check: check,
    set: set,
    getSyntax: getSyntax,
  }
})
