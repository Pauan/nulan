define(["../../lib/util/uuid", "./macex", "./vars", "../print", "./symbol"], function (a, b, c, d, e) {
  "use strict";

  var v4      = a.v4
    , mode    = b.mode
    , local   = b.local
    , path    = b.path
    , vars    = c.vars
    , isPrint = d.isPrint
    , print   = d.print
    , error   = d.error
    , Symbol  = e.Symbol

  var boxId = 0

  var boxes = {}

  function Box(x) {
    var i      = "" + (++boxId)
    this.id    = v4().slice(0, -i.length) + i // TODO use something other than uuid v4 ?
    this.value = x
    this.mode  = mode.get()
    this.local = local.get()
    this.path  = path.get()
    boxes[this.id] = o
  }
  Box.prototype[isPrint] = function (x, i) {
    if (x.value != null) {
      var s = "#(box " + x.id + " "
      return s + print(new Symbol(x.value), i + s.length) + ")"
    } else {
      return "#(box " + x.id + ")"
    }
  }

  function getBox(i) {
    console.assert(i in boxes)
    return boxes[i]
  }
  
  function isBoxOrSym(x) {
    return x instanceof Box || x instanceof Symbol
  }
  
  function checkMode(x, y) {
    if (x[isInfo].mode !== mode.get()) {
      error(y, "undefined symbol: ", [y], " (but it exists at " + x[isInfo].mode + " time)")
    }
  }
  
  function toBox(x) {
    if (x instanceof Symbol) {
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
    console.assert(s instanceof Box)
    /*if (x instanceof data.Symbol) {

    } else if (!(x instanceof Box)) {
      error(x, "expected box or symbol but got ", [x])
    }*/
    return x instanceof Box && x.id === s.id
  }

  // TODO replace with generic equals check
  function checkBox(x, y) {
    // TODO: use isSym ?
    if (!isBox(toBox(x), y)) {
      // TODO is this good ?
      if (y instanceof Box) {
        y = new Symbol(y.value)
      }
      error(x, "expected ", [y], " but got ", [x])
    }
  }

  function setBox(x) {
    if (x instanceof Box) {
      return x
    } else if (x instanceof Symbol) {
      var o = new Box(x.value)
      o.loc = x.loc
      vars.set(x.value, o)
      return o
    } else {
      error(x, "expected symbol but got: ", [x])
    }
  }

  // TODO not sure if this should be in here or not...
  function getSyntax(x) {
    // TODO ew
    if (typeof x === "string" && vars.has(x)) {
      x = vars.get(x)
    } else if (x instanceof Symbol && vars.has(x.value)) {
      x = vars.get(x.value)
    }
    if (x instanceof Box && isInfo in x && x[isInfo].syntax != null) {
      return x[isInfo].syntax
    } else {
      return null
    }
  }

  return {
    Box: Box,
    getBox: getBox,
    
    isBoxOrSym: isBoxOrSym,
    checkMode: checkMode,
    toBox: toBox,
    isBox: isBox,
    checkBox: checkBox,
    setBox: setBox,
    getSyntax: getSyntax,
  }
})