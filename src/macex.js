define(["./data", "./box", "./error", "./state"], function (data, box, error, state) {
  "use strict";
  
  function moduleImport(m, x, s) {
    var u = box.make("a")
    var a = /^js!(.*)$/.exec(s)
    if (a !== null) {
      console.log(a)
      s = new data.String(a[1])
    } else {

    }
    console.log(s)
    m.arguments.push(u)
    m.imports.push(new data.String(s))
    return macex([get("vars"), [get("="), x, u]])
  }
  
  function macex1(a) {
    var x = box.toBox(a[0])
    if (x instanceof data.Box && data.macex in x) {
      return x[data.macex](a)
    } else {
      if (a.length === 0) {
        return new data.Op("empty", [])
      } else {
        return new data.Op("call", a.map(macex))
      }
    }
  }
  
  function compileBox(x) {
    return new data.Op("call", [new data.Op(".", [new data.Symbol("box"),
                                                  new data.String("get")]),
                                new data.String(x.id)])
  }
  
  function compileBoxValue(x) {
    return new data.Op(".", [compileBox(x), new data.String("v")])
  }

  function macex(x) {
    if (x == null) {
      return new data.Op("empty", [])
    } else if (Array.isArray(x)) {
      return macex1(x)
    } else if (x instanceof data.MacexBypass) {
      return x.value
    } else if (x instanceof data.Box) {
      if (data.get in x) {
        return x[data.get]([x])
      } else {
        if (state.mode.get() === "run" || x.local) {
          return x
        } else {
          return compileBoxValue(x)
        }
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
  
  return {
    macex: macex,
    compileBox: compileBox,
    compileBoxValue: compileBoxValue,
    moduleImport: moduleImport,
  }
})
