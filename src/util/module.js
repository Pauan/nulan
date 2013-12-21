define(["./data", "./options", "./state", "./box"], function (data, options, state, box) {
  "use strict";

  function relToAbs(abs, rel) {
    var a1 = (rel[0] === "/"
               ? []
               : abs.split(/\//))
      , a2 = rel.split(/\//)
    a2.forEach(function (x) {
      if (x === "..") {
        if (a1.length === 1) {
          throw new Error("invalid module path: " + rel)
        } else {
          a1.pop()
        }
      } else if (x !== ".") {
        a1.push(x)
      }
    })
    return a1.join("/")
  }

  function importBox(x) {
    var m = state.module.get()
    if (m.importArguments[x.module.path] == null) {
      // TODO global and runtime ?
      m.importArguments[x.module.path] = x.module.importBox
      //m.arguments.push(x.module.importBox)
      //m.imports.push(new data.String(s))
    }

    //if (x.mode === "run") {
    //m.assigns.push(op("var", x, [op("=", x, [o, op(".", x, [x.module.importBox, new data.String(x.value)])])]))
    //}
  }

  function make() {
    var o = new data.Module()
    o.importArguments = {}
    o.assigns         = []
    o.arguments       = []
    o.imports         = []

    // TODO global and runtime ?
    o.exportBox       = box.make()
    o.exports         = {}
    return o
  }

  function getBox(m, s, y) {
    var o = m.exports[y.value]
    if (m.isJavaScript) {
      // TODO needs to be both global and runtime
      if (o == null) {
        o = m.exports[y.value] = box.make(y.value)
        o.loc = y.loc
      }
    } else {
      if (o == null) {
        error(y, "module ", [s], "does not have an export for variable ", [y])
      }
    }
    return o
  }

  function fromPath(s) {
    var a = /^js!(.*)$/.exec(s)
    if (a !== null) {
      s = a[1]
    }

    if (options.basedir == null) {
      throw new Error("options::basedir must be set")
    }
    var abs = relToAbs(options.basedir, s)

    var imp = state.moduleCache[abs]
    if (imp == null) {
      imp = state.moduleCache[abs] = make()
      imp.isJavaScript = (a !== null)
      imp.path         = abs
    }
    return imp
  }

  return {
    importBox: importBox,
    make: make,
    fromPath: fromPath,
    getBox: getBox,
  }
})
