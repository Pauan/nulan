define(["../util/data", "./compile", "../filesystem"], function (a, b, c) {
  "use strict";
  
  var op        = a.op
    , opApply   = a.opApply
    , Op        = a.Op
    , Symbol    = a.Symbol
    , String    = a.String
    , Box       = a.Box
    , path      = a.path
    , statement = b.statement
    , absToRel  = c.absToRel

  function isLast(x, last) {
    return x instanceof Box && last[x.value] === x.id
  }

  function toProperty(x, u) {
    return op(".", x, u, new String(x.value))
  }

  function exported(x, u, last) {
    if (x instanceof Op) {
      if (x.name === "var") {
        var r = []
        x.args.forEach(function (x) {
          if (x instanceof Op && x.name === "=") {
            var y = x.args[0]
            if (isLast(y, last)) {
              r.push(exported(x, u, last))
            } else {
              r.push(op("var", x, exported(x, u, last)))
            }
          } else if (isLast(x, last)) {
            // TODO not sure if this should do this or not...
            //r.push(op("=", x, toProperty(x, u), op("empty", x)))
          } else {
            r.push(op("var", x, exported(x, u, last)))
          }
        })
        return opApply(",", x, r)
      } else {
        x.args = x.args.map(function (x) {
          return exported(x, u, last)
        })
        return x
      }
    } else if (isLast(x, last)) {
      return toProperty(x, u)
    } else {
      return x
    }
  }

  function isExported(x) {
    return x instanceof Box && !x.local && x.value != null && x.path === path.get() && !x.external
  }

  function findLast(x, last) {
    if (x instanceof Op) {
      if (x.name === "var") {
        x.args.forEach(function (x) {
          if (x instanceof Op && x.name === "=") {
            var y = x.args[0]
            if (isExported(y)) {
              last[y.value] = y.id
            } else {
              findLast(y, last)
            }
            findLast(x.args[1], last)
          } else if (isExported(x)) {
            last[x.value] = x.id
          } else {
            findLast(x, last)
          }
        })
      } else {
        x.args.forEach(function (x) {
          findLast(x, last)
        })
      }
    }
  }

  function getPath(path, s, o) {
    for (var i = 0; i < path.length; ++i) {
      if (path[i].path === s) {
        return path[i]
      }
    }
    o.path = s
    o.arg  = new Box()
    path.push(o)
    return o
  }

  function findImports(x, paths, imported) {
    if (x instanceof Op) {
      x.args.forEach(function (x) {
        findImports(x, paths, imported)
      })
    } else if (x instanceof Box) {
      // TODO check if !x.local ?
      if (x.path !== path.get() && !imported[x.id]) {
        imported[x.id] = true
        getPath(paths, x.path, { imports: [] }).imports.push(x)
      }
    }
  }

  function findRequires(x, paths) {
    if (x instanceof Op) {
      if (x.name === "require") {
        return getPath(paths, x.args[0], {}).arg
      } else {
        x.args = x.args.map(function (x) {
          return findRequires(x, paths)
        })
        return x
      }
    } else {
      return x
    }
  }

  function module(top) {
    var paths    = []
      , requires = []
      , imported = {}
      , last     = {}

    top.forEach(function (x) {
      findLast(x, last)
      findImports(x, paths, imported)
    })

    var u = new Box("exports")

    top = top.map(function (x) {
      return exported(findRequires(x, requires), u, last)
    })

    var imports = [new String("exports")]
      , args    = [u]
      , vars    = []

    paths.forEach(function (x) {
      imports.push(new String("./" + absToRel(x.path)))
      args.push(x.arg)
      x.imports.forEach(function (y) {
        vars.push(op("=", y, y, toProperty(y, x.arg)))
      })
    })

    requires.forEach(function (x) {
      imports.push(x.path)
      args.push(x.arg)
    })

    if (vars.length) {
      top = [opApply("var", null, vars)].concat(top)
    }

    top = [new String("use strict")].concat(top)

    return statement([op("call", null, new Symbol("define"), opApply("array", null, imports),
                        op("function", null, opApply(",", null, args), opApply(",", null, top)))])
  }

  return {
    module: module,
  }
})