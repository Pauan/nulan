define(["fs", "vm", "path", "./util/data"], function (a, b, c, d) {
  "use strict";

  var readFileSync  = a.readFileSync
    , createContext = b.createContext
    , runInContext  = b.runInContext
    , resolve       = c.resolve
    , relative      = c.relative
    , getBox        = d.getBox
    , path          = d.path

  function readFile(s) {
    return readFileSync(s, "utf8")
  }

  function newContext() {
    return createContext({
      getBox: getBox
    })
  }

  function relToAbs(s) {
    return resolve(path.get(), s)
  }

  function absToRel(s) {
    return relative(path.get(), s)
  }

  // TODO filename argument
  function evalString(s, o) {
    return runInContext(s, o)
  }

  return {
    cwd: process.cwd(),
    relToAbs: relToAbs,
    absToRel: absToRel,
    readFile: readFile,
    newContext: newContext,
    evalString: evalString,
  }
})