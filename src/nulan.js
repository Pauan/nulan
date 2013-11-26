define(["./macex", "./builtins", "./compile", "./rules", "./tokenize", "./parse"], function (macex, builtins, compile, rules, tokenize, parse) {
  "use strict";
  
  function nulanCompile(x, filename) {
    var o = tokenize.tokenize(x, rules.rules, filename)
      , r = []
    return compile.module(function () {
      while (o.has()) {
        r.push(compile.statement(macex.macex(parse(o, rules.rules))))
      }
      return compile.module(r)
    })
  }
  
  return {
    compile: nulanCompile,
  }
})
