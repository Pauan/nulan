define(["./scope"], function (a) {
  "use strict";

  var Scope = a.Scope

  var vars = new Scope()

  return {
    vars: vars,
  }
})