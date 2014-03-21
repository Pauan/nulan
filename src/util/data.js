define(["../../lib/util/key", "../../lib/util/uuid", "../../lib/util/object", "../../lib/scope", "./print"], function (a, b, c, d, e) {
  "use strict";

  var Key       = a.Key
    , v4        = b.v4
    , is        = c.is
    , iso       = c.iso
    , isIs      = c.isIs
    , Scope     = d.Scope
    , isPrint   = e.isPrint
    , isComplex = e.isComplex
    , print     = e.print
    , error     = e.error

  // macex
  var isGet     = Key("get?")
    , isSet     = Key("set?")
    , isMacro   = Key("macro?")
    , isMacex   = Key("macex?")
    , isPattern = Key("pattern?")
    , isSyntax  = Key("syntax?")

  function makeSetter(value) {
    return {
      get: function () {
        return value
      },
      set: function (x, f) {
        var old = value
        value = x
        try {
          return f()
        } finally {
          value = old
        }
      }
    }
  }

  var mode    = makeSetter("run")
    , local   = makeSetter(false)
    , module  = makeSetter(null)
    , context = makeSetter(null)


  // number
  Number.prototype[isPrint] = function (x, i) {
    return print(x.value, i)
  }
  Number.prototype[isMacex] = function (x) {
    return x
  }
  Number.prototype[isIs] = function (x, y) {
    return x instanceof Number &&
           y instanceof Number &&
           is(x.value, y.value)
  }


  // string
  function String(x) {
    this.value = x
  }
  String.prototype[isPrint] = function (x, i) {
    return print(x.value, i)
  }
  String.prototype[isMacex] = function (x) {
    return x
  }
  String.prototype[isIs] = function (x, y) {
    return x instanceof String &&
           y instanceof String &&
           is(x.value, y.value)
  }


  // symbol
  Symbol.prototype[isPrint] = function (x) {
    var s = x.value
    if (/^[0-9]+$|^[0-9]+\.[0-9]+$|[\(\)\{\}\[\]\.\\\'" \n]/.test(s)) {
      return "'" + s.replace(/['\\]/g, "\\$&").replace(/\n/g, "\\n") + "'"
    } else {
      return s
    }
  }
  Symbol.prototype[isMacex] = function (x) {
    return macexBox(toBox(x), x)
  }
  /*Symbol.prototype[isMacro] = function (a) {
    toBox(a[0])
    return macex([toBox(a[0])].concat(a.slice(1)))
  }*/
  Symbol.prototype[isIs] = function (x, y) {
    return x instanceof Symbol &&
           y instanceof Symbol &&
           is(x.value, y.value)
  }


  // compile
  function Op(s, a) {
    this.name = s
    this.args = a
  }
  Op.prototype[isComplex] = true
  Op.prototype[isPrint] = function (x, i) {
    return print([new Symbol(x.name)].concat(x.args), i)
    //var s = "#(op " + x.name + " "
    //return s + printNormal(x.args, s.length) + ")"
  }
  Op.prototype[isIs] = function (x, y) {
    return x instanceof Op &&
           y instanceof Op &&
           is(x.name, y.name) &&
           iso(x.args, y.args) // TODO
  }

  function opApply(s, x, a) {
    var o = new Op(s, a)
    if (x) {
      o.loc = x
    }
    return o
  }

  function op(s, x) {
    return opApply(s, x, [].slice.call(arguments, 2))
  }


  // parse
  function ParseStart() {}
  ParseStart.prototype[isPrint] = function () {
    return "["
  }

  function ParseEnd() {}
  ParseEnd.prototype[isPrint] = function () {
    return "]"
  }

  function ParseBypass(x) {
    this.value = x
  }
  ParseBypass.prototype[isPrint] = function (x, i) {
    return print(x.value, i)
  }
  /* TODO
  ParseBypass.prototype[isIs] = function (x, y) {
    return is(x.value, y)
  }*/

  return {
    vars: vars,
    isGet: isGet,
    isSet: isSet,
    isMacro: isMacro,
    isMacex: isMacex,
    isPattern: isPattern,
    isSyntax: isSyntax,
    local: local,
    mode: mode,
    path: path,
    context: context,
    external: external,
    Number: Number,
    String: String,
    Symbol: Symbol,
    Op: Op,
    opApply: opApply,
    op: op,
    Box: Box,
    getBox: getBox,
    compileBox: compileBox,
    compileBoxValue: compileBoxValue,
    toBox: toBox,
    ParseStart: ParseStart,
    ParseEnd: ParseEnd,
    ParseBypass: ParseBypass,
  }
})