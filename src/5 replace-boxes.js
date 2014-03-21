"use strict";

var a = require("../lib/util/object")
  , b = require("../lib/scope")
  , c = require("./1 tokenize")
  , d = require("./2 parse")

var isObject   = a.isObject
  , isString   = a.isString
  , isNumber   = a.isNumber
  , isRegExp   = a.isRegExp
  , isBoolean  = a.isBoolean
  , isFunction = a.isFunction
  , Scope      = b.Scope
  , Error      = c.Error
  , Symbol     = c.Symbol
  , proxy      = d.proxy

function getNextUniq(s) {
  var r = s.split("")
    , i = r.length
  while (i--) {
    if (r[i] === "z") {
      r[i] = "a"
    } else {
      r[i] = String.fromCharCode(r[i].charCodeAt(0) + 1)
      return r.join("")
    }
  }
  return r.join("") + "a"
}

function boxToSym(x, symbols) {
  var s
  if (x.name == null) {
    s = "a"
    while (s in symbols) {
      s = getNextUniq(s)
    }
  } else {
    var orig = x.name //mangle(x.value)
      , i    = 2
    s = orig
    while (s in symbols) {
      s = orig + i
      ++i
    }
  }
  return new Symbol(s, x.loc)
}

// TODO "ObjectPattern" "ArrayPattern"
function setBox(x, boxes, symbols) {
  if (x !== null) {
    if (x.type === "x-nulan-box") {
      var s = boxToSym(x, symbols)
      boxes[x.id] = s
      symbols[s.name] = true
    }
  }
}

// Finds all symbols in the current scope
// TODO "BreakStatement" "ContinueStatement" "ObjectExpression" "MemberExpression" do not use actual identifiers
function findSymbols(x) {
  var symbols = {}
  walk(x, function (node) {
    if (node.type === "Identifier") {
      symbols[node.name] = true
    }
    return node
  })
  return symbols
}

function replaceBoxes(x) {
  var symbols = findSymbols(x)
    , boxes   = {}

  // TODO "LetStatement"
  // TODO "ArrowExpression"
  // TODO "LetExpression"
  // TODO "LabeledStatement" ?
  return walk(x, function (node) {
    if (node.type === "x-nulan-box") {
      if (node.id in boxes) {
        return proxy(boxes[node.id], node.loc)
      } else {
        throw new Error(node, node + " used before assignment")
      }
    } else if (node.type === "FunctionDeclaration" || node.type === "FunctionExpression") {
      setBox(node.id, boxes, symbols)
      node.params.forEach(function (x) {
        setBox(x, boxes, symbols)
      })
      setBox(node.rest, boxes, symbols)
    } else if (node.type === "VariableDeclarator") {
      setBox(node.id, boxes, symbols)
    } else if (node.type === "CatchClause") {
      setBox(node.param, boxes, symbols)
    }
    return node
  })
}
exports.replaceBoxes = replaceBoxes


var types = {}

function Type(s, parent, f) {
  this.name     = s
  this.parent   = parent
  this.children = f
  types[s] = this
}

function Or(args) {
  this.args = args
}

function Is(args) {
  this.args = args
}

function isa(x, y) {
  while (true) {
    if (x === y) {
      return true
    } else if (x === null) {
      return false
    } else {
      x = types[x]
      x = x.parent
    }
  }
}

function check(node, x, f) {
  if (Array.isArray(x)) {
    if (!Array.isArray(node)) {
      throw new Error(node, "expected array but got " + node)
    }
    x = x[0]
    return node.map(function (node) {
      return check(node, x, f)
    })

  } else if (x instanceof Or) {
    for (var i = 0; i < x.args.length; ++i) {
      try {
        return check(node, x.args[i], f)
      } catch (e) {}
    }
    throw new Error(node, "expected any of [" + x.args.join(", ") + "] but got " + node.type)

  } else if (x instanceof Is) {
    for (var i = 0; i < x.args.length; ++i) {
      if (x.args[i] === node) {
        return node
      }
    }
    throw new Error(node, "expected any of [" + x.args.join(", ") + "] but got " + node)

  } else if (isString(x)) {
    if (!isa(node.type, x)) {
      throw new Error(node, "expected type of " + x + " but got " + node.type)
    }
    return walk(node, f)

  } else if (x === null) {
    if (node !== null) {
      throw new Error(node, "expected null but got " + null)
    }
    return node

  } else if (isFunction(x)) {
    x(node)
    return node

  } else {
    console.log(node, x)
    if (!isObject(node)) {
      throw new Error(node, "expected object but got " + node)
    }
    throw new Error(x, "invalid pattern: " + x)
  }
}

function walk(node, f) {
  if (typeof node.type !== "string") {
    throw new Error(node, "node must have a type")
  }
  if (isObject(node.loc)) {
    if (!isObject(node.loc.start)) {
      throw new Error(node, "node must have a loc.start")
    }
    if (!isObject(node.loc.end)) {
      throw new Error(node, "node must have a loc.end")
    }
    if (!isString(node.loc.source)) {
      throw new Error(node, "node must have a loc.source")
    }
    if (!isNumber(node.loc.start.line)) {
      throw new Error(node, "node must have a loc.start.line")
    }
    if (!isNumber(node.loc.start.column)) {
      throw new Error(node, "node must have a loc.start.column")
    }
    if (!isNumber(node.loc.end.line)) {
      throw new Error(node, "node must have a loc.end.line")
    }
    if (!isNumber(node.loc.end.column)) {
      throw new Error(node, "node must have a loc.end.column")
    }
  } else {
    //throw new Error(node, "node must have a loc")
  }

  var r = types[node.type]
  if (r == null) {
    throw new Error(node, "unknown type " + node.type)
  }

  var x = f(node)
  for (var s in r.children) {
    node[s] = check(node[s], r.children[s], f)
  }
  return x
}


function type(s, parent, o) {
  return new Type(s, parent, o)
}

function or() {
  return new Or([].slice.call(arguments))
}

function is() {
  return new Is([].slice.call(arguments))
}

var string = function (x) {
  if (!isString(x)) {
    throw new Error(x, "expected string but got " + x)
  }
}

var bool = function (x) {
  if (!isBoolean(x)) {
    throw new Error(x, "expected boolean but got " + x)
  }
}

var number = function (x) {
  if (!isNumber(x)) {
    throw new Error(x, "expected number but got " + x)
  }
}

var regexp = function (x) {
  if (!isRegExp(x)) {
    throw new Error(x, "expected regexp but got " + x)
  }
}


type("Node", null, {})

type("Statement", "Node", {})

type("Pattern", "Node", {})

type("Expression", "Pattern", {})

type("Program", "Node", {
  body: [ "Statement" ]
})

type("BlockStatement", "Statement", {
  body: [ "Statement" ]
})

type("EmptyStatement", "Statement", {})

type("ExpressionStatement", "Statement", {
  expression: "Expression"
})

type("IfStatement", "Statement", {
  test: "Expression",
  consequent: "Statement",
  alternate: or("Statement", null)
})

type("LabeledStatement", "Statement", {
  label: "Identifier",
  body: "Statement"
})

type("BreakStatement", "Statement", {
  label: or("Identifier", null)
})

type("ContinueStatement", "Statement", {
  label: or("Identifier", null)
})

/*type("WithStatement", "Statement", {
  object: "Expression",
  body: "Statement"
})*/

type("SwitchStatement", "Statement", {
  discriminant: "Expression",
  cases: [ "SwitchCase" ],
  lexical: bool
})

type("ReturnStatement", "Statement", {
  argument: or("Expression", null)
})

type("ThrowStatement", "Statement", {
  argument: "Expression"
})

type("TryStatement", "Statement", {
  block: "BlockStatement",
  handler: or("CatchClause", null),
  //guardedHandlers: [ "CatchClause" ],
  finalizer: or("BlockStatement", null)
})

type("WhileStatement", "Statement", {
  test: "Expression",
  body: "Statement"
})

type("DoWhileStatement", "Statement", {
  body: "Statement",
  test: "Expression"
})

type("ForStatement", "Statement", {
  init: or("VariableDeclaration", "Expression", null),
  test: or("Expression", null),
  update: or("Expression", null),
  body: "Statement"
})

type("ForInStatement", "Statement", {
  left: or("VariableDeclaration", "Expression"),
  right: "Expression",
  body: "Statement",
  each: bool
})

/*type("ForOfStatement", "Statement", {
  left: or("VariableDeclaration", "Expression"),
  right: "Expression",
  body: "Statement",
})*/

/*type("LetStatement", "Statement", {
  head: [ { id: "Pattern", init: or("Expression", null) } ],
  body: "Statement"
})*/

type("DebuggerStatement", "Statement", {})

type("FunctionDeclaration", "Statement", {
  id: "Identifier",
  params: [ "Pattern" ],
  defaults: [ "Expression" ],
  rest: or("Identifier", null),
  body: "BlockStatement",
  //body: or("BlockStatement", "Expression"),
  //generator: bool,
  //expression: bool
})

type("VariableDeclaration", "Statement", {
  declarations: [ "VariableDeclarator" ],
  kind: is("var", "let", "const")
})

type("VariableDeclarator", "Node", {
  id: "Pattern",
  init: or("Expression", null)
})

type("ThisExpression", "Expression", {})

type("ArrayExpression", "Expression", {
  elements: [ or("Expression", null) ]
})

type("ObjectExpression", "Expression", {
  properties: [ { key:   or("Literal", "Identifier")
                , value: "Expression"
                , kind:  is("init", "get", "set") } ]
})

type("FunctionExpression", "Expression", {
  id: or("Identifier", null),
  params: [ "Pattern" ],
  defaults: [ "Expression" ],
  rest: or("Identifier", null),
  body: "BlockStatement",
  //body: or("BlockStatement", "Expression"),
  //generator: bool,
  //expression: bool
})

/*type("ArrowExpression", "Expression" {
  params: [ "Pattern" ],
  defaults: [ "Expression" ],
  rest: or("Identifier", null),
  body: "BlockStatement",
  llbody: or("BlockStatement", "Expression"),
  //generator: bool,
  //expression: bool
})*/

type("SequenceExpression", "Expression", {
  expressions: [ "Expression" ]
})

type("UnaryExpression", "Expression", {
  operator: is("-", "+", "!", "~", "typeof", "void", "delete"),
  prefix: bool,
  argument: "Expression"
})

type("BinaryExpression", "Expression", {
  operator: is("==", "!=", "===", "!==", "<", "<=", ">", ">=", "<<", ">>", ">>>", "+", "-", "*", "/", "%", "|", "^", "&", "in", "instanceof", ".."),
  left: "Expression",
  right: "Expression"
})

type("AssignmentExpression", "Expression", {
  operator: is("=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "|=", "^=", "&="),
  left: "Expression",
  right: "Expression"
})

type("UpdateExpression", "Expression", {
  operator: is("++", "--"),
  argument: "Expression"
})

type("LogicalExpression", "Expression", {
  operator: is("||", "&&"),
  left: "Expression",
  right: "Expression"
})

type("ConditionalExpression", "Expression", {
  test: "Expression",
  alternate: "Expression",
  consequent: "Expression"
})

type("NewExpression", "Expression", {
  callee: "Expression",
  arguments: [ "Expression" ]
})

type("CallExpression", "Expression", {
  callee: "Expression",
  arguments: [ "Expression" ]
})

type("MemberExpression", "Expression", {
  object: "Expression",
  property: or("Identifier", "Expression"),
  computed: bool
})

/*type("LetExpression", "Expression", {
  head: [ { id: "Pattern", init: or("Expression", null) } ],
  body: "Expression"
})*/

/*type("ObjectPattern", "Pattern", {
  properties: [ { key:   or("Literal", "Identifier")
                , value: "Pattern" } ]
})

type("ArrayPattern", "Pattern", {
  elements: [ or("Pattern", null) ]
})*/

type("SwitchCase", "Node", {
  test: or("Expression", null),
  consequent: [ "Statement" ]
})

type("CatchClause", "Node", {
  param: "Pattern",
  //guard: or("Expression", null),
  body: "BlockStatement"
})

type("Identifier", "Expression", {
  name: string
})

type("Literal", "Expression", {
  value: or(string, bool, number, regexp, null)
})

type("x-nulan-box", "Expression", {})