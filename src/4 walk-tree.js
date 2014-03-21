"use strict";

var a = require("../lib/util/object")
  , b = require("./1 tokenize")
  , c = require("./3 macex")

var isObject = a.isObject
  , isString = a.isString
  , isNumber = a.isNumber
  , Error    = b.Error

var statements
  , expressions
  , isStatement = false

var rules = {}

function validate(x) {
  if (!isObject(x)) {
    throw new Error(x, x + " is not an object")
  }
  if (!isObject(x.loc)) {
    throw new Error(x, x.type + " does not have a loc")
  }
  if (x.loc.source == null) {
    throw new Error(x, x.type + " does not have a loc.source")
  }
  if (!isObject(x.loc.start)) {
    throw new Error(x, x.type + " does not have a loc.start")
  }
  if (!isObject(x.loc.end)) {
    throw new Error(x, x.type + " does not have a loc.end")
  }
  // TODO error checking for loc.start.column, loc.start.line, loc.end.column, and loc.end.line
  if (rules[x.type] == null) {
    throw new Error(x, x.type + " does not have a rule")
  }
}

function isDestructive(x) {
  validate(x)
  // TODO proxies `x.type === "MemberExpression"`
  // TODO yield expression
  return x.type === "BreakStatement"      || x.type === "ContinueStatement"    ||
         x.type === "ReturnStatement"     || x.type === "ThrowStatement"       ||
         x.type === "FunctionDeclaration" || x.type === "VariableDeclaration"  ||
         x.type === "VariableDeclarator"  || x.type === "AssignmentExpression" ||
         x.type === "UpdateExpression"    || x.type === "NewExpression"        ||
         x.type === "CallExpression"      || (x.type === "UnaryExpression" && x.operator === "delete")
}

function withStatements(f) {
  var old  = statements
    , old2 = expressions
  statements  = []
  expressions = []
  try {
    return f()
  } finally {
    statements  = old
    expressions = old2
  }
}

function statement1(x) {
  var r = rules[x.type]
  if (r.statement != null) {
    isStatement = true
    if (r.walk != null) {
      r.walk(x)
    }
    r.statement(x)
  } else {
    var y = expression(x)
    statements.push({
      loc: y.loc,
      type: "ExpressionStatement",
      expression: y
    })
  }
}

function expression1(x) {
  var r = rules[x.type]
  if (r.walk != null) {
    r.walk(x)
  }
  if (r.expression != null) {
    return r.expression(x)
  } else {
    return x
  }
}

function block1(a) {
  return withStatements(function () {
    a.forEach(function (x) {
      validate(x)
      statement1(x)
    })
    return statements
  })
}

function checkType(x, s) {
  if (x.type !== s) {
    throw new Error(x, "expected type " + s + " but got " + x.type)
  }
}

function checkProperty(x) {
  if (x.type === "Literal") {
    if (!(isNumber(x.value) || isString(x.value))) {
      throw new Error(x, "property must be a string, number, or identifier, but got " + x.value)
    }
  } else if (x.type !== "Identifier") {
    throw new Error(x, "expected type Literal or Identifier but got " + x.type)
  }
}

function walkTree(r) {
  return block1(r)
}
exports.walkTree = walkTree


function walk(x) {
  if (x === null) {
    return x
  } else {
    validate(x)
    var r = rules[x.type]
    if (r.walk != null) {
      r.walk(x)
    }
    return x
  }
}

function block(x, s) {
  if (x === null) {
    return x
  } else {
    validate(x)
    checkType(x, "BlockStatement")
    // TODO use Object.create here ?
    x.body = block1(x.body)
    return x
  }
}

function statement(x, s) {
  if (x === null) {
    return x
  } else {
    validate(x)
    return withStatements(function () {
      statement1(x)
      return {
        loc: x.loc, // TODO is this correct ?
        type: "BlockStatement",
        body: statements
      }
    })
  }
}

function expression(x, s) {
  if (x === null) {
    return x
  } else {
    validate(x)
    x = expression1(x)
    if (isDestructive(x)) {
      expressions.push(x)
    }
    return x
  }
}

function identifier(x) {
  if (x === null) {
    return x
  } else {
    validate(x)
    checkType(x, "Identifier")
    return expression(x)
  }
}

function property(x) {
  if (x === null) {
    return x
  } else {
    validate(x)
    checkProperty(x)
    return expression(x)
  }
}

function pattern(x) {
  return expression(x)
}

function zap(f, x, s) {
  validate(x)
  x[s] = f(x[s])
}

function zapMap(f, x, s) {
  x[s] = x[s].map(function (x) {
    return f(x)
  })
}


rules["BlockStatement"] = {
  walk: function (x) {
    zapMap(statement, x, "body")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["IfStatement"] = {
  walk: function (x) {
    var old = isStatement
    isStatement = false
    try {
      zap(expression, x, "test")
      zap(statement, x, "consequent")
      zap(statement, x, "alternate")
    } finally {
      console.log(isStatement)
      isStatement = old
    }
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["ReturnStatement"] = rules["ThrowStatement"] = {
  walk: function (x) {
    zap(expression, x, "argument")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["TryStatement"] = {
  walk: function (x) {
    zap(block, x, "block")
    zap(walk, x, "handler") // TODO
    zap(block, x, "finalizer")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["CatchClause"] = {
  walk: function (x) {
    zap(pattern, x, "param")
    zap(block, x, "body")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["WhileStatement"] = {
  walk: function (x) {
    zap(expression, x, "test")
    zap(statement, x, "body")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["VariableDeclaration"] = {
  walk: function (x) {
    zapMap(expression, x, "declarations")
  },
  statement: function (x) {
    statements.push(x)
  }
}

rules["VariableDeclarator"] = {
  walk: function (x) {
    zap(pattern, x, "id")
    zap(expression, x, "init")
  }
}

rules["ArrayExpression"] = {
  walk: function (x) {
    zapMap(expression, x, "elements")
  }
}

rules["ObjectExpression"] = {
  walk: function (x) {
    x.properties.forEach(function (x) {
      zap(property, x, "key")
      zap(expression, x, "value")
    })
  }
}

rules["FunctionExpression"] = {
  walk: function (x) {
    zap(identifier, x, "id")
    zapMap(pattern, x, "params")
    zapMap(expression, x, "defaults")
    zap(identifier, x, "rest")
    if (x.expression) {
      zap(expression, x, "body")
    } else {
      zap(block, x, "body")
    }
  }
}

rules["SequenceExpression"] = {
  walk: function (x) {
    zapMap(expression, x, "expressions")
  }
}

rules["UnaryExpression"] = {
  walk: function (x) {
    zap(expression, x, "argument")
  }
}

rules["BinaryExpression"] = rules["AssignmentExpression"] = rules["LogicalExpression"] = {
  walk: function (x) {
    zap(expression, x, "left")
    zap(expression, x, "right")
  }
}

rules["NewExpression"] = rules["CallExpression"] = {
  walk: function (x) {
    zap(expression, x, "callee")
    zapMap(expression, x, "arguments")
  }
}

rules["MemberExpression"] = {
  walk: function (x) {
    zap(expression, x, "object")
    if (x.computed) {
      zap(expression, x, "property")
    } else {
      zap(identifier, x, "property")
    }
  }
}

rules["Identifier"] = rules["Literal"] = rules["x-nulan-box"] = {}