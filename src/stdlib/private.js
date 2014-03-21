function type(x) {
  return {}.toString.call(x)
}

function typeChecker(s) {
  return function (x) {
    return type(x) === s
  }
}

var isString = typeChecker("[object String]")
var isDict   = typeChecker("[object Object]")

function isObject(x) {
  return Object(x) === x
}

function isArrayLike(x) {
  return (isObject(x) && "length" in x) || isString(x)
}