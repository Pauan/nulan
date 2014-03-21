var nil = void 0

function isNil(x) {
  return x === nil
}

function isList(x) {
  return isArrayLike(x) || isNil(x)
}

function len(x) {
  if (isArrayLike(x)) {
    return x.length
  } else if (isNil(x)) {
    return 0
  } else {
    throw new TypeError("cannot use len on " + x)
  }
}

function is(x, y) {
  if (x === y) {
    return true
  } else if (isNil(x) && isNil(y)) {
    return true
  } else {
    // NaN !== NaN, but they are identical.
    // NaNs are the only non-reflexive value, i.e., if x !== x,
    // then x is a NaN.
    // isNaN is broken: it converts its argument to number, so
    // isNaN("foo") => true
    return x !== x && y !== y
  }
}

function place(x, key, f) {
  if (isDict(x)) {
    if (key in x) {
      return f(x[key], true)
    } else {
      return f(nil, false)
    }
  } else if (isNil(x)) {
    return f(nil, false)
  } else {
    throw new TypeError("expected dict but got " + x)
  }
}

function set(x, key, value) {
  if (isDict(x)) {
    x[key] = value
    return x
  } else if (isNil(x)) {
    var y = {}
    y[key] = value
    return y
  } else {
    throw new TypeError("expected dict but got " + x)
  }
}

function rem(x, key) {
  if (isDict(x)) {
    delete x[key]
    return x
  } else if (isNil(x)) {
    return x
  } else {
    throw new TypeError("expected dict but got " + x)
  }
}

function nth(x, i) {
  if (i < 0) {
    throw new RangeError("second argument to nth must be greater than or equal to 0")
  }
  if (isArrayLike(x)) {
    if (i < x.length) {
      return x[i]
    } else {
      return nil
    }
  } else if (isNil(x)) {
    return nil
  } else {
    throw new TypeError("expected list but got " + x)
  }
}

function push(x, value) {
  if (isArrayLike(x)) {
    ;[].push.call(x, value)
    return x
  } else if (isNil(x)) {
    return [value]
  } else {
    throw new TypeError("expected list but got " + x)
  }
}

function join(x, y) {
  if (isArrayLike(y)) {
    for (var i = 0; i < y.length; ++i) {
      x = push(x, y[i])
    }
    return x
  } else {
    throw new TypeError("expected list but got " + y)
  }
}