function toArray(f1) {
  var x = force(f1)
  if (isNull(x)) {
    return []
  } else if (x instanceof Cons) {
    var r = []
    do {
      r.push(car(x))
      x = cdr(x)
    } while (!isNull(x))
    return r
  } else if (isArray(x)) {
    return x
  } else {
    throw new TypeError("expected cons but got " + x)
  }
}