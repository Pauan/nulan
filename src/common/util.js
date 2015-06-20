export function plural(i, s) {
  if (i === 1) {
    return i + " " + s;
  } else {
    return i + " " + s + "s";
  }
}
