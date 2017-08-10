import { Order } from "./order";


export function order(a: string, b: string): Order {
  if (a === b) {
    return 0;

  } else if (a < b) {
    return -1;

  } else {
    return 1;
  }
}


// TODO better pretty-printing
export function pretty(a: string): string {
  return "\"" + a.replace(/["\\]/g, "\\$&") + "\"";
}


// TODO better pretty-printing
// TODO make this more efficient
export function prettyCharacter(a: string): string {
  return a.replace(/\n\r|\r\n|\n|\r/g, "<NEWLINE>").replace(/ /g, "<SPACE>");
}


export function plural(a: number, b: string): string {
  if (a === 1) {
    return a + b;

  } else {
    return a + b + "s";
  }
}


export function repeat(a: string, amount: number): string {
  if (amount <= 0) {
    return "";

  } else {
    // TODO make this more efficient ?
    return new Array(amount + 1).join(a);
  }
}


export function parseHex(hex: string): string {
  return String.fromCodePoint(parseInt(hex, 16));
}
