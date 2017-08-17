import * as $chalk from "chalk";
import * as $string from "./string";
import * as $node from "./node";
import * as $loc from "./loc";


export function assertExists<A>(v: A | undefined | null): A {
  if (v == null) {
    throw new Error("Value cannot be null or undefined");

  } else {
    return v;
  }
}


export function assert(test: boolean): void {
  if (!test) {
    throw new Error("Assertion failed");
  }
}


export class NulanError extends Error {
  public name: string = "NulanError";
  public loc: $loc.Loc;

  constructor(loc: $loc.Loc, message: string) {
    super(message);

    // TODO very hacky, remove later
    Object.setPrototypeOf(this, NulanError.prototype);

    this.loc = loc;
  }

  pretty() {
    return $chalk.bold(
      $chalk.red.dim("Error in " + $loc.pretty(this.loc) + ":") +
      $node.EOL + "  " + this.message.replace(/\n/g, $node.EOL + "  ")
    );
  }
}


export function warning(loc: $loc.Loc, message: string): void {
  console.warn($chalk.bold(
    $chalk.magenta.dim("Warning in " + $loc.pretty(loc) + ":") +
    $node.EOL + "  " + message.replace(/\n/g, $node.EOL + "  ")
  ));
}
