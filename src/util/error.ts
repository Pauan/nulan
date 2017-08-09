import * as $chalk from "chalk";
import * as $string from "./string";
import * as $node from "./node";
import * as $loc from "./loc";


export class NulanError {
  public loc: $loc.Loc;
  public message: string;

  constructor(loc: $loc.Loc, message: string) {
    this.loc = loc;
    this.message = message;
  }

  toString() {
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
