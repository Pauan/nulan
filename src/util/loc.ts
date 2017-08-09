import * as $chalk from "chalk";
import * as $string from "./string";


export interface Position {
  index: number,
  line: number,
  column: number
}

export interface Loc {
  filename: string,
  start: Position,
  end: Position
}


export function concat(left: Loc, right: Loc): Loc {
  return {
    filename: right.filename,
    start: left.start,
    end: right.end
  };
}


export function pretty(loc: Loc): string {
  return "file " +
    $chalk.green($string.pretty(loc.filename)) +
    " at position " +
    $chalk.green((loc.start.line + 1) + ":" + (loc.start.column + 1)) +
    " to " +
    $chalk.green((loc.end.line + 1) + ":" + (loc.end.column + 1));
}


export function loc(filename: string, start: Position, end: Position): Loc {
  return { filename, start, end };
}

export function position(index: number, line: number, column: number): Position {
  return { index, line, column };
}
