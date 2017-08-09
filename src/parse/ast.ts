import { Loc } from "../util/loc";


export type Token
  = { type: "integer", value: string, loc: Loc }
  | { type: "string", value: string, loc: Loc }
  | { type: "symbol", value: string, loc: Loc };


export function integer(value: string, loc: Loc): Token {
  return { type: "integer", value, loc };
}

export function string(value: string, loc: Loc): Token {
  return { type: "string", value, loc };
}

export function symbol(value: string, loc: Loc): Token {
  return { type: "symbol", value, loc };
}
