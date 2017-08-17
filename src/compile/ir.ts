import { Loc } from "../util/loc";


type UnaryOperator = "-" | "+" | "!" | "typeof";

type BinaryOperator = "===" | "!==" | "<" | "<=" | ">" | ">=" | ">>>" | "+" | "-" | "*" | "/" | "%" | "|" | "||" | "&&";


export interface String {
  type: "string";
  value: string;
  loc: Loc
}

export interface Variable {
  type: "variable";
  id: number;
  name: string | null;
  loc: Loc;
}

export interface Property {
  key: String;
  value: Pure;
  loc: Loc;
}


export type Program
  = { type: "define", variable: Variable, expression: Pure, loc: Loc }
  | { type: "main", fn: Variable, arg: Variable, loc: Loc };


export type Statement
  = { type: "define-ssa", variable: Variable, expression: Pure, loc: Loc }
  | { type: "break", loc: Loc }
  | { type: "return", value: Pure, loc: Loc }
  | { type: "if", test: Pure, then: Array<Statement>, else: Array<Statement>, loc: Loc }
  | { type: "throw", value: Pure, loc: Loc }
  | { type: "loop", body: Array<Statement>, loc: Loc };


export type Pure
  = String
  | Variable
  | { type: "number", value: number, loc: Loc }
  | { type: "boolean", value: boolean, loc: Loc }
  | { type: "null", loc: Loc }
  | { type: "regexp", pattern: string, flags: string, loc: Loc }
  | { type: "call", fn: Pure, args: Array<Pure>, loc: Loc }
  | { type: "array", args: Array<Pure>, loc: Loc }
  | { type: "object", args: Array<Property>, loc: Loc }
  | { type: "object-lookup", object: Pure, key: Pure, loc: Loc }
  | { type: "function", args: Array<Variable>, body: Array<Statement>, loc: Loc }
  | { type: "unary", operator: UnaryOperator, right: Pure, loc: Loc }
  | { type: "binary", operator: BinaryOperator, left: Pure, right: Pure, loc: Loc }
  | { type: "ternary-if", test: Pure, then: Pure, else: Pure, loc: Loc }
  | { type: "sequence", args: Array<Pure>, loc: Loc };


export function define(variable: Variable, expression: Pure, loc: Loc): Program {
  return { type: "define", variable, expression, loc };
}

export function variable(id: number, name: string | null, loc: Loc): Variable {
  return { type: "variable", id, name, loc };
}

export function number(value: number, loc: Loc): Pure {
  return { type: "number", value, loc };
}
