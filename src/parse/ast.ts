import { Loc } from "../util/loc";
import * as $string from "../util/string";


export type Token
  = { type: "integer", value: string, loc: Loc }
  | { type: "string", value: string, loc: Loc }
  | { type: "symbol", value: string, loc: Loc }
  | { type: "tag", value: string, loc: Loc };

export type AST
  = Token
  | { type: "float", value: string, loc: Loc }
  | { type: "wildcard", loc: Loc }
  | { type: "variable", id: number, name: string | null, loc: Loc }
  | { type: "call", args: Array<AST>, loc: Loc }
  | { type: "array", args: Array<AST>, loc: Loc }
  | { type: "record", args: Array<AST>, loc: Loc }
  | { type: "implicit", value: AST, loc: Loc }
  | { type: "quote", value: AST, loc: Loc }
  | { type: "unquote", value: AST, loc: Loc }
  | { type: "splice", value: AST, loc: Loc }
  | { type: "dot", left: AST, right: AST, loc: Loc }
  | { type: "match", args: Array<AST>, body: AST, loc: Loc }
  | { type: "assign", left: AST, right: AST, loc: Loc }
  | { type: "type", left: AST, right: AST, loc: Loc };


export function pretty(a: AST): string {
  switch (a.type) {
  case "variable":
    if (a.name == null) {
      return "#(var " + a.id + ")";
    } else {
      return "#(var " + a.id + " " + a.name + ")";
    }
  case "integer":
  case "symbol":
  case "float":
    return a.value;
  case "string":
    return $string.pretty(a.value);
  case "tag":
    return "*" + a.value;
  case "wildcard":
    return "_";
  case "call":
    if (a.args.length === 0) {
      return "()";
    } else {
      return "( " + a.args.map(pretty).join(" ") + " )";
    }
  case "array":
    if (a.args.length === 0) {
      return "[]";
    } else {
      return "[ " + a.args.map(pretty).join(" ") + " ]";
    }
  case "record":
    if (a.args.length === 0) {
      return "{}";
    } else {
      return "{ " + a.args.map(pretty).join(" ") + " }";
    }
  case "quote":
    return "(& " + pretty(a.value) + ")";
  case "unquote":
    return "(~ " + pretty(a.value) + ")";
  case "splice":
    return "(@ " + pretty(a.value) + ")";
  case "implicit":
    return "(% " + pretty(a.value) + ")";
  case "dot":
    return "(" + pretty(a.left) + " . " + pretty(a.right) + ")";
  case "match":
    if (a.args.length === 0) {
      return "(| : " + pretty(a.body) + ")";
    } else {
      return "(| " + a.args.map(pretty).join(" ") + " : " + pretty(a.body) + ")";
    }
  case "assign":
    return "(" + pretty(a.left) + " <= " + pretty(a.right) + ")";
  case "type":
    return "(" + pretty(a.left) + " :: " + pretty(a.right) + ")";
  }
}


export function variable(id: number, name: string | null, loc: Loc): AST {
  return { type: "variable", id, name, loc };
}

export function wildcard(loc: Loc): AST {
  return { type: "wildcard", loc };
}

export function tag(value: string, loc: Loc): Token {
  return { type: "tag", value, loc };
}

export function integer(value: string, loc: Loc): Token {
  return { type: "integer", value, loc };
}

export function string(value: string, loc: Loc): Token {
  return { type: "string", value, loc };
}

export function symbol(value: string, loc: Loc): Token {
  return { type: "symbol", value, loc };
}

export function float(value: string, loc: Loc): AST {
  return { type: "float", value, loc };
}

export function call(args: Array<AST>, loc: Loc): AST {
  return { type: "call", args, loc };
}

export function array(args: Array<AST>, loc: Loc): AST {
  return { type: "array", args, loc };
}

export function record(args: Array<AST>, loc: Loc): AST {
  return { type: "record", args, loc };
}

export function quote(value: AST, loc: Loc): AST {
  return { type: "quote", value, loc };
}

export function unquote(value: AST, loc: Loc): AST {
  return { type: "unquote", value, loc };
}

export function splice(value: AST, loc: Loc): AST {
  return { type: "splice", value, loc };
}

export function implicit(value: AST, loc: Loc): AST {
  return { type: "implicit", value, loc };
}

export function match(args: Array<AST>, body: AST, loc: Loc): AST {
  return { type: "match", args, body, loc };
}

export function assign(left: AST, right: AST, loc: Loc): AST {
  return { type: "assign", left, right, loc };
}

export function type(left: AST, right: AST, loc: Loc): AST {
  return { type: "type", left, right, loc };
}

export function dot(left: AST, right: AST, loc: Loc): AST {
  return { type: "dot", left, right, loc };
}
