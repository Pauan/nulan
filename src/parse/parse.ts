import { Token } from "./tokenize";
import { Loc } from "./loc";


export type AST
  = Token
  | { type: "float", value: string, loc: Loc }
  | { type: "wildcard", loc: Loc }
  | { type: "variable", id: number, loc: Loc }
  | { type: "lambda", args: Array<AST>, body: AST, loc: Loc }
  | { type: "call", args: Array<AST>, loc: Loc }
  | { type: "array", args: Array<AST>, loc: Loc }
  | { type: "record", args: Array<AST>, loc: Loc }
  | { type: "bar", value: AST, loc: Loc }
  | { type: "quote", value: AST, loc: Loc }
  | { type: "unquote", value: AST, loc: Loc }
  | { type: "splice", value: AST, loc: Loc }
  | { type: "dot", left: AST, right: AST, loc: Loc }
  | { type: "match", left: AST, right: AST, loc: Loc }
  | { type: "assign", left: AST, right: AST, loc: Loc }
  | { type: "type", left: AST, right: AST, loc: Loc };

type Associativity = "left" | "right";

type Priority = number;

type TokenInfo = {
  priority: Priority,
  parse: (left: Array<AST>, token: Token, priority: Priority) => Array<AST>
};

type ParserState = {
  input: Array<Token>
};


function peek(state: ParserState): Token | null {
  if (state.input.length) {
    return state.input[0];

  } else {
    return null;
  }
}

function next(state: ParserState): void {
  if (state.input.length) {
    state.input.shift();
  }
}

function parseBrackets(state: ParserState, make: (args: Array<AST>) => AST, end: string): AST {
  const token = peek(state);

  if (token == null) {
    throw new Error("missing ending " + end);

  } else {
  }
}
