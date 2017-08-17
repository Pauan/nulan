import { Loc } from "../util/loc";
import { AST } from "../parse/ast";
import { Program } from "./ir";


interface VariableInfo {
  id: number;
  name: string | null;
  loc: Loc;
}

interface Scope {
  symbols: { [key: string]: number };
  defined: { [key: number]: boolean };
  used: { [key: string]: boolean };
}

interface CompileState {
  variableCounter: number;
  variables: { [key: number]: VariableInfo };
  scopes: Array<Scope>;
}


function compile1(state: CompileState, ast: AST, output: Array<Program>): void {

}


export function compile(ast: Array<AST>): Array<Program> {
  const state = {
    variableCounter: 0,
    variables: {},
    scopes: [{
      symbols: {},
      defined: {},
      used: {}
    }]
  };

  const output: Array<Program> = [];

  const length = ast.length;

  for (let i = 0; i < length; ++i) {
    compile1(state, ast[i], output);
  }

  return output;
}
