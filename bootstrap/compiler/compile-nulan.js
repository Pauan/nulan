import { generate } from "./generate";
import { map } from "../../util/promise";
import { lookup_variable, lookup, push, pop, define_last } from "./scope";
import * as $ast from "../parser/type";
import { mangle_name } from "./mangle";


export const compile_nulan_expression = async (state, module, ast) => {
  if (ast.type === $ast.CALL) {
    const first = lookup_variable(module, ast.value[0]);

    if (first !== null && first.macro) {
      return await first.macro(state, module, ast);

    } else {
      if (ast.value.length === 1) {
        return await compile_nulan_expression(state, module, ast.value[0]);

      } else {
        return {
          type: "CallExpression",
          callee: await compile_nulan_expression(state, module, ast.value[0]),
          arguments: await map(ast.value.slice(1), (x) =>
            compile_nulan_expression(state, module, x))
        };
      }
    }

  } else if (ast.type === $ast.LAMBDA) {
    push(module.scopes);

    const args = await map(ast.parameters, async (x) => {
      define_last(module.scopes, x, $ast.variable({}));
      return await compile_nulan_expression(state, module, x);
    });

    const body = await compile_nulan_expression(state, module, ast.body);

    pop(module.scopes);

    return {
      type: "FunctionExpression",
      id: null,
      params: args,
      body: {
        type: "BlockStatement",
        body: [{
          type: "ReturnStatement",
          argument: body
        }]
      }
    };

  } else if (ast.type === $ast.SYMBOL) {
    console.log(ast.value, lookup(module, ast.value));
    return await compile_nulan_expression(state, module, lookup(module, ast.value));

  } else if (ast.type === $ast.VARIABLE) {
    if (ast.macro) {
      return await ast.macro(state, module, ast);

    } else {
      return {
        type: "Identifier",
        name: "$" + ast.id
      };
    }

  } else if (ast.type === $ast.TYPE) {
    return await compile_nulan_expression(state, module, ast.left);

  } else if (ast.type === $ast.INTEGER || ast.type === $ast.NUMBER || ast.type === $ast.TEXT) {
    return {
      type: "Literal",
      value: ast.value
    };
  }
};

export const compile_nulan_statement = async (state, module, ast) => {
  if (ast.type === $ast.CALL) {
    const first = lookup_variable(module, ast.value[0]);

    if (first !== null && first.macro) {
      return await first.macro(state, module, ast);

    } else {
      return {
        type: "ExpressionStatement",
        expression: await compile_nulan_expression(state, module, ast)
      };
    }
  }
};

export const compile_nulan = async (state, module, a) => {
  return generate({
    type: "Program",
    body: await map(a, (x) => compile_nulan_statement(state, module, x))
  });
};
