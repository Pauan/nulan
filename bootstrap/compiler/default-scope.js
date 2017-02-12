import { make_scope, define, define_last, push, pop } from "./scope";
import * as $ast from "../parser/type";
import { compile_nulan_expression } from "./compile-nulan";
import { map, reduce_right, each } from "../../util/promise";
import { zip, flatten } from "../../util/array";
import { compile_file } from "./compile";
import * as $path from "path";
import { define_export } from "./state";


export const default_scope = make_scope();


const generate_property = (i) =>
  String.fromCharCode(i + 97);


define(default_scope, "TYPE", $ast.variable({
  macro: async (state, module, ast) => {
    const body = ast.value.slice(2);

    body.forEach((x, i) => {
      if (x.type === $ast.CALL) {
        const box = $ast.variable({
          macro: async (state, module, ast) => {
            const values =
              (ast.value[1].type === $ast.RECORD
                ? await map(ast.value[1].value, async (x, i) => {
                    return {
                      type: "Property",
                      kind: "init",
                      key: {
                        type: "Identifier",
                        name: generate_property(i)
                      },
                      value: await compile_nulan_expression(state, module, x.right)
                    };
                  })
                : await map(ast.value.slice(1), async (x, i) => {
                    return {
                      type: "Property",
                      kind: "init",
                      key: {
                        type: "Identifier",
                        name: generate_property(i)
                      },
                      value: await compile_nulan_expression(state, module, x)
                    };
                  }));

            return {
              type: "ObjectExpression",
              properties: [
                {
                  type: "Property",
                  kind: "init",
                  key: {
                    type: "Identifier",
                    name: "$"
                  },
                  value: {
                    type: "Literal",
                    value: x.value[0].value
                  }
                },
                ...values
              ]
            };
          }
        });

        define_export(module, x.value[0].value, box);
        define_last(module.scopes, x.value[0], box);

      } else if (x.type === $ast.SYMBOL) {
        const box = $ast.variable({
          macro: async (state, module, ast) => {
            return {
              type: "ObjectExpression",
              properties: [{
                type: "Property",
                kind: "init",
                key: {
                  type: "Identifier",
                  name: "$"
                },
                value: {
                  type: "Literal",
                  value: x.value
                }
              }]
            };
          }
        });

        define_export(module, x.value, box);
        define_last(module.scopes, x, box);
      }
    });

    return {
      type: "EmptyStatement"
    };
  }
}));


define(default_scope, "CONSTANT", $ast.variable({
  macro: async (state, module, ast) => {
    const type = ast.value[1];

    const box = $ast.variable({});

    define_export(module, type.left.value, box);
    define_last(module.scopes, type.left, box);

    return {
      type: "VariableDeclaration",
      declarations: [{
        type: "VariableDeclarator",
        id: await compile_nulan_expression(state, module, box),
        init: await compile_nulan_expression(state, module, ast.value[2])
      }],
      kind: "const"
    };
  }
}));


const pattern_does_match = (x, expr) => {
  if (x.type === $ast.CALL) {
    return [
      {
        type: "BinaryExpression",
        operator: "===",
        left: {
          type: "MemberExpression",
          object: expr,
          property: {
            type: "Identifier",
            name: "$"
          },
          computed: false
        },
        right: {
          type: "Literal",
          value: x.value[0].value
        }
      },
      ...flatten(x.value.slice(1).map((x, i) =>
        pattern_does_match(x, {
          type: "MemberExpression",
          object: expr,
          property: {
            type: "Identifier",
            name: generate_property(i)
          },
          computed: false
        })))
    ];

  } else if (x.type === $ast.SYMBOL && x.value[0] === "*") {
    return [{
      type: "BinaryExpression",
      operator: "===",
      left: {
        type: "MemberExpression",
        object: expr,
        property: {
          type: "Identifier",
          name: "$"
        },
        computed: false
      },
      right: {
        type: "Literal",
        value: x.value
      }
    }];

  } else if (x.type === $ast.WILDCARD) {
    return [{
      type: "Literal",
      value: true
    }];

  } else {
    return [{
      type: "Literal",
      value: true
    }];
  }
};


const pattern_match_vars = async (state, module, x, expr) => {
  if (x.type === $ast.CALL) {
    return flatten(await map(x.value.slice(1), async (x, i) =>
      await pattern_match_vars(state, module, x, {
        type: "MemberExpression",
        object: expr,
        property: {
          type: "Identifier",
          name: generate_property(i)
        },
        computed: false
      })));

  } else if (x.type === $ast.SYMBOL && x.value[0] === "*") {
    return [];

  } else if (x.type === $ast.WILDCARD) {
    return [];

  } else {
    define_last(module.scopes, x, $ast.variable({}));

    return [{
      type: "VariableDeclaration",
      declarations: [{
        type: "VariableDeclarator",
        id: await compile_nulan_expression(state, module, x),
        init: expr
      }],
      kind: "const"
    }];
  }
};


const wrap_function = (expr) => {
  return {
    type: "CallExpression",
    callee: {
      type: "FunctionExpression",
      id: null,
      params: [],
      body: {
        type: "BlockStatement",
        body: [expr]
      }
    },
    arguments: []
  }
};


const wrap_and = (a) => {
  return a.reduce((x, y) => {
    return {
      type: "LogicalExpression",
      operator: "&&",
      left: x,
      right: y
    };
  });
};


define(default_scope, "MATCHES", $ast.variable({
  macro: async (state, module, ast) => {
    const expr = await map(ast.value[1].value, async (x) =>
      await compile_nulan_expression(state, module, x));

    const body = ast.value.slice(2);

    return wrap_function(await reduce_right(body, {
      type: "ThrowStatement",
      argument: {
        type: "NewExpression",
        callee: {
          type: "Identifier",
          name: "Error"
        },
        arguments: [{
          type: "Literal",
          value: "Pattern match failed"
        }]
      }
    }, async (x, y) => {
      push(module.scopes);

      const lefts = zip([x.left.value, expr]);

      const vars = flatten(await map(lefts, async ([pat, expr]) =>
        await pattern_match_vars(state, module, pat, expr)));

      const body = await compile_nulan_expression(state, module, x.right);

      pop(module.scopes);

      return {
        type: "IfStatement",
        test: wrap_and(flatten(lefts.map(([pat, expr]) => pattern_does_match(pat, expr)))),
        consequent: {
          type: "BlockStatement",
          body: [
            ...vars,
            {
              type: "ReturnStatement",
              argument: body
            }
          ]
        },
        alternate: y
      };
    }));
  }
}));


define(default_scope, "FUNCTION", $ast.variable({
  macro: async (state, module, ast) => {
    const body = ast.value.slice(2);

    const args = body[0].left.value.map((x) =>
      $ast.variable({}));

    return await compile_nulan_expression(state, module, $ast.call([
      $ast.symbol("CONSTANT"),

      ast.value[1],

      $ast.lambda(args, $ast.call([
        $ast.symbol("MATCHES"),

        $ast.list(args),

        ...body
      ]))
    ]));
  }
}));


define(default_scope, "USE", $ast.variable({
  macro: async (state, module, ast) => {
    const name = ast.value[1].value;

    const x = /^local:(.+)$/.exec(name)[1];

    const inner_module = await compile_file(state, $path.join(state.root, x));

    module.imports.push(inner_module);

    return {
      type: "EmptyStatement"
    };
  }
}));
