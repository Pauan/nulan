import * as $ast from "../parser/type";


export const make_module = (id) => {
  return {
    id: id,
    variable_id: 0,
    variables: {},
    statements: []
  };
};

export const make_state = () => {
  return {
    module_id: 0,
    modules: {},
    scopes: []
  };
};

export const define_new_variable = (module, variable) => {
  module.variables[variable.id] = variable;
  return module;
};

const define_new_symbol = (module, symbol) => {
  if (symbol.type === $ast.SYMBOL) {

  } else {
    error(symbol, "cannot define a non-symbol");
  }
};


export const lookup_symbol = (state, symbol) => {
  const scopes = state.scopes;
  const x = symbol.value;

  let i = scopes["length"];

  while (i--) {
    const scope = scopes[i];

    if (x in scope) {
      return scope[x];
    }
  }

  return null;
};

export const lookup_box = (state, x) => {
  switch (x.type) {
  case $ast.BOX:
    return x;

  case $ast.SYMBOL:
    return lookup_symbol(state, x);

  default:
    return null;
  }
};

export const lookup_variable = (state, box) => {
  const module = state.modules[box.module];
  return module.variables[box.id];
};
