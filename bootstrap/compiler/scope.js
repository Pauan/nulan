import * as $ast from "../parser/type";


export const make_scope = () => {
  return {
    variables: {},
    boxes: {}
  };
};


export const push = (scopes) => {
  const x = make_scope();
  scopes.push(x);
  return x;
};


export const pop = (scopes) => {
  scopes.pop();
};


export const define = (scope, name, value) => {
  if (name in scope.variables) {
    throw new Error("variable already exists: " + name);
  }

  scope.variables[name] = value;
};


const define_box = (scope, id, value) => {
  if (id in scope.boxes) {
    throw new Error("variable already exists: #<box " + id + ">");
  }

  scope.boxes[id] = value;
};


export const define_last = (scopes, x, value) => {
  if (x.type === $ast.SYMBOL) {
    define(scopes[scopes.length - 1], x.value, value);

  } else if (x.type === $ast.VARIABLE) {
    define_box(scopes[scopes.length - 1], x.id, value);
  }
};


export const lookup = (module, x) => {
  const scopes = module.scopes;

  let i = scopes["length"];

  while (i--) {
    const scope = scopes[i];

    if (x in scope.variables) {
      return scope.variables[x];
    }
  }

  const imports = module.imports;

  for (let i = 0; i < imports.length; ++i) {
    const module = imports[i];

    if (x in module.exports) {
      return module.exports[x];
    }
  }

  return null;
};


export const lookup_variable = (module, x) => {
  switch (x.type) {
  case $ast.VARIABLE:
    return x;

  case $ast.SYMBOL:
    return lookup(module, x.value);

  default:
    return null;
  }
};
