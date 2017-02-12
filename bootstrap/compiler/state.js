import { crash } from "../../builtin/ffi/crash";
import { make_scope } from "./scope";
import * as $path from "path";
import * as $ast from "../parser/type";
import { default_scope } from "./default-scope";


export const make_state = ({ root }) => {
  return {
    module_id: 0,
    modules: {},
    root
  };
};


export const make_module = (state, path) => {
  if (state.modules[path] != null) {
    return state.modules[path];

  } else {
    const id = ++state.module_id;

    const module = {
      id,
      path,
      output: null,
      imports: [],
      exports: {},
      scopes: [default_scope, make_scope()]
    };

    state.modules[path] = module;

    return module;
  }
};


export const each_imports = (state, module, f) => {
  const imports = module.imports;
  const length = imports["length"];

  for (let i = 0; i < length; ++i) {
    const module = imports[i];

    each_imports(state, module, f);

    f(module);
  }
};


export const define_export = (module, key, value) => {
  if (module.exports[key] == null) {
    module.exports[key] = value;

  } else {
    throw new Error("Variable is already exported: " + key);
  }
};


export const lookup_export = (module, name) => {
  const exported = module.exports[name];

  if (exported == null) {
    return crash(new Error("could not find variable \"" + name + "\" in file \"" + module.path + "\""));

  } else {
    return exported;
  }
};
