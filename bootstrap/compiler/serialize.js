import { each_imports } from "./state";


const serialize_module = (seen, output, module) => {
  if (seen[module.path] == null) {
    seen[module.path] = true;

    output.push(module.output);
    output.push("\n");
  }
};


export const serialize = (state, module) => {
  const seen = {};

  const output = ["(function(){\"use strict\";\n"];

  each_imports(state, module, (module) => {
    serialize_module(seen, output, module);
  });

  serialize_module(seen, output, module);

  output.push("})();\n");

  return output.join("");
};
