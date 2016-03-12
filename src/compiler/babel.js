import * as $babel from "babel-core";
import * as $fs from "fs";
import * as $path from "path";

import { crash } from "../../util/error";
import { none, some } from "../../ffi/types";
import { async_unkillable } from "../../ffi/task";

import $module from "./babel-plugin-transform-es2015-modules-global";


const resolve = (base, path) => {
  path = $path.resolve(base, path);

  // TODO is this correct ?
  if ($path.extname(path) !== ".js") {
    path += ".js";
  }

  return $fs.realpathSync(path);
};


const read_file = (path) =>
  $fs.readFileSync(path, { "encoding": "utf8" });


const lookup_export = (exports, name, file) => {
  const exported = exports[name];

  if (exported == null) {
    return crash(new Error("could not find variable \"" + name + "\" in file \"" + file + "\""));

  } else {
    return exported;
  }
};


const compile1 = (state, path) => {
  if (state.modules[path] != null) {
    return state;

  } else {
    const dir = $path.dirname(path);

    const id = ++state.module_id;

    const module = {
      id,
      output: none,
      imports: [],
      exports: {}
    };

    state.modules[path] = module;

    const code = read_file(path);

    const output = $babel.transform(code, {
      "filename": path, // TODO is this correct ?
      "sourceMaps": true,
      "code": true,
      "babelrc": false,
      "ast": false,
      // sourceMapTarget
      // sourceFileName
      // sourceRoot
      // moduleIds
      // moduleId
      "presets": [
        "es2015-native-modules"
      ],
      "plugins": [
        [$module, {
          "loadFile": (file) => {
            const path = resolve(dir, file);

            module.imports["push"](path);

            state = compile1(state, path);

            return path;
          },

          "export": (from, to) => {
            // TODO handle conflicts
            module.exports[to] = {
              module: module.id,
              value: from
            };
          },

          "exportFrom": (file, from, to) => {
            const _module = state.modules[file];

            // TODO handle conflicts
            module.exports[to] = lookup_export(_module.exports, from, file);
          },

          "exportAllFrom": (file) => {
            const _module = state.modules[file];

            const exports = _module.exports;

            // TODO make this more efficient ?
            // TODO is this guaranteed to work in all situations ?
            // TODO handle conflicts
            for (let name in exports) {
              module.exports[name] = exports[name];
            }
          },

          "getAllExports": () => {
            const exports = module.exports;

            const output = {};

            // TODO make this more efficient ?
            for (let name in exports) {
              const exported = exports[name];
              output[name] = "$" + exported.module + "$" + exported.value;
            }

            return output;
          },

          "namespace": "$" + id + "$",

          "namespaceFrom": (file) => {
            const _module = state.modules[file];
            return "$" + _module.id + "$"
          },

          "localVariable": (name) =>
            "$" + id + "$" + name,

          "importedVariable": (name, file) => {
            const _module = state.modules[file];
            const exported = lookup_export(_module.exports, name, file);

            return "$" + exported.module + "$" + exported.value;
          }
        }]
      ]
    });

    module.output = some(output);

    return state;
  }
};

export const compile = (state, file) =>
  async_unkillable((success, error) =>
    success(compile1(state, $fs.realpathSync(file))));
