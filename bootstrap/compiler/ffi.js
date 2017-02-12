import * as $babel from "babel-core";
import * as $fs from "fs";
import * as $path from "path";
import * as $source_map from "source-map";
import * as $serialize from "./serialize";

import { crash } from "../../builtin/ffi/crash";
import { make_module, lookup_export } from "./state";

import $module from "./babel-plugin-transform-es2015-modules-global";


export const ffi_dir = $path.join("builtin", "ffi");


const resolve1 = (base, path) => {
  path = $path.resolve(base, path);

  // TODO is this correct ?
  if ($path.extname(path) !== ".js") {
    path += ".js";
  }

  return $fs.realpathSync(path);
};

export const resolve = (base, path) => {
  const a = /^nulan:(.+)$/["exec"](path);

  if (a === null) {
    return resolve1(base, path);

  } else {
    // TODO find relative path
    return resolve1(ffi_dir, a[1]);
  }
};


const read_file = (path) =>
  $fs.readFileSync(path, { "encoding": "utf8" });


export const compile_ffi = (state, path) =>
  make_module(state, path, (module) => {
    const code = read_file(path);

    const dir = $path.dirname(path);

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
          "file": path,

          "loadFile": (file) => {
            const path = resolve(dir, file);

            module.imports["push"](path);

            compile_ffi(state, path);

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
            module.exports[to] = lookup_export(_module, from);
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
              output[name] = $serialize.variable(exported);
            }

            return output;
          },

          // TODO
          "namespace": "$" + module.id + "$",

          "namespaceFrom": (file) => {
            const _module = state.modules[file];
            // TODO
            return "$" + _module.id + "$";
          },

          "localVariable": (value) =>
            // TODO
            "$" + module.id + "$" + value,

          "importedVariable": (name, file) => {
            const _module = state.modules[file];
            const exported = lookup_export(_module, name);

            return $serialize.variable(exported);
          }
        }]
      ]
    });

    module.output = {
      $: 1,
      a: $source_map.SourceNode["fromStringWithSourceMap"](
        output["code"],
        new $source_map.SourceMapConsumer(output["map"]),
        // TODO
        $path.relative(state.root, dir)
      )
    };
  });
