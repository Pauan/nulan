import * as $path from "path";
import * as $fs from "../../util/fs";
import { lines } from "../../util/string";
import { parse } from "../parser/parse";
import { tokenize } from "../parser/tokenize";
import { make_state, make_module } from "./state";
import { serialize } from "./serialize";
import { compile_nulan } from "./compile-nulan";


export const compile_file = async (state, file_name) => {
  const module = make_module(state, file_name);

  if (module.output === null) {
    const file = await $fs.read_file(file_name);

    module.output = await compile_nulan(state, module, parse(tokenize(lines(file), file_name)));
  }

  return module;
};


export const compile = async ({ root, input, output, source_map }) => {
  const state = make_state({ root });

  const top = await compile_file(state, input);

  console.log(serialize(state, top));
};



/*import * as $fs from "fs";
import * as $path from "path";
import * as $source_map from "source-map";
import { ffi_dir, compile_ffi, resolve } from "./ffi";
import * as $serialize from "./serialize";
import { chain, reply, throw_error, make_thread_run, async_unkillable } from "../../builtin/ffi/task";
import { make_state, lookup_export } from "./state";


export const serialize_module = (state, module) => {
  const module_task = compile_ffi(state, resolve(state.root, "nulan:task"));

  // TODO a bit hacky
  module.imports["push"](module_task.path);

  const end =
    ($serialize.variable(lookup_export(module_task, "make_thread_run")) + "(" +
     $serialize.variable(lookup_export(module, state.main)) + ");\n");

  return $serialize.serialize(state, module, end);
};


export const compile_file = (a) =>
  async_unkillable((success, error) => {
    const input = a.a;
    const main = a.b;
    const watch = a.c;
    const target = a.d;
    const output = a.e;
    const source_map = a.f;

    const state = make_state(input, main, watch, target, output, source_map);

    const module = compile_ffi(state, $fs.realpathSync(state.input));

    const x = serialize_module(state, module);

    $fs.writeFileSync(state.output, x.code);

    if (state.source_map.$ === 1) {
      $fs.writeFileSync(state.source_map.a, x.map);
    }

    return success(null);
  });


const start = Date["now"]();

const state = make_state("test/compiler/ffi/foo.js", "main", 0, 0, "tmp/source.js", { $: 1, a: "tmp/source.js.map" });

const module = compile_ffi(state, $fs.realpathSync(state.input));

const x = $serialize.serialize(state, module, "");

$fs.writeFileSync(state.output, x.code);
$fs.writeFileSync(state.source_map.a, x.map);*/

/*make_thread_run(chain(compile_file({
  a: "test/compiler/ffi/foo.js",
  b: "main",
  c: 0,
  d: 0,
  e: "tmp/source.js",
  f: { $: 1, a: "tmp/source.js.map" }
}), (a) => {*/
  //console["log"]("took " + (Date["now"]() - start) + "ms");

/*  return reply(5);
}));*/


/*new $source_map.SourceMapConsumer();

console.log(state.modules["/home/pauan/Programming/nulan/nulan/test/compiler/ffi/qux.js"].output.a.code);
console.log(state.modules["/home/pauan/Programming/nulan/nulan/test/compiler/ffi/bar.js"].output.a.code);
console.log(state.modules["/home/pauan/Programming/nulan/nulan/test/compiler/ffi/foo.js"].output.a.code);
console.log("console.log($1$foo1);");*/
//console.log(x.output.a.code);

/*import * as $ast from "../parser/type";
import * as $variable from "./variable";
import * as $state from "./state";
import * as $list from "../../ffi/list";
import * as $fs from "../../ffi/node/fs";


const escape_name = (name) =>
  filename["replace"](/(?:^[0-9])|[^a-zA-Z0-9]/g, (s) =>
    "_" + s["charCodeAt"](0) + "_");

// TODO code duplication
const escape_filename = (filename) =>
  filename["replace"](/(?:^[0-9])|[^a-zA-Z0-9]/g, (s) => {
    if (s === "/") {
      return "$";
    } else {
      return "_" + s["charCodeAt"](0) + "_";
    }
  });


const compile_module1 = (state, module, statement) => {

};


const compile_statements = (state, module, statements) =>
  $list.reduce_left(reply(state), statements, (state, statement) =>
    chain(state, (state) =>
      compile_module1(state, module, statement)));


const make_state = (target) => {
  return {
    target,
    module_id: 0,
    modules: {}
  };
};

const make_nulan_module = (state) => {
  const id = ++state.module_id;

  return {
    $: 0,
    id,
    variable_id: 0,
    variables: {},
    exports: {},
    imports: [],
    scopes: [{}],
    javascript: ""
  };
};


const compile_main = (filename, file, target) => {
  const state = make_state(target);

  const module = make_nulan_module(state);

  state.modules[filename] = module;

  const statements = parse(tokenize(lines(file), filename));

  return chain(compile_statements(state, module, statements), (_) =>
    serialize_to_javascript(module));
};


export const compile = (info) => {
  const filename = info.a;
  const main = info.b;
  const target = info.c;

  return chain($fs.read_file(filename), (maybe) => {
    if (maybe.$ === 0) {
      return throw_error(new Error("could not find file " + filename));

    } else {
      const file = maybe.a;
      return compile_main(filename, file, target);
    }
  });
};


make_thread_run(compile({ a: "notes/Current state", b: "main" }));
*/
