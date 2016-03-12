import * as $babel from "./babel";
import { chain, reply, throw_error, make_thread_run } from "../../ffi/task";


console.log("COMPILING");

const state = {
  module_id: 0,
  modules: {}
};

make_thread_run(chain($babel.compile(state, "test/compiler/ffi/foo.js"), (x) => {
  console.log(state.modules["/home/pauan/Programming/nulan/nulan/test/compiler/ffi/qux.js"].output.a.code);
  //console.log(x.output.a.code);

  return reply(5);
}));

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
