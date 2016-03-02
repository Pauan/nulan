import * as $ast from "../parser/type";
import * as $variable from "./variable";
import * as $state from "./state";


const compile1 = (state, statement) => {

};

export const compile = (statements, filename) => {
  const state = make_state();

  const id = ++state.module_id;

  const module = make_module(id);

  state.modules[id] = module;


  for (let i = 0; i < statements["length"]; ++i) {
    const { state, input } = compile1(state, statements[i]);

  }


  return state;
};
