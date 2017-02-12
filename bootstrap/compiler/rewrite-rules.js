import * as $ast from "../parser/type";
import * as $variable from "./variable";
import * as $state from "./state";


export const rewrite_rules = (state, input) => {
  if (input.type === $ast.CALL &&
      input.value["length"] > 0) {

    const box = $state.lookup_box(state, input.value[0]);

    if (box !== null) {
      const variable = $state.lookup_variable(state, box);

      if (variable.type === $variable.REWRITE_RULE) {
        return variable.rule(state, input);
      }
    }
  }

  return $ast.map(input, (x) => rewrite_rules(state, x));
};
