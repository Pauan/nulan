import * as $esotope from "esotope";


export const generate = (ast) => {
  console.log(require("util").inspect(ast, { depth: null, colors: true }));
  return $esotope.generate(ast);
};
