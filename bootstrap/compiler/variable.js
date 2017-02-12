export const CONSTANT = 0;
export const TYPE = 1;
export const PROTOCOL = 2;
export const REWRITE_RULE = 3;


export const constant = (id, name) => {
  return { type: CONSTANT, id, name };
};

export const rewrite_rule = (id, name, rule) => {
  return { type: REWRITE_RULE, id, name, rule };
};
