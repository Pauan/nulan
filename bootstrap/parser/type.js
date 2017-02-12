import * as $list from "../../builtin/ffi/list";


// Literals
export const SYMBOL   = 0;
export const INTEGER  = 1;
export const NUMBER   = 2;
export const TEXT     = 3;
export const VARIABLE = 4;
export const WILDCARD = 5;

// Lists
export const CALL     = 6;
export const LIST     = 7;
export const RECORD   = 8;

// Lambda
export const LAMBDA   = 9;

// Infix
export const ASSIGN   = 10;
export const DOT      = 11;
export const TYPE     = 12;
export const BAR      = 13;

// Prefix
export const QUOTE    = 14;
export const UNQUOTE  = 15;
export const SPLICE   = 16;


export const symbol = (value, loc) => {
  return { type: SYMBOL, value, loc };
};

let variable_id = 0;

export const variable = ({ macro }, loc) => {
  return { type: VARIABLE, id: ++variable_id, macro, loc };
};

export const integer = (value, loc) => {
  return { type: INTEGER, value, loc };
};

export const number = (value, loc) => {
  return { type: NUMBER, value, loc };
};

export const text = (value, loc) => {
  return { type: TEXT, value, loc };
};

export const wildcard = (loc) => {
  return { type: WILDCARD, loc };
};

export const call = (value, loc) => {
  return { type: CALL, value, loc };
};

export const list = (value, loc) => {
  return { type: LIST, value, loc };
};

export const record = (value, loc) => {
  return { type: RECORD, value, loc };
};

export const assign = (left, right, loc) => {
  return { type: ASSIGN, left, right, loc };
};

export const dot = (left, right, loc) => {
  return { type: DOT, left, right, loc };
};

export const type = (left, right, loc) => {
  return { type: TYPE, left, right, loc };
};

export const bar = (left, right, loc) => {
  return { type: BAR, left, right, loc };
};

export const lambda = (parameters, body, loc) => {
  return { type: LAMBDA, parameters, body, loc };
};

export const quote = (right, loc) => {
  return { type: QUOTE, right, loc };
};

export const unquote = (right, loc) => {
  return { type: UNQUOTE, right, loc };
};

export const splice = (right, loc) => {
  return { type: SPLICE, right, loc };
};


export const loc = (filename, lines, start, end) => {
  return { filename, lines, start, end };
};

export const concat_loc = (left, right) =>
  loc(left.filename, left.lines, left.start, right.end);


export const map = (x, f) => {
  switch (x.type) {
  case CALL:
  case LIST:
  case RECORD:
    return {
      type: x.type,
      value: $list.transform(x.value, f),
      loc: x.loc
    };

  case LAMBDA:
    return {
      type: x.type,
      parameters: $list.transform(x.parameters, f),
      body: f(x.body),
      loc: x.loc
    };

  case ASSIGN:
  case DOT:
  case TYPE:
  case BAR:
    return {
      type: x.type,
      left: f(x.left),
      right: f(x.right),
      loc: x.loc
    };

  case QUOTE:
  case UNQUOTE:
  case SPLICE:
    return {
      type: x.type,
      right: f(x.right),
      loc: x.loc
    };

  // TODO is this correct ?
  default:
    return x;
  }
};


export const print = (x) => {
  switch (x.type) {
  case SYMBOL:
  case INTEGER:
  case NUMBER:
    return "" + x.value;

  case VARIABLE:
    return "#<box " + x.id + ">";

  case WILDCARD:
    return "_";

  // TODO fix this
  case TEXT:
    return x.values;

  case CALL:
    return "(" + x.value.map(print).join(" ") + ")";

  case LIST:
    return "[ " + x.value.map(print).join(" ") + " ]";

  case RECORD:
    return "{ " + x.value.map(print).join(" ") + " }";

  case LAMBDA:
    return "(-> " + [...x.parameters, x.body].map(print).join(" ") + ")";

  case ASSIGN:
    return print(x.left) + " <= " + print(x.right);

  case DOT:
    return print(x.left) + "." + print(x.right);

  case TYPE:
    return print(x.left) + " :: " + print(x.right);

  case BAR:
    return print(x.left) + " : " + print(x.right);

  case QUOTE:
    return "&" + print(x.right);

  case UNQUOTE:
    return "~" + print(x.right);

  case SPLICE:
    return "@" + print(x.right);
  }
};
