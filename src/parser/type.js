import * as $array from "../../util/array";


// Literals
export const SYMBOL   = 0;
export const INTEGER  = 1;
export const NUMBER   = 2;
export const TEXT     = 3;
export const VARIABLE = 4;

// Lists
export const CALL     = 5;
export const LIST     = 6;
export const RECORD   = 7;

// Lambda
export const LAMBDA   = 8;

// Infix
export const ASSIGN   = 9;
export const DOT      = 10;
export const TYPE     = 11;
export const BAR      = 12;

// Prefix
export const QUOTE    = 13;
export const UNQUOTE  = 14;
export const SPLICE   = 15;


export const symbol = (value, loc) => {
  return { type: SYMBOL, value, loc };
};

export const variable = (module, id, loc) => {
  return { type: VARIABLE, module, id, loc };
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
      value: $array.map(x.value, f),
      loc: x.loc
    };

  case LAMBDA:
    return {
      type: x.type,
      parameters: $array.map(x.parameters, f),
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