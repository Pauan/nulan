import * as $array from "../../util/array";


export const SYMBOL  = 0;
export const INTEGER = 1;
export const NUMBER  = 2;
export const STRING  = 3;
export const BOX     = 4;

export const CALL    = 5;
export const LIST    = 6;
export const RECORD  = 7;

export const LAMBDA  = 8;

export const ASSIGN  = 9;
export const DOT     = 10;
export const TYPE    = 11;

export const BAR     = 12;
export const QUOTE   = 13;
export const UNQUOTE = 14;
export const SPLICE  = 15;


export const box = (module, id, name, loc) => {
  return { type: BOX, module, id, name, loc };
};

export const symbol = (value, loc) => {
  return { type: SYMBOL, value, loc };
};

export const integer = (value, loc) => {
  return { type: INTEGER, value, loc };
};

export const number = (value, loc) => {
  return { type: NUMBER, value, loc };
};

export const string = (value, loc) => {
  return { type: STRING, value, loc };
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

export const lambda = (parameters, body, loc) => {
  return { type: LAMBDA, parameters, body, loc };
};

export const bar = (value, loc) => {
  return { type: BAR, value, loc };
};

export const quote = (value, loc) => {
  return { type: QUOTE, value, loc };
};

export const unquote = (value, loc) => {
  return { type: UNQUOTE, value, loc };
};

export const splice = (value, loc) => {
  return { type: SPLICE, value, loc };
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
    return {
      type: x.type,
      left: f(x.left),
      right: f(x.right),
      loc: x.loc
    };

  case BAR:
  case QUOTE:
  case UNQUOTE:
  case SPLICE:
    return {
      type: x.type,
      value: f(x.value),
      loc: x.loc
    };

  // TODO is this correct ?
  default:
    return x;
  }
};
