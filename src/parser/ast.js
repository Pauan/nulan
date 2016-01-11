export const SYMBOL      = 0;
export const CONSTRUCTOR = 1;
export const PROTOCOL    = 2;
export const INTEGER     = 3;
export const NUMBER      = 4;
export const STRING      = 5;
export const CALL        = 6;
export const LIST        = 7;
export const RECORD      = 8;
export const ASSIGN      = 9;
export const LAMBDA      = 10;
export const BAR         = 11;
export const DOT         = 12;
export const TYPE        = 13;
export const QUOTE       = 14;
export const UNQUOTE     = 15;
export const SPLICE      = 16;

export const symbol = (value, filename, lines, start, end) => {
  return { type: SYMBOL, value, filename, lines, start, end };
};

export const constructor = (value, filename, lines, start, end) => {
  return { type: CONSTRUCTOR, value, filename, lines, start, end };
};

export const protocol = (value, filename, lines, start, end) => {
  return { type: PROTOCOL, value, filename, lines, start, end };
};

export const integer = (value, filename, lines, start, end) => {
  return { type: INTEGER, value, filename, lines, start, end };
};

export const number = (value, filename, lines, start, end) => {
  return { type: NUMBER, value, filename, lines, start, end };
};

export const string = (value, filename, lines, start, end) => {
  return { type: STRING, value, filename, lines, start, end };
};

export const call = (value, filename, lines, start, end) => {
  return { type: CALL, value, filename, lines, start, end };
};

export const list = (value, filename, lines, start, end) => {
  return { type: LIST, value, filename, lines, start, end };
};

export const record = (value, filename, lines, start, end) => {
  return { type: RECORD, value, filename, lines, start, end };
};

export const assign = (left, right, filename, lines, start, end) => {
  return { type: ASSIGN, left, right, filename, lines, start, end };
};

export const dot = (left, right, filename, lines, start, end) => {
  return { type: DOT, left, right, filename, lines, start, end };
};

export const type = (left, right, filename, lines, start, end) => {
  return { type: TYPE, left, right, filename, lines, start, end };
};

export const lambda = (parameters, body, filename, lines, start, end) => {
  return { type: LAMBDA, parameters, body, filename, lines, start, end };
};

export const bar = (value, filename, lines, start, end) => {
  return { type: BAR, value, filename, lines, start, end };
};

export const quote = (value, filename, lines, start, end) => {
  return { type: QUOTE, value, filename, lines, start, end };
};

export const unquote = (value, filename, lines, start, end) => {
  return { type: UNQUOTE, value, filename, lines, start, end };
};

export const splice = (value, filename, lines, start, end) => {
  return { type: SPLICE, value, filename, lines, start, end };
};
