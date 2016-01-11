export const SYMBOL  = 0;
export const INTEGER = 1;
export const NUMBER  = 2;
export const STRING  = 3;
export const CALL    = 4;
export const LIST    = 5;
export const RECORD  = 6;
export const ASSIGN  = 7;
export const LAMBDA  = 8;
export const BAR     = 9;
export const DOT     = 10;
export const TYPE    = 11;
export const QUOTE   = 12;
export const UNQUOTE = 13;
export const SPLICE  = 14;

export const symbol = (value, filename, lines, start, end) => {
  return {
    type: SYMBOL,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const integer = (value, filename, lines, start, end) => {
  return {
    type: INTEGER,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const number = (value, filename, lines, start, end) => {
  return {
    type: NUMBER,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const string = (value, filename, lines, start, end) => {
  return {
    type: STRING,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const call = (value, filename, lines, start, end) => {
  return {
    type: CALL,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const list = (value, filename, lines, start, end) => {
  return {
    type: LIST,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const record = (value, filename, lines, start, end) => {
  return {
    type: RECORD,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const assign = (left, right, filename, lines, start, end) => {
  return {
    type: ASSIGN,
    left: left,
    right: right,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const dot = (left, right, filename, lines, start, end) => {
  return { type: DOT, left, right, filename, lines, start, end };
};

export const type = (left, right, filename, lines, start, end) => {
  return { type: TYPE, left, right, filename, lines, start, end };
};

export const lambda = (parameters, body, filename, lines, start, end) => {
  return {
    type: LAMBDA,
    parameters: parameters,
    body: body,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
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
