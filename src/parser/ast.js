export const SYMBOL  = 0;
export const INTEGER = 1;
export const NUMBER  = 2;
export const STRING  = 3;
export const CALL    = 4;
export const LIST    = 5;
export const RECORD  = 6;
export const ASSIGN  = 7;

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