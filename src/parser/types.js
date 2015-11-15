export const symbol = (value, filename, lines, start, end) => {
  return {
    type: 0,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const integer = (value, filename, lines, start, end) => {
  return {
    type: 1,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const number = (value, filename, lines, start, end) => {
  return {
    type: 2,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const string = (value, filename, lines, start, end) => {
  return {
    type: 3,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};

export const list = (value, filename, lines, start, end) => {
  return {
    type: 4,
    value: value,
    filename: filename,
    lines: lines,
    start: start,
    end: end
  };
};
