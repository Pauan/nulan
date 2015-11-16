const os = require("os");
const util = require("util");

export const crash = (x) => {
  console["error"](x);
  process["exit"](1);
};

export const eol = os["EOL"];

export const pretty = (x) =>
  util["inspect"](x, { "colors": true });
