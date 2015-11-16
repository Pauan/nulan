const os = require("os");
const util = require("util");

export const crash = (x) => {
  throw new Error(x);
};

export const eol = os["EOL"];

export const pretty = (x) =>
  util["inspect"](x, { "colors": true });

process["on"]("uncaughtException", (err) => {
  console["error"](err["message"]);
  process["exit"](1);
});
