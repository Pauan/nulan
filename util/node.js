const os = require("os");
const util = require("util");

export const crash = (x) => {
  throw x;
};

export const eol = os["EOL"];

export const pretty = (x) =>
  util["inspect"](x, { "colors": true, "depth": 5 });

export const get_message = (err) =>
  err["stack"] || err["message"] || "INVALID ERROR: " + err;

process["on"]("uncaughtException", (err) => {
  console["error"](get_message(err));
  process["exit"](1);
});
