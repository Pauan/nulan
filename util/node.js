const os = require("os");
const util = require("util");

export const crash = (x) => {
  throw x;
};

export const eol = os["EOL"];

export const pretty = (x) =>
  util["inspect"](x, { "colors": true, "depth": 5 });

export const get_stack = (err) =>
  err["stack"] || get_message(err);

export const get_message = (err) =>
  err["message"] || "INVALID ERROR: " + err;

process["on"]("uncaughtException", (err) => {
  console["error"]("Error: " + get_stack(err));
  process["exit"](1);
});
