import { inspect } from "util";
export { EOL as eol } from "os";


export const pretty = (x) =>
  inspect(x, { "colors": true, "depth": 100 });

export const get_stack = (err) =>
  err["stack"] || get_message(err);

export const get_message = (err) =>
  err["message"] || "INVALID ERROR: " + err;

process["on"]("uncaughtException", (err) => {
  console["error"]("Error: " + get_message(err));
  process["exit"](1);
});
