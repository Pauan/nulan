const os = require("os");

export const crash = (x) => {
  console["error"](x);
  process["exit"](1);
};

export const eol = os["EOL"];
