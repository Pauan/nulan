const purs = require("rollup-plugin-purs");

module.exports = {
  entry: "src/cli/Main.purs",
  dest: "nulan",

  format: "cjs",
  banner: "#! /usr/bin/env node",

  plugins: [
    purs()
  ]
};
