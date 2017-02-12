const babel = require("rollup-plugin-babel");

module.exports = {
  entry: "bootstrap/nulan.js",
  dest: "nulan",

  format: "cjs",
  banner: "#! /usr/bin/env node",

  plugins: [
    babel({
      presets: [
        ["es2015", { modules: false }]
      ],
      plugins: [
        "transform-async-to-generator",
        "external-helpers"
      ],
      exclude: "node_modules/**"
    })
  ],

  // TODO is this correct ?
  onwarn: function () {}
};
