const babel = require("rollup-plugin-babel");
const es2015 = require("babel-preset-es2015-rollup");

module.exports = {
  entry: "test/test.js",
  dest: "build/test.js",

  format: "cjs",

  plugins: [
    babel({
      presets: [ es2015 ]
    })
  ]
};
