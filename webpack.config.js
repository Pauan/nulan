const $path = require("path");

module.exports = {
    entry: {
      nulan: "./src/cli/nulan.ts"
    },

    output: {
        filename: "./dist/[name].js",
        library: "nulan",
        libraryTarget: "umd",
        pathinfo: true
    },

    // Enable sourcemaps for debugging webpack's output.
    devtool: "source-map",

    target: "node",

    resolve: {
        // Add '.ts' as resolvable extensions.
        extensions: [".webpack.js", ".web.js", ".ts", ".js"]
    },

    module: {
        rules: [
            // All files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'.
            {
                test: /\.ts$/,
                use: ["ts-loader"],
                include: $path.resolve(__dirname, "src")
            }
        ]
    }
};
