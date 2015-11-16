var fs = require("fs");
var esperanto = require("esperanto");

function mkdir(x, cb) {
  fs.mkdir(x, function (err) {
    if (err) {
      if (err.code === "EEXIST") {
        return cb(null);
      } else {
        return cb(err);
      }
    } else {
      return cb(null);
    }
  });
}

function compile(file, cb) {
  esperanto.bundle({
    entry: file
  }).then(function (bundle) {
    return cb(null, bundle.concat().code);
    /*var x = bundle.toUmd({ name: "nulan", strict: true });
    return cb(null, x.code);*/
  }).catch(function (err) {
    process.nextTick(function () {
      return cb(err);
    });
  });
}

function run(cb) {
  mkdir("build", function (err) {
    if (err) return cb(err);

    compile("src/nulan", function (err, code) {
      if (err) return cb(err);

      fs.writeFile("build/nulan.js", code, cb);
    });
  });
}

run(function (err) {
  if (err) throw err;
});