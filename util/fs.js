import * as $fs from "fs";


export const mkdir = (a) =>
  new Promise((success, error) => {
    $fs.mkdir(a, (e) => {
      if (e) {
        if (e["code"] === "EEXIST") {
          success(false);

        } else {
          error(e);
        }

      } else {
        success(true);
      }
    });
  });


export const read_file = (path) =>
  new Promise((success, error) => {
    $fs.readFile(path, { "encoding": "utf8" }, (e, file) => {
      if (e) {
        if (e["code"] === "ENOENT") {
          success(null);

        } else {
          error(e);
        }

      } else {
        success(file);
      }
    });
  });
