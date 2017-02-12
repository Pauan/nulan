import { async_unkillable, async_killable, with_resource, chain, reply,
         make_thread_pool, run_in_thread_pool, kill_thread_pool,
         concurrent_null } from "../task";
import * as $fs from "node:fs";
import * as $path from "node:path";
import * as $list from "../list";
import * as $unsafe_list from "../unsafe/list";


export const DEFAULT_MODE = 0o666;

const ENOENT = 0;
const EEXIST = 1;

const EISDIR = 0;

const ENOTEMPTY = 0;


const callback_maybe = (success, error) =>
  (err, value) => {
    if (err) {
      if (err["code"] === "ENOENT") {
        return success({ $: 0 });

      } else {
        return error(err);
      }

    } else {
      return success({ $: 1, a: value });
    }
  };


// TODO test this
// TODO copy metadata ?
// TODO what about copying a file to itself ?
// TODO does this need to clean up anything when an error occurs ?
const copy_file = (from, to) =>
  // TODO abort it when it's killed ?
  async_killable((success, error) => {
    const on_end = () => {
      console.log("read end");
    };

    const on_finish = () => {
      console.log("write finish");
      return success(null);
    };

    const reader = $fs.createReadStream(null, { "fd": from, "autoClose": false });
    reader["on"]("error", error);
    reader["on"]("end", on_end);
    reader["on"]("finish", () => {
      console.log("read finish");
    });

    const writer = $fs.createWriteStream(null, { "fd": to, "autoClose": false });
    writer["on"]("error", error);
    writer["on"]("end", () => {
      console.log("write end");
    });
    writer["on"]("finish", on_finish);

    reader["pipe"](writer);

    // TODO
    return () => {
      reader["unpipe"](writer);
      reader["destroy"]();
      // TODO call writer["destroy"]() ?
      writer["end"]();
    };
  });


export const with_file = (path, flags, mode, f) =>
  with_resource(open(path, flags, mode), f, close);


// TODO test this
export const copy = (from, to) =>
  with_file(from, "r", DEFAULT_MODE, (from_fd) =>
    with_file(to, "wx", DEFAULT_MODE, (to_fd) =>
      copy_file(from_fd, to_fd)));


export const open = (path, flags, mode) =>
  async_unkillable((success, error) => {
    $fs.open(path, flags, mode, callback(success, error));
  });

export const close = (fd) =>
  async_unkillable((success, error) => {
    $fs.close(fd, callback_null(success, error));
  });

export const rename = (from, to) =>
  async_unkillable((success, error) => {
    $fs.rename(from, to, callback_null(success, error));
  });

export const realpath = (path) =>
  async_unkillable((success, error) => {
    $fs.realpath(path, callback_maybe(success, error));
  });

export const readdir = (path) =>
  async_unkillable((success, error) => {
    $fs.readdir(path, callback_maybe(success, error));
  });


export const read_file = (path) =>
  async_unkillable((success, error) => {
    $fs.readFile(path, { "encoding": "utf8" }, callback_maybe(success, error));
  });

export const write_file = (path, data) =>
  async_unkillable((success, error) => {
    $fs.writeFile(path, data, {
      "encoding": "utf8",
      "flag": "wx"
    }, callback_null(success, error));
  });


export const open_read = (path) =>
  async_unkillable((success, error) => {
    $fs.open(path, "r", DEFAULT_MODE, callback_maybe(success, error));
  });

export const open_write = (path, mode) =>
  async_unkillable((success, error) => {
    $fs.open(path, "wx", mode, callback(success, error));
  });

export const stat = (path) =>
  async_unkillable((success, error) => {
    $fs.stat(path, callback_maybe(success, error));
  });


export const is_directory = (path) =>
  chain(stat(path), (stats) => {
    if (stats.$ === 0) {
      return throw_error(new Error(""));

    } else {
      if (stats.a["isDirectory"]()) {
        return reply(true);

      } else {
        return reply(false);
      }
    }
  });

// TODO handle EMFILE
// TODO does it need to handle additional stuff (e.g. like rimraf) ?
export const rmdir = (path) =>
  async_unkillable((_success, _error) => {
    $fs.rmdir(path, (err) => {
      if (err) {
        if (err["code"] === "ENOTEMPTY") {
          _success(failure(ENOTEMPTY));

        } else {
          _error(err);
        }

      } else {
        _success(success(null));
      }
    });
  });

export const mkdir = (path) =>
  async_unkillable((_success, _error) => {
    $fs.mkdir(path, (err) => {
      if (err) {
        if (err["code"] === "ENOENT") {
          _success(failure(ENOENT));

        } else if (err["code"] === "EEXIST") {
          _success(failure(EEXIST));

        } else {
          _error(err);
        }

      } else {
        _success(success(null));
      }
    });
  });

// TODO EPERM ?
export const unlink = (path) =>
  async_unkillable((_success, _error) => {
    $fs.unlink(path, (err) => {
      if (err) {
        if (err["code"] === "ENOENT") {
          _success(success(null));

        } else if (err["code"] === "EISDIR") {
          _success(failure(EISDIR));

        } else {
          _error(err);
        }

      } else {
        _success(success(null));
      }
    });
  });



const watching_directories = {};
const watching_files = {};


const watch_directory = (path, f) => {
  if (watching_directories[path] == null) {
    const listeners = [];

    const watcher = $fs.watch(path, (type, filename) => {
      const length = listeners["length"];

      for (let i = 0; i < length; ++i) {
        listeners[i](filename);
      }
    });

    watching_directories[path] = {
      a: listeners,
      b: watcher
    };
  }

  return watching_directories[path].a["push"](f);
};


const unwatch_directory = (path, f) => {
  const a = watching_directories[path];

  $unsafe_list.remove_element(a.a, f);

  if (a.a["length"] === 0) {
    delete watching_directories[path];

    return a.b["close"]();
  }
};


const is_file_modified = (old_stats, new_stats) =>
  // TODO is this correct ?
  (new_stats["ino"] !==
   old_stats["ino"]) ||
  // TODO what if it doesn't have mtime ?
  (new_stats["mtime"]["getTime"]() !==
   old_stats["mtime"]["getTime"]());


// TODO test this
const _watch_file = (path, error, f) => {
  if (watching_files[path] == null) {
    const basename = $path.basename(path);
    const directory = $path.dirname(path);

    const errors = [error];
    const listeners = [f];

    const pool = make_thread_pool();

    // TODO unwatch if there is an error ?
    const on_error = (e) => {
      kill_thread_pool(pool);

      const length = errors.length;

      for (let i = 0; i < length; ++i) {
        errors[i](e);
      }
    };

    const trigger = (value) =>
      concurrent_null($list.transform(listeners, (f) => f(value)));

    const state = {
      a: directory,
      b: errors,
      c: listeners,
      d: pool,
      e: { $: 0 }
    };

    run_in_thread_pool(pool, on_error, chain(stat(path), (old_stats) => {
      const compare = (new_stats) => {
        if (old_stats.$ === 0 && new_stats.$ === 1) {
          old_stats = new_stats;
          return trigger(0);

        } else if (old_stats.$ === 1 && new_stats.$ === 0) {
          old_stats = new_stats;
          return trigger(2);

        } else if ((old_stats.$ === 1 && new_stats.$ === 1) &&
                   is_file_modified(old_stats.a, new_stats.a)) {
          old_stats = new_stats;
          return trigger(1);

        } else {
          old_stats = new_stats;
          return reply(null);
        }
      };

      // TODO use throttling ?
      const on_change = (filename) => {
        console.log(filename);

        // TODO test this
        if (!filename || filename === basename) {
          run_in_thread_pool(pool, on_error, chain(stat(path), compare));
        }
      };

      watch_directory(directory, on_change);
      state.e = { $: 1, a: on_change };

      return reply(null);
    }));

    watching_files[path] = state;

  } else {
    watching_files[path].b["push"](error);
    return watching_files[path].c["push"](f);
  }
};


const _unwatch_file = (path, error, f) => {
  const state = watching_files[path];

  $unsafe_list.remove_element(state.b, error);
  $unsafe_list.remove_element(state.c, f);

  // TODO test that a.b["length"] is 0 ?
  if (state.c["length"] === 0) {
    delete watching_files[path];

    kill_thread_pool(state.d);

    // TODO test this
    if (state.e.$ === 1) {
      return unwatch_directory(state.a, state.e.a);
    }
  }
};


// TODO use absolute file path ?
// TODO can this be made more efficient ?
// TODO what if the grandparent directory is removed ?
export const watch_file = (path, f) =>
  async_killable((success, error) => {
    _watch_file(path, error, f);

    return () =>
      _unwatch_file(path, error, f);
  });
