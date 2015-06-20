//import { then, on_error } from "../common/async";

var fs = require("fs");

// handle errors (somehow)
// read
// write
// read+write
// close
// know if it's closed
// automatically close things like file descriptors when closed

function proxy(c) {
  var out = Channel({
    read: async_fn(function* () {
      return (yield c).read();
    }),
    write: async_fn(function* (value) {
      return (yield c).write(value);
    }),
    error: async_fn(function* (err) {
      return (yield c).error(err);
    }),
    close: async_fn(function* () {
      return (yield c).close();
    })
  });

  return out;
}

function readable_stream(input, close) {
  return proxy(async(function* () {
    var stream = yield input;

    var ended = false;

    var c = Channel({
      close: function () {
        ended = true;
        //stream.removeListener("error", error);
        stream.removeListener("end", finish);
        stream.removeListener("close", finish); // TODO is this correct ?
        stream.removeListener("readable", readable);

        try {
          if (close) {
            return close(stream);
          }
        } finally {
          console.log("CLEANED UP");
        }
      }
    });

    function finish() {
      console.log("FINISH");
      // TODO close is async
      c.close();
    }

    function error(err) {
      console.log("ERROR", err);
      // TODO error is async
      c.error(err);
    }

    function readable() {
      console.log("READING");
      if (!ended) {
        var chunk = stream.read();
        console.log("READ", chunk === null);
        if (chunk !== null) {
          c.write(chunk).then(function (wrote) {
            if (wrote) {
              readable();
            } else {
              finish();
            }
          }, error);
        }
      }
    }

    stream.setEncoding("utf8");
    stream.on("error", error);
    stream.on("end", finish);
    stream.on("close", finish); // TODO is this correct ?
    stream.on("readable", readable);

    readable();

    return c;
  }));
}

function writable_stream(stream) {
  var ended = false;

  var c = Channel({
    close: function () {
      ended = true;
      stream.removeListener("error", error);
      stream.removeListener("finish", finish);
      stream.removeListener("drain", drain);

      return new Promise(function (resolve) {
        // TODO what if the error is emitted after calling stream.end ?
        stream.end(function () {
          console.log("FULLY CLOSED WRITER");
          resolve();
        });
      });
    }
  });

  function finish() {
    // TODO close is async
    c.close();
  }

  function error(e) {
    // TODO error is async
    c.error(e);
  }

  function drain() {
    console.log("READING");
    if (!ended) {
      c.read().then(function (value) {
        if (value === Channel.closed) {
          finish();
        } else if (stream.write(value, "utf8")) {
          drain();
        }
      }, error);
    }
  }

  // TODO is this correct ?
  //stream.setDefaultEncoding("utf8");
  stream.on("error", error);
  stream.on("finish", finish);
  stream.on("drain", drain);

  drain();

  return c;
}

var foldl = async_fn(function* (chan, init, f) {
  for (;;) {
    var value = yield chan.read();
    if (value === Channel.closed) {
      return init;
    } else {
      init = yield f(init, value);
    }
  }
});

var pipe = async_fn(function* (from, to) {
  for (;;) {
    var value = yield from.read();
    if (value === Channel.closed) {
      yield to.close();
      break;

    } else {
      var wrote = yield to.write(value);
      if (!wrote) {
        break;
      }
    }
  }
});

function wrap_nodejs(self, f) {
  return function () {
    // TODO is this faster or slower than pushing ?
    var a = new Array(arguments.length);

    for (var i = 0; i < arguments.length; ++i) {
      a[i] = arguments[i];
    }

    return new Promise(function (resolve, reject) {
      a.push(function (err, value) {
        if (err) {
          reject(err);

        } else if (arguments.length <= 2) {
          resolve(value);

        } else {
          var a = [value];
          for (var i = 2; i < arguments.length; ++i) {
            a.push(arguments[i]);
          }
          resolve(a);
        }
      });

      f.apply(self, a);
    });
  };
}


var fs_open  = wrap_nodejs(fs, fs.open);
var fs_close = wrap_nodejs(fs, fs.close);

var foldl = async_fn(function* (x, init, f) {
  var iterator = to_iterator(x);
  for (;;) {
    var info = yield iterator.next();
    if (info.done) {
      return init;
    } else {
      init = yield f(init, info.value);
    }
  }
});

var map = async_fn(function* (x, f) {
  return foldl(x, List(), async_fn(function* (out, _in) {
    return push(out, (yield f)(_in));
  }));
});


var read_file = function (path) {
  var fd = fs_open(path, "r");

  var stream = async(function* () {
    return fs.createReadStream(null, {
      encoding: "utf8",
      fd: yield fd,
      autoClose: false
    });
  });

  return readable_stream(stream, async_fn(function* () {
    // Automatically close the file descriptor when the channel is closed
    return fs_close(yield fd);
  }));
};

var write_file = function (path) {
  var fd = fs_open(path, "w");

  var stream = async(function* () {
    return fs.createWriteStream(null, {
      encoding: "utf8",
      fd: yield fd
    });
  });

  return writable_stream(stream);
};

var read_all = async_fn(function* (channel) {
  var a = yield foldl(channel, [], function (output, input) {
    output.push(input);
    return output;
  });
  return a.join("");
});

function log(x) {
  return then(x, function (x) {
    console.log(x);
    return x;
  });
}

async_run(function* () {
  yield _with(read_file("/home/pauan/Scratch/2014-09-30"), function (channel) {
    return log(channel.read());
  });

  //yield _with(yield read_file("/home/pauan/Scratch/2014-09-30"), async_fn(function* (channel) {
    //console.log(yield channel.read());
  //}));

  //_with(yield write_file("/home/pauan/Scratch/foo"), function (channel) {
    //return channel.write("foobarqux");
  //});
});
