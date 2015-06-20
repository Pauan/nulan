var empty_promise = Promise.resolve();

function async_generator(generator) {
  function loop(info) {
    // The generator returned a value, so that becomes the value of the promise
    if (info.done) {
      return info.value;

    // The generator yielded to a promise, so we wait for the promise to settle
    } else {
      // TODO should this use Promise.resolve ?
      // This uses Promise.resolve for two reasons:
      //
      //   1) If you yield to a non-native Promise (e.g. a promise library
      //      like Q or Bluebird), it might not behave the same as a native
      //      Promise. Using Promise.resolve ensures that we get a native
      //      Promise, with the correct behavior
      //
      //   2) Using Promise.resolve ensures that if you yield to a non-promise
      //      (e.g. a number, string, etc.), it will have the same behavior as
      //      if it were a promise
      //
      return Promise.resolve(info.value).then(success, error);
    }
  }

  function error(e) {
    // If the promise is rejected, we throw the error into the generator,
    // so that it can be handled by `try...catch/finally`. If it isn't
    // handled, `generator.throw` will throw the error back up here,
    // and then it will be caught by `then`
    //
    // In other words:
    //
    //   Promise rejected ->
    //   Throw into generator ->
    //   Thrown back out of generator ->
    //   Caught by then ->
    //   Promise rejected
    //
    return loop(generator.throw(e));
  }

  function success(value) {
    // It's possible for `generator.next` to throw an error, but that's
    // okay, because it will be caught by `then`
    return loop(generator.next(value));
  }

  // TODO is there a better/faster way of ensuring that this runs asynchronously ?
  // This is to ensure that the generator is always run asynchronously,
  // and that if `success` throws an error, it will reject the promise
  return empty_promise.then(success);
}

function async(f) {
  return async_generator(f());
}

function async_fn(f) {
  return function () {
    return async_generator(f.apply(this, arguments));
  }
}

// TODO is this needed ?
function async_run(f) {
  return async(f).catch(function (e) {
    console.error(e.stack);
    throw e;
  });
}

function sleep(i) {
  //var start = Date.now();

  return new Promise(function (resolve, reject) {
    setTimeout(resolve, i);

//    /*setTimeout(function () {
//      var end  = Date.now();
//      var diff = end - start;

//      if (diff < i) {
//        reject(new Error("Expected to sleep at least " + i + "ms but slept " + diff + "ms"));

//      } else {
//        resolve(diff);
//      }
//    }, i);*/
  });
}


function Channel(info) {
  if (info == null) {
    info = {};
  }

  var readers   = [];
  var writers   = [];
  var isClosed  = false;
  var isError   = false;
  var error_obj = null;

  var read = async_fn(function* () {
    if (isError) {
      throw error_obj;

    } else if (isClosed) {
      return Channel.closed;

    } else if (writers.length) {
      var info = writers.shift();
      info.resolve(true); // TODO is this correct ?
      return info.value;

    } else {
      return new Promise(function (resolve, reject) {
        readers.push({
          resolve: resolve,
          reject: reject
        });
      });
    }
  });

  var write = async_fn(function* (value) {
    if (isError) {
      throw error_obj;

    } else if (isClosed) {
      return false;

    } else if (readers.length) {
      var info = readers.shift();
      info.resolve(value);
      return true;

    } else {
      return new Promise(function (resolve, reject) {
        writers.push({
          resolve: resolve,
          reject: reject,
          value: value
        });
      });
    }
  });

  var error = async_fn(function* (err) {
    if (!isError) {
      readers.forEach(function (info) {
        info.reject(err);
      });

      writers.forEach(function (info) {
        info.reject(err);
      });

      readers = [];
      writers = [];

      error_obj = err;
      isError = true;
    }

    yield close();
  });

  var close = async_fn(function* () {
    if (!isClosed) {
      readers.forEach(function (info) {
        info.resolve(Channel.closed);
      });

      writers.forEach(function (info) {
        info.resolve(false);
      });

      readers = [];
      writers = [];

      isClosed = true;

      if (info.close) {
        yield info.close();
      }
    }
  });

  return {
    read: read,
    write: write,
    close: close,
    error: error
  };
}

Channel.closed = {};


var _with = async_fn(function* (channel, f) {
  try {
    return yield f(channel);
  } finally {
    console.log("CLOSING");
    // TODO replace with an interface
    yield channel.close();
    console.log("CLOSED");
  }
});




function then(x, f) {
  return Promise.resolve(x).then(f);
}

function on_error(x, f) {
  return Promise.resolve(x).catch(f);
}

function cleanup(x, f) {
  return Promise.resolve(x).then(function (x) {
    return then(f(), function () {
      return x;
    });
  }, function (err) {
    return then(f(), function () {
      throw err;
    });
  });
}

function plural(i, s) {
  if (i === 1) {
    return i + " " + s;
  } else {
    return i + " " + s + "s";
  }
}

function str_splice() {
  if (arguments.length < 1) {
    throw new Error("Expected at least 1 argument but got " + arguments.length);
  }

  var args = [];
  for (var i = 0; i < arguments.length; ++i) {
    args.push(arguments[i]);
  }

  return Promise.all(args).then(function (args) {
    var s = args[0];
    if (typeof s !== "string") {
      throw new Error("Expected string but got " + s);
    }

    var input  = s.split("@");
    var output = [];

    if (args.length !== input.length) {
      throw new Error("Expected " + plural(input.length, "argument") + " but got " + args.length);
    }

    for (var i = 0; i < input.length - 1; ++i) {
      output.push(input[i]);
      output.push(args[i + 1]);
    }

    output.push(input[i]);

    return output;
  });
}

function format() {
  return str_splice.apply(this, arguments).then(function (x) {
    return x.join("");
  });
}

function error() {
  return format.apply(this, arguments).then(function (x) {
    throw new Error(x);
  });
}
