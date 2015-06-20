function Type(name, id) {
  return name + " (" + id + ")";
}

//var t_Type     = Type("Type",     "41139550-2192-444e-890b-cce125b78e8c");
//var t_isa      = Type("isa" ,     "ace28494-b793-4f52-beaf-49abce8174df");
var t_String   = Type("String",   "8108919f-26fc-4a1b-852d-c17d1636b555");
var t_Number   = Type("Number",   "d3d58145-bb5d-4218-932d-a8c1c01cab55");
var t_Function = Type("Function", "a5afd656-004f-4c4b-bf87-0bc323cb5184");
var t_Record   = Type("Record",   "7b183beb-f417-41f1-8c14-c07b3e6825ba");
var t_Tuple    = Type("Tuple",    "b82ad266-28c9-4c49-9b2c-deadf9644300");
var t_Dict     = Type("Dict",     "bd0ded6c-73a4-44de-bc4f-7f64869eca34");
var t_List     = Type("List",     "3573453f-af3c-408a-8435-66bc8731d286");
var t_Void     = Type("Void",     "7f297ff3-3674-4e13-8007-7ca6722c3f43");

var itf_chain       = {};
var itf_String_from = {};


function type(x) {
  if (isIsa(x)) {
    return x.type();
  //} else if (isRecord(x)) {
  //  return t_Record;
  } else if (isTuple(x)) {
    return t_Tuple;
  //} else if (isDict(x)) {
  //  return t_Dict;
  //} else if (isList(x)) {
  //  return t_List;
  } else if (typeof x === "number") {
    return t_Number;
  } else if (typeof x === "string") {
    return t_String;
  } else if (typeof x === "function") {
    return t_Function;
  } else if (x == null) {
    return t_Void;
  } else {
    throw new Error("Unknown type: " + x);
  }
}

function plural(i, s) {
  if (i === 1) {
    return i + " " + s;
  } else {
    return i + " " + s + "s";
  }
}

function isa(t, x) {
  if (type(x) === t) {
    return x;
  } else {
    throw new Error("Invalid type. Expected " + t + " but got " + type(x));
  }
}

function check_eq(actual, expected) {
  if (actual !== expected) {
    throw new Error("Expected " + expected + " but got " + actual);
  }
}

function check_arguments_equal(actual, expected) {
  if (actual !== expected) {
    throw new Error("Expected " + plural(expected, "argument") + " but got " + actual);
  }
}

function check_arguments_min(actual, expected) {
  if (actual < expected) {
    throw new Error("Expected at least " + plural(expected, "argument") + " but got " + actual);
  }
}

function pattern_fail(x) {
  throw new Error("Pattern match failed: " + str(x));
}

function extend(obj, type, name, f) {
  if (type in obj) {
    throw new Error("Cannot have two extensions in " + name + " for the same type: " + type);
  } else {
    obj[type] = f;
  }
}

function lookup(obj, type, name) {
  if (type in obj) {
    return obj[type];
  } else {
    throw new Error("There is no extension in " + name + " for type: " + type);
  }
}


function chain(x, f) {
  check_arguments_equal(arguments.length, 2);
  return lookup(itf_chain, type(x), "chain")(x, f);
}


function Tuple(a) {
  this._args = a;
}

Tuple.prototype.get = function (i) {
  return this._args[i];
};

function isTuple(x) {
  return x instanceof Tuple;
}

extend(itf_String_from, t_Tuple, "String-from", function (x) {
  return "[" + x._args.map(String_from).join(" ") + "]";
});


function Isa(t, a) {
  this._type = t;
  this._args = a;
}

Isa.prototype.type = function () {
  return this._type;
};

Isa.prototype.get = Tuple.prototype.get;

function isIsa(x) {
  return x instanceof Isa;
}

//extend(itf_String_from, t_Isa, "String-from", function (x) {
//  return "(isa " + x._type + " " + x._args.map(String_from).join(" ") + ")";
//});


function String_from(x) {
  check_arguments_equal(arguments.length, 1);
  return isa(t_String, lookup(itf_String_from, type(x), "String-from")(x));
}

function self(x) {
  return x;
}

function toStr(x) {
  return "" + x;
}

extend(itf_String_from, t_String, "String-from", self);
extend(itf_String_from, t_Number, "String-from", toStr);

function str_splice(base1) {
  check_arguments_min(arguments.length, 1);

  var base2 = String_from(base1);

  var args = [];
  for (var i = 1; i < arguments.length; ++i) {
    args.push(String_from(arguments[i]));
  }

  var input  = base2.split("@");
  var output = [];

  if ((args.length + 1) !== input.length) {
    throw new Error("Expected " + plural(input.length, "argument") + " but got " + (args.length + 1));
  }

  for (var i = 0; i < input.length - 1; ++i) {
    output.push(input[i]);
    output.push(args[i]);
  }

  output.push(input[i]);

  return output;
}

function str() {
  return str_splice.apply(this, arguments).join("");
}



var t_IO = Type("IO", "6fb2a574-04b0-4268-b3bf-a0e6ff570cb2");

function IO_wrap(x) {
  check_arguments_equal(arguments.length, 1);
  return new Isa(t_IO, ["wrap", x]);
}

function IO_from_Promise(f) {
  check_arguments_equal(arguments.length, 1);
  isa(t_Function, f);
  return new Isa(t_IO, ["promise", f]);
}

function isPromise(x) {
  return Object(x) === x && typeof x.then === "function";
}

function error() {
  var x = new Error(str.apply(this, arguments));
  return IO_from_Promise(function () {
    return Promise.reject(x);
  });
}

function log() {
  var x = str.apply(this, arguments);
  return IO_from_Promise(function () {
    console.log(x);
    return Promise.resolve(undefined);
  });
}

function Promise_from_IO(io) {
  //console.log("TESTING IT");
  isa(t_IO, io);
  check_eq(io.get(0), "promise");
  //console.log("TESTED IT");
  var f = io.get(1);
  var x = f();
  if (isPromise(x)) {
    return x;
  } else {
    throw new Error("Expected Promise but got " + str(x));
  }
}

extend(itf_chain, t_IO, "chain", function (x, f) {
  if (x.get(0) === "wrap") {
    return f(x.get(1));

  } else if (x.get(0) === "promise") {
    return IO_from_Promise(function () {
      return Promise_from_IO(x).then(function (data) {
        return f(data);
      });
    });

  } else {
    throw new Error("Failed pattern matching: " + str(x));
  }
});


var t_Maybe = Type("Maybe", "337731e3-5875-45b9-8242-28e65f378005");

function nothing() {
  check_arguments_equal(arguments.length, 0);
  return new Isa(t_Maybe, ["nothing"]);
}

function something(x) {
  check_arguments_equal(arguments.length, 1);
  return new Isa(t_Maybe, ["something", x]);
}

extend(itf_chain, t_Maybe, "chain", function (x, f) {
  if (x.get(0) === "nothing") {
    return x;
  } else if (x.get(0) === "something") {
    return f(x.get(1));
  }
});

extend(itf_String_from, t_Maybe, "String-from", function (x) {
  return "(Maybe " + x.get(0) + ")";
});


(type Log
  (log Any Any))

(extend chain -> (isa Log x) f
  (match x
    (log v1 s1)
      (match (f v1)
        (log v2 s2)
          (log v2 (concat s1 s2)))))


function main() {
  return chain(something(5), function (x) {
    return chain(log(x), function (x) {
      //console.log("HIYA", x);
      //return something(10);
      return chain(something(10), function (x) {
        return chain(something(x + 20), function (x) {
          return log(x);
        });
      });
    });
  });
  return chain(log("foo @ bar", "qux"), function (x) {
    console.log("HIYA", x);
    return chain(IO_wrap(20), function (x) {
      console.log(x);
      return chain(log("testhingyes"), function () {
        return log("LAST");
      });
    });
  });
}


function forever(x) {
  return chain(x, function () {
    return forever(x);
  });
}

function run1(f) {
  return Promise_from_IO(f).then(function (x) {
    if (type(x) === t_Void) {
      return x;

    } else {
      isa(t_IO, x);

      if (x.get(0) === "wrap") {
        return x.get(1);

      } else if (x.get(0) === "promise") {
        return run1(x);

      } else {
        pattern_fail(x);
      }
    }
  });
}

// TODO is there an easier way of doing this ?
function catcher(f) {
  return new Promise(function (resolve) {
    resolve(f());
  });
}

function run(f) {
  catcher(f).then(run1).then(function (x) {
    console.log("VALUE", x);

  }).catch(function (err) {
    console.error(err.stack);
  });
}

run(main);
