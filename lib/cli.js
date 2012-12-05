var e = require("events")
//var keypress = require('keypress')

exports.create = function (o) {
  var r = new e.EventEmitter()

  var buffer = []
    , saved  = []

  /*var i = process.stdin
    , o = process.stdout*/

  //keypress(i)

  //i.setRawMode(true)
  //i.resume()

  var rl = require("readline").createInterface({
    input: process.stdin,
    output: process.stdout,
    completer: o.completer
  })

  //rl.setPrompt("> ")
  rl.prompt()

  //o.write("> ")

  rl.on("line", function (s) {
    s = "" + s
    if (s === "") {
      if (buffer.length === 0) {
        process.exit()
      } else {
        s = buffer.join("\n")
        saved.push(s)
        r.emit("input", s)
        //o.write("> ")
        rl.prompt()
        buffer = []
      }
    } else {
      buffer.push(s)
    }
  })

  rl.on("close", function () {
    rl.write("\n")
  })

  return r
}

// TODO: is it necessary to assign to exports?
// exports =
//module.exports = r
