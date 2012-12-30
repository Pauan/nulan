"use strict";

CodeMirror.defineMode("nulan", function (config, parserConfig) {
/*
  // TODO: inefficient, maybe fold into the state object somehow?
  function getLast(x, state) {
    var last = x[0]
    for (var i = 1, iLen = x.length; i < iLen; ++i) {
      if (x[i].start > state.line) {
        break
      } else {
        last = x[i]
      }
    }
    return last// || { syntaxRules: {}, values: {}, vars: {} }
  }

  function symbol(x, state) {
    // /^([a-z]\-[a-z]|[a-zA-Z])+$/.test(r.join(""))
    if (/^[0-9]+$/.test(x)) {
      state.box = null
      return "number"
    } else if ((x = NULAN.tokenInfo[state.line + ":"])) {
      var last = getLast(doc.forms, state)
      state.box = last.vars[x]
      if (last.syntaxRules[x]) {
        return "special"
      } else {
        // TODO: a bit ugly
        var old  = NULAN.values
          , old2 = NULAN.vars
        NULAN.values = last.values
        NULAN.vars   = last.vars
        var b = NULAN.isMacro(new NULAN.Symbol(x), true)
        NULAN.values = old
        NULAN.vars   = old2

        if (b) {
          return "keyword"
                   // TODO: is this correct?
        } else if (NULAN.vars[x] === "true" || NULAN.vars[x] === "false") {
          return "atom"
        }
      }
      console.info(x, state.box)
      if ((x = last.vars[x]) && (x = last.boxes[x]) && x.scope === "local") {
        return "variable"
      } else {
        return "builtin"
      }
    }
  }*/

  return {
    startState: function () {
      return { line: 0 }
    },

    blankLine: function (state) {
      ++state.line
    },

    token: function (stream, state) {
      if (stream.sol()) {
        ++state.line
      }
      //var c = stream.peek()
      var x
      /*if ((c === "`" && state.raw) || c === "#") {
        stream.next()
        stream.eatWhile(/[^\n`]/)
        if (stream.peek() === "`") {
          state.raw = true
          stream.next()
        }
        return "comment"
      } else*/
      if ((x = NULAN.tokenInfo[state.line + ":" + (stream.column() + 1)])) {
        if (!x.length) {
          stream.next()
        } else {
          for (var i = 0; i < x.length; ++i) {
            stream.next()
          }
        }
        state.box = null
        if (x.type === "number") {
          return "number"
        } else if (x.type === "string") {
          return "string"
        } else if (x.type === "comment") {
          return "comment"
        } else if (x.type === "comment-doc") {
          return "comment-doc"
        } else if (x.type === "symbol") {
          if (x.syntax || !x.box) {
            return "special"
          } else {
            state.box = x.box
            if (x.box.scope === "local") {
              return "variable"
            } else if (NULAN.isMacro(x.box)) {
              return "keyword"
            } else {
              return "builtin"
            }
          }
        }
      } else {
        stream.next()
      }
      /*var c = stream.next()
        , r
      if (state.string) {
        if (c === "\"") {
          state.string = false
        } else {
          stream.eatWhile(function (c) {
            if (c === "\"") {
              state.string = false
              stream.next()
            } else if (c === "\\") {
              stream.next()
              return true
            } else {
              return true
            }
          })
        }
        return "string"
      } else {
        if ((c === "`" && state.raw) || c === "#") {
          stream.eatWhile(/[^\n`]/)
          if (stream.peek() === "`") {
            state.raw = true
            stream.next()
          }
          return "comment"
        } else if (c === "\"") {
          state.string = true
          return "string"
        } else {
          if (doc.forms && doc.forms.length) {
            var rules = getLast(doc.forms, state).syntaxRules
            if (rules[c] && rules[c].delimiter) {
              return "special"
            } else {
              r = [c]
              stream.eatWhile(function (c) {
                if (!(c === "`" && state.raw)) {
                  var x = rules[c]
                  if (!(x && x.delimiter)) {
                    r.push(c)
                    return true
                  }
                }
              })
              return symbol(r.join(""), state, stream.column())
            }
          }
        }
      }*/
    }
  }
})
