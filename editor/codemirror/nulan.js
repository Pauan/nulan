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

  function getToken(state, col) {
    return NULAN.tokenInfo[state.line + ":" + (col + 1)]
  }

  function getType(x, state) {
    if (x.type === "symbol") {
      if (x.syntaxRule) {
        if (x.syntaxRule.endAt || x.syntaxRule.startAt) {
          return "brackets"
        } else {
          return "operator"
        }
      } else if (x.box) {
        state.box = x.box
        if (x.box.scope === "local") {
          return "variable-2"
        } else if (NULAN.isMacro(x.box)) {
          return "keyword"
        } else if (x.box.value === "true" || x.box.value === "false") {
          return "atom"
        } else {
          return "variable"
        }
      }
    } else if (x.type) {
      return x.type
    }
    return null
  }

  return {
    startState: function () {
      return {
        line: 0,
        tokens: []
      }
    },

    blankLine: function (state) {
      ++state.line
    },

    /*copyState: function (state) {
      return {
        tokens:
      }
    },*/

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
      if ((x = getToken(state, stream.column()))) {
        state.tokens.push(x)
      }
      stream.next()
      if (state.tokens.length) {
        x = state.tokens[state.tokens.length - 1]
        var i = 1
        while (true) {
          if (state.line >= x.end.line && (stream.column() + i + 1) >= x.end.column) {
            state.tokens.pop()
            break
          } else if (stream.eol() || (!x.override && getToken(state, stream.column() + i))) {
            break
          } else {
            stream.next()
          }
          ++i
        }
        state.token = x
        state.box = null
        return getType(x, state)
      }
      return null
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
