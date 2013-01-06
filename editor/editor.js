var editor = (function (n) {
  "use strict";

  function stylesheet(s) {
    var x = document.createElement("link")
    x.rel = "stylesheet"
    x.href = s
    document.head.appendChild(x)
  }
/*
  function script(a, f) {
    if (Array.isArray(a)) {
      var i = a.length

      var elems = a.map(function (s) {
        var x = document.createElement("script")
        x.src = s
        return x
      })

      var event = function (e) {
        if (e.target === this) {
          if (--i === 0) {
            elems.forEach(function (x) {
              x.removeEventListener("load", event, true)
            })
            f()
          }
        }
      }

      elems.forEach(function (x) {
        if (f) {
          x.addEventListener("load", event, true)
        }
        document.body.appendChild(x)
      })
    } else {
      script([a], f)
    }
  }*/

  function script(a, f) {
    if (Array.isArray(a)) {
      a.forEach(function (s) {
        var x = document.createElement("script")
        x.src = s
        x.async = false
        document.body.appendChild(x)
      })
      if (f) f()
    } else {
      script([a], f)
    }
  }

  function domReady(f) {
    if (document.readyState === "complete") {
      f()
    } else {
      addEventListener("readystatechange", function anon(e) {
        if (document.readyState === "complete") {
          this.removeEventListener(e.type, anon, true)
          f()
        }
      }, true)
    }
  }


  stylesheet("../lib/codemirror/lib/codemirror.css")
  stylesheet("codemirror/custom.css")
  stylesheet("editor.css")

  script(["../src/NULAN.parse.js",
          "../lib/nino/compile.js",
          "../lib/nino/transform.js",
          "../lib/nino/partial.js",
          "../src/NULAN.js"], function () {
    script("../src/NULAN.macros.js", function () {
      script("../src/modes/browser.js")
    })
  })

  script("../lib/codemirror/lib/codemirror.js", function () {
    //"../lib/codemirror/lib/util/searchcursor.js"
    script("codemirror/nulan.js")
  })


  n.options = {
    debug: false
  }

  n.init = function (name, defText, resetText) {
    //localStorage[name + ".debug"] = "yes"
    //n.options.debug = true

    domReady(function () {
      var lines = []

      defText = document.getElementById(defText)

      if (!localStorage[name + ".saved"]) {
        localStorage[name + ".saved"] = defText.textContent
      }
    /*
      i.id = "input"
      i.setAttribute("autofocus", "") // TODO
    */

      function editor(x, o) {
        x = CodeMirror(x, {
          theme: "custom",
          value: localStorage[name + ".saved"],
          lineWrapping: true,
          //lineNumbers: true,
          autofocus: true,
          extraKeys: {
            "Tab":       "indentMore",
            "Shift-Tab": "indentLess"
          },
          mode: "nulan"
        })

/*
        var y = x.getWrapperElement()

        var buttons = document.createElement("div")
        buttons.id = "buttons"

        var reset = document.createElement("button")
        reset.textContent = resetText

        reset.addEventListener("click", function () {
          x.setValue(defText.textContent)
        })

        var label = document.createElement("label")
        label.id = "debug"

        var debug = document.createElement("input")
        debug.type = "checkbox"
        debug.checked = n.options.debug = !!localStorage[name + ".debug"]

        debug.addEventListener("change", function () {
          localStorage[name + ".debug"] = this.checked ? "yes" : ""
          n.options.debug = this.checked
          //output(x, o)
          process(x, localStorage[name + ".saved"], o) // TODO: inefficient, but necessary to make the errors toggle
        }, true)

        buttons.appendChild(reset)

        label.appendChild(debug)
        label.appendChild(document.createTextNode("Debug mode"))
        buttons.appendChild(label)

        y.insertBefore(buttons, y.firstChild)*/


        //var timer

        x.on("change", function (x) {
          //clearTimeout(timer)
          var s = x.getValue()
          localStorage[name + ".saved"] = s
          process(s)
          output(x, o)
        })
    /*
        x.on("update", function () {
          console.log("UPDATE")
        })*/

        x.on("cursorActivity", function (x) {
          output(x, o) // TODO: don't do a full output
        })

        // http://codemirror.net/demo/activeline.html
        var hlLine = x.addLineClass(0, "background", "activeline")

        x.on("cursorActivity", function () {
          var cur = x.getLineHandle(x.getCursor().line)
          if (cur !== hlLine) {
            x.removeLineClass(hlLine, "background", "activeline")
            hlLine = x.addLineClass(cur, "background", "activeline")
          }
        })

        process(x.getValue()) //localStorage[name + ".saved"]
        output(x, o)

        return x
      }

      // http://codemirror.net/demo/widget.html
      function error(oEditor, e, end) {
        var line, message
        if (e instanceof NULAN.Error) {
          line = e.start.line
          message = (n.options.debug
                      ? "" + e
                      : e.originalMessage)
        } else {
          line = end
          message = "" + e
        }

        var x = document.createElement("div")
        x.className = "nulan-error"

        var y = document.createElement("span")
        y.className = "nulan-error-icon"
        y.textContent = "!"
        x.appendChild(y)

        y = document.createElement("span")
        y.className = "nulan-error-message"
        y.textContent = message
        x.appendChild(y)

        lines.push(oEditor.addLineWidget(line - 1, x))
      }

/*
      function some(a, f) {
        var x
        for (var i = 0, iLen = a.length; i < iLen; ++i) {
          if ((x = f(a[i]))) {
            return x
          }
        }
        return false
      }

      function find(x, info) {
        if (Array.isArray(x)) {
          return some(x, function (x) {
            return find(x, info)
          })
        } else if (x instanceof NULAN.Symbol) {
          if (x.line === info.line + 1 &&
              info.ch + 1 >= x.column &&
              info.ch + 1 <= x.length + x.column) {
            return x.value
          } else {
            return false
          }
        } else {
          return false
        }
      }*/

      var marks

      function nulanToCodeMirror(x) {
        return { line: x.line - 1, ch: x.column - 1 }
      }

      function mark(o, currentToken, startCursor, line, end) {
          //, line = startCursor.line
        o.operation(function () {
          var ch, sLine
          var s = /^.*/.exec(tokenValue(currentToken))[0]
            , r = []
          while (line < end) {
            ch = 0
            sLine = o.getLine(line)
            while (true) {
              ch = sLine.indexOf(s, ch)
              if (ch === -1) {
                break
              } else {
                r.push({ line: line, ch: ch + 1 })
                ch += s.length
              }
            }
            ++line
          }
          r.forEach(function (x) {
            var y = o.getTokenAt(x)
              , t = y.state.token
            if (compatibleTypes(y, currentToken) &&
                tokenValue(y) === tokenValue(currentToken) &&
                currentToken.state.box === y.state.box) {
              marks.push(o.markText(nulanToCodeMirror(t.start),
                                    nulanToCodeMirror(t.end),
                                    { className: "CodeMirror-matchhighlight" }))
            }
          })
        })
      }

      function isHighlight(x) {
        return x.type === "keyword"    ||
               x.type === "variable"   ||
               x.type === "variable-2" ||
               x.type === "atom"       ||
               x.type === "number"     ||
               x.type === "string"     ||
               x.type === "property"
      }

      function typeToType(x) {
        if (x === "variable-2") {
          return "local"
        } else if (x === "variable") {
          return "normal"
        } else if (x === "keyword") {
          return "macro"
        }
      }

      function compatibleTypes(x, y) {
        return ((x.type === "property" || x.type === "string") &&
                (y.type === "property" || y.type === "string")) ||
               x.type === y.type
      }

      function tokenValue(x) {
        return x.state.token.value || x.string
      }

      function outputToken(x, r) {
        if (x.state.box) {
          var s = typeToType(x.type)
          if (x.state.box.mode["compile"] && s !== "macro") {
            r.push(["token",
                     ["type   ", s],
                     ["string ", x.string],
                     ["value  ", tokenValue(x)],
                     ["mode   ", Object.keys(x.state.box.mode).join(", ")]])
          } else {
            r.push(["token",
                     ["type   ", s],
                     ["string ", x.string],
                     ["value  ", tokenValue(x)]])
          }
        } else {
          r.push(["token",
                   ["type   ", x.type],
                   ["string ", x.string],
                   ["value  ", tokenValue(x)]])
        }
      }

      function output(oEditor, o) {
        if (marks) {
          oEditor.operation(function () {
            marks.forEach(function (x) {
              x.clear()
            })
          })
        }
        marks = []

        var output = {
          //error:        [],
          prints:       [],
          eval:         [],
          compile:      [],
          compileEvals: [],
          parse:        []
        }

        var start = oEditor.getCursor(true).line + 1
          , end   = oEditor.getCursor(false).line + 1

        //var currentForm

        n.forms.forEach(function (x) {
          /*if ("error" in x) {
            output.error.push(x.error)
          }*/
          /*if (start >= x.start && end <= x.end) {
            currentForm = x
          }*/
          if (x.start <= end && x.end >= start) {
            output.prints.push.apply(output.prints, x.prints)
            if ("eval" in x) {
              output.eval.push(x.eval)
            }
            if ("compile" in x) {
              output.compile.push(x.compile)
            }
            output.compileEvals.push.apply(output.compileEvals, x.compileEvals)
            if ("parse" in x) {
              output.parse.push(x.parse)
            }
          }
        })

        var r = []

        var startCursor  = oEditor.getCursor("head")
          , currentToken = oEditor.getTokenAt(startCursor)

        /*if (currentToken.type === "variable") {
          if (currentForm) {
            mark(oEditor, currentToken, startCursor, currentForm.start - 1, currentForm.end)
          }
        } else */if (isHighlight(currentToken)) {
          mark(oEditor, currentToken, startCursor, 0, oEditor.lineCount())
          if (n.options.debug) {
            outputToken(currentToken, r)
          }
        } else {
          currentToken = oEditor.getTokenAt({ line: startCursor.line, ch: startCursor.ch + 1 })
          /*if (currentToken.type === "variable") {
            if (currentForm) {
              mark(oEditor, currentToken, startCursor, currentForm.start - 1, currentForm.end)
            }
          } else */if (isHighlight(currentToken)) {
            mark(oEditor, currentToken, startCursor, 0, oEditor.lineCount())
            if (n.options.debug) {
              outputToken(currentToken, r)
            }
          }
        }
/*
        if (currentForm) {
                                                // TODO: slight code duplication
          currentForm = find(currentForm.parse, oEditor.getCursor(true))
          if (currentForm) {

          }
        }*/

        /*if (output.error.length) {
          r.push(["error"].concat(output.error))
        }*/
        if (n.options.debug) {
          if (output.prints.length) {
            r.push(["print"].concat(output.prints))
          }
          if (output.eval.length) {
            r.push(["eval"].concat(output.eval.map(pretty)))
          }
        } else {
          r.push([""].concat(output.prints, output.eval.map(pretty)))
        }

        if (n.options.debug) {
          if (output.compile.length) {
            r.push(["compile"].concat(output.compile))
          }
          if (output.compileEvals.length) {
            r.push(["compile-eval"].concat(output.compileEvals))
          }
          if (output.parse.length) {
            r.push(["parse"].concat(output.parse.map(function (x) {
              return prettyParse(x)
            })))
          }
        }

        function prettyOutput1(x) {
          return "  @" + x[0] + x[1]
        }

        function prettyOutput(r) {
          return r.map(function (x) {
            return (x[0] !== ""
                     ? "@" + x[0] + "\n\n"
                     : "\n") +
                    // TODO: ew
                   x.slice(1).map(function (x) {
                     if (Array.isArray(x)) {
                       return prettyOutput1(x)
                     } else {
                       return "  " + x.replace(/\n/g, "$&  ")
                     }
                   }).join("\n\n")
          }).join("\n\n\n")
        }

        o.textContent = prettyOutput(r)
        // NUIT.serialize(r, { multiline: true })
      }

      // TODO: these pretty functions should be in Nulan
      function prettyString(x) {
        return "\"" + x.replace(/[\r\n\t"\\]/g, function (s) {
          if (s === "\r") {
            return "\\r"
          } else if (s === "\n") {
            return "\\n"
          } else if (s === "\t") {
            return "\\t"
          } else {
            return "\\" + s
          }
        }) + "\""
      }

      function prettyParse(x) {
        // TODO: code duplication with pretty
        if (Array.isArray(x)) {
          if (x.length) {
            var seen
            return "(" + //prettyParse(x[0]) + " " +
                                  // TODO: ew slice
                                  x.map(function (x) {
                                    return withIndent(function () {
                                      indent += 2
                                      if (Array.isArray(x)) {
                                        seen = true
                                      }
                                      if (seen) {
                                        return "\n" + spaces() + prettyParse(x)
                                      } else {
                                        return prettyParse(x)
                                      }
                                    })
                                  }).join(" ") + ")"
          } else {
            return "()"
          }
        } else if (x instanceof NULAN.Wrapper) {
          return prettyParse(x.value)
        } else if (typeof x === "string") {
          return prettyString(x)
        } else if (x === void 0) {
          return "()"
        } else {
          return "" + x
        }
      }
    /*
      function coords() {
        output(this, o)
      }

      // TODO: onselectstart onselectionchange
      i.addEventListener("select", coords, true)
      // TODO: keydown and mousedown fire before selection changes, ugh
      i.addEventListener("keyup", coords, true)
      i.addEventListener("mouseup", coords, true)
    */
      //var myEval = eval

      var indent = 0

      function withIndent(f) {
        var old = indent
        //indent += 2
        try {
          var x = f()
        } finally {
          indent = old
        }
        return x
      }

      function indenter(s) {
        indent += s.length
        return s
      }

      function spaces() {
        return new Array(indent + 1).join(" ")
      }

      function prop(x) {
        if (/^[$_a-zA-Z][$_a-zA-Z0-9]*$/.test(x)) {
          return x
        } else {
          return pretty(x)
        }
      }

      function pretty(x) {
        // TODO: a bit ew
        if (Array.isArray(x)) {
          var seen
          return "{" + x.map(function (x) {
                         return withIndent(function () {
                           indent += 2
                           if (Array.isArray(x)) {
                             seen = true
                           }
                           if (seen) {
                             return "\n" + spaces() + pretty(x)
                           } else {
                             return pretty(x)
                           }
                         })
                       }).join(" ") + "}"
        /*} else if (x instanceof Object && typeof x !== "function") {
          return "{\n" + withIndent(function () {
                           return Object.getOwnPropertyNames(x).map(function (s) {
                             return spaces() + prop(s) + ": " +
                                      (x[s] === x
                                        ? "[Circular]"
                                        : pretty(x[s]))
                           }).join(",\n")
                         }) + "\n" + spaces() + "}"*/
        } else if (typeof x === "function") {
          return "#<function>"
        } else if (x && typeof x === "object") {
          return "#<dict>"
        } else if (typeof x === "string") {
          return "\"" + x.replace(/[\r\n\t"\\]/g, function (s) {
            if (s === "\r") {
              return "\\r"
            } else if (s === "\n") {
              return "\n" + spaces()
            } else if (s === "\t") {
              return "\\t"
            } else {
              return "\\" + s
            }
          }) + "\""
        } else if (x === void 0) {
          return "()"
        } else if (x === true) {
          return "%t"
        } else if (x === false) {
          return "%f"
        } else {
          return "" + x
        }
      }

      var oldContext, sandbox, sandboxParent
/*
      clearTimeout(timer)
        oEditor.operation(function () {
          if (!isTimer) {
            lines.forEach(function (x) {
              oEditor.removeLineWidget(x)
            })
          }

          if (sandbox) {
            sandboxParent.removeChild(sandbox)
          }

          sandbox = document.createElement("iframe")
          sandbox.src = "javascript:;" // TODO: hack needed for Firefox
          //sandbox.className = "cm-s-custom CodeMirror"
          sandboxParent.appendChild(sandbox)

          var myEval = sandbox.contentWindow.eval

          lines   = []

          output(oEditor, o)
        })

        timer = setTimeout(function () {
              isTimer = false
              error(oEditor, err, end)
            }, 400)

        o.eval = myEval(o.compile) // indirect eval
*/
      function process(s) {
        if (oldContext) {
          oldContext()
        }
        oldContext = NULAN.withNewContext()

        n.forms = []

        NULAN.tokenInfo = {} // TODO: make this a part of withNewContext?

        // TODO: use enriched arrays instead of a start and line argument ?
        NULAN.parse(s, function (err, x, start, end) {
          var o = {
            compileEvals: [],
            prints:       [],
            start:        start,
            end:          end,
            /*vars:         NULAN.vars,
            values:       NULAN.values,
            syntaxRules:  NULAN.syntaxRules*/
          }

          if (err) {
            o.error = err
          } else {
            o.parse = x

            NULAN.options.debug = function (x) {
              o.compileEvals.push(x)
            }

            console.log = /*sandbox.contentWindow.console.log = */function () {
              o.prints.push([].slice.call(arguments).join(" "))
            }

            //console.log("PROCESS", start)

            try {
              o.compile = NULAN.compile(x)
            } catch (e) {
              o.error = e
              //console.log("ERROR", e)
              //o.error = "" + e
              //o.textContent = print(["" + e])
            }
/*
            o.boxes       = NULAN.boxes
            o.vars        = NULAN.vars
            o.values      = NULAN.values
            o.syntaxRules = NULAN.syntaxRules

            //NULAN.boxes       = Object.create(NULAN.boxes)
            NULAN.vars        = Object.create(NULAN.vars)
            NULAN.values      = Object.create(NULAN.values)
            NULAN.syntaxRules = Object.create(NULAN.syntaxRules)*/
          }

          n.forms.push(o)
        })
      }

      ;(function (main) {
        main.id = "main"

        var x

        x = document.createElement("div")
        x.id = "sidebar"
        main.appendChild(x)

        var xEditor = document.createElement("div")
        xEditor.id = "editors"
        main.appendChild(xEditor)

        x = document.createElement("div")
        x.id = "panels"

        var xSandbox = document.createElement("div")
        xSandbox.id = "sandbox"
        sandboxParent = xSandbox
        x.appendChild(xSandbox)

        var xOutput = document.createElement("div")
        xOutput.id = "output"
        //xOutput.className = "cm-s-custom CodeMirror"

        var xText = document.createElement("div")

        var xLabel = document.createElement("label")
        xLabel.id = "debug-label"

        var xCheckbox = document.createElement("input")
        xCheckbox.type = "checkbox"
        xCheckbox.checked = n.options.debug = !!localStorage[name + ".debug"]

        xLabel.appendChild(xCheckbox)
        xLabel.appendChild(document.createTextNode("Debug mode"))

        xOutput.appendChild(xLabel)
        xOutput.appendChild(xText)
        x.appendChild(xOutput)

        main.appendChild(x)

        document.body.appendChild(main)

        xEditor = editor(xEditor, xText)

        xCheckbox.addEventListener("change", function () {
          localStorage[name + ".debug"] = n.options.debug = this.checked ? "yes" : ""
          //output(x, o)
          // TODO: save the errors and rerun them rather than calling process
          //process(xEditor, xEditor.getValue(), xText) // TODO: inefficient, but necessary to make the errors toggle
          //output(xEditor, xText)
        }, true)
      }(document.createElement("div")))
    })
  }

  return n
})({})
