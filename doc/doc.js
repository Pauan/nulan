var doc = (function (n) {
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
  stylesheet("doc.css")

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
    script("codemirror/nulan.js")
  })


  n.options = {
    debug: false
  }

  n.init = function (name, defText, resetText) {
    domReady(function () {
      var i// = document.createElement("textarea")
        , o = document.createElement("pre")
        , sandbox
        , sandboxParent
        , lines = []

      defText = document.getElementById(defText)

      if (!localStorage[name + ".saved"]) {
        localStorage[name + ".saved"] = defText.textContent
      }
    /*
      i.id = "input"
      i.setAttribute("autofocus", "") // TODO
    */
      o.id = "output"
      o.className = "cm-s-custom CodeMirror"

      function editor(x) {
        x = CodeMirror(x, {
          theme: "custom",
          value: localStorage[name + ".saved"],
          lineWrapping: true,
          lineNumbers: true,
          autofocus: true,
          extraKeys: {
            "Tab":       "indentMore",
            "Shift-Tab": "indentLess"
          },
          mode: "nulan"
        })

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
          output(x, o)
        }, true)

        buttons.appendChild(reset)

        label.appendChild(debug)
        label.appendChild(document.createTextNode("Debug mode"))
        buttons.appendChild(label)

        y.insertBefore(buttons, y.firstChild)

        var timer

        x.on("change", function (x, o) {
          clearTimeout(timer)
          var s = x.getValue()
          localStorage[name + ".saved"] = s
          process(x, s)
          /*timer = setTimeout(function () {

          }, 400)*/
        })
    /*
        x.on("update", function () {
          console.log("UPDATE")
        })*/

        x.on("cursorActivity", function (x) {
          output(x, o)
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


        process(x, localStorage[name + ".saved"])
      }

      // http://codemirror.net/demo/widget.html
      function error(oEditor, e, end) {
        var line, message
        if (e instanceof NULAN.Error) {
          line = e.line
          message = e.originalMessage
        } else {
          line = end
          message = "" + e
        }

        var x = document.createElement("div")
        x.style.fontFamily = "monospace"
        x.style.fontSize = "12px"
        x.style.background = "#ffa"
        x.style.color = "#a00"
        //x.style.padding = "2px 5px 3px"
        x.style.paddingTop = "1px"
        x.style.paddingBottom = "2px"

        var y = document.createElement("span")
        y.textContent = "!"
        y.style.color = "white"
        y.style.backgroundColor = "red"
        y.style.fontSize = "12px"
        y.style.fontWeight = "bold"
        y.style.borderRadius = "25%"
        y.style.cssFloat = "left"
        y.style.paddingLeft = "4px"
        y.style.paddingRight = "3px"
        y.style.paddingBottom = "1px"
        y.style.marginLeft = "2px"
        y.style.marginRight = "4px"

        x.appendChild(y)
        x.appendChild(document.createTextNode(message))
        lines.push(oEditor.addLineWidget(line - 1, x))
      }

      function output(oEditor, o) {
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

        n.forms.forEach(function (x) {
          /*if ("error" in x) {
            output.error.push(x.error)
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

        o.textContent = r.map(function (x) {
          return (x[0] !== ""
                   ? "@" + x[0] + "\n\n"
                   : "\n") +
            // TODO: ew
            x.slice(1).map(function (x) {
              return "  " + x.replace(/\n/g, "$&  ")
            }).join("\n\n")
        }).join("\n\n\n")
        // NUIT.serialize(r, { multiline: true })
      }

      function prettyParse(x) {
        // TODO: code duplication with pretty
        if (Array.isArray(x)) {
          return "(" + withIndent(function () {
                         var seen
                         return x.map(function (x) {
                           if (Array.isArray(x)) {
                             seen = true
                           }
                           if (seen) {
                             return "\n" + spaces() + prettyParse(x)
                           } else {
                             return prettyParse(x)
                           }
                         }).join(" ")
                       }) + ")"
        } else if (x instanceof NULAN.Wrapper) {
          return prettyParse(x.value)
        } else if (typeof x === "string") {
          return "\"" + x + "\"" // TODO replace " inside the string with \"
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
        indent += 2
        try {
          var x = f()
        } finally {
          indent = old
        }
        return x
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
          return "{" + withIndent(function () {
                         var seen
                         return x.map(function (x) {
                           if (Array.isArray(x)) {
                             seen = true
                           }
                           if (seen) {
                             return "\n" + spaces() + pretty(x)
                           } else {
                             return pretty(x)
                           }
                         }).join(" ")
                       }) + "}"
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
          // TODO
          return "\"" + x + "\""
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

      var oldContext

      function process(oEditor, s) {
        oEditor.operation(function () {
          lines.forEach(function (x) {
            oEditor.removeLineWidget(x)
          })
          if (sandbox) {
            sandboxParent.removeChild(sandbox)
          }
          if (oldContext) {
            oldContext()
          }

          oldContext = NULAN.withNewContext()

          sandbox = document.createElement("iframe")
          sandbox.src = "javascript:;" // TODO: hack needed for Firefox
          sandbox.id = "sandbox"
          sandbox.className = "cm-s-custom CodeMirror"
          sandboxParent.appendChild(sandbox)

          var myEval = sandbox.contentWindow.eval

          lines = []
          n.forms = []

          NULAN.parse(s, function (err, x, start, end) {
            if (err) {
              error(oEditor, err, end)
            } else {
              var o = {
                compileEvals: [],
                prints:       [],
                parse:        x,
                start:        start,
                end:          end,
                /*vars:         NULAN.vars,
                values:       NULAN.values,
                syntaxRules:  NULAN.syntaxRules*/
              }

              NULAN.options.debug = function (x) {
                o.compileEvals.push(x)
              }

              sandbox.contentWindow.console.log = function () {
                o.prints.push([].slice.call(arguments).join(" "))
              }

              //console.log("PROCESS", start)

              try {
                o.compile = NULAN.compile(x)
                o.eval = myEval(o.compile) // indirect eval
              } catch (e) {
                //console.log("ERROR", e)
                error(oEditor, e, end)

                //o.error = "" + e
                //o.textContent = print(["" + e])
              }

              o.vars        = NULAN.vars
              o.values      = NULAN.values
              o.syntaxRules = NULAN.syntaxRules

              NULAN.vars        = Object.create(NULAN.vars)
              NULAN.values      = Object.create(NULAN.values)
              NULAN.syntaxRules = Object.create(NULAN.syntaxRules)

              n.forms.push(o)
            }
          })

          output(oEditor, o)
        })
      }

      /*if (localStorage["saved"]) {
        i.value = localStorage["saved"]
        process(i.value)
      }

      i.addEventListener("input", function () {
        //if (this.value !== localStorage["saved"]) {
        localStorage["saved"] = this.value
        process(this.value)
        //}
      })*/

      ;(function (tab) {
        tab.id = "table"

        document.body.appendChild(tab)

        var y, z

        ;(function (x) {
          y = x.insertCell(0)
          y.width = "50%"
          y.height = "50%"
          z = document.createElement("div")
          z.className = "wrapper-for-firefox"
          z.appendChild(o)
          y.appendChild(z)
        })(tab.insertRow(0))

        ;(function (x) {
          y = x.insertCell(0)
          y.width = "50%"
          y.height = "50%"
          z = document.createElement("div")
          z.className = "wrapper-for-firefox"
          y.appendChild(z)
          sandboxParent = z

          y = x.insertCell(0)
          y.width = "50%"
          y.setAttribute("rowspan", "2")
          z = document.createElement("div")
          z.className = "wrapper-for-firefox"
          y.appendChild(z)
          editor(z)
        })(tab.insertRow(0))
      }(document.createElement("table")))
    })
  }

  return n
})({})
