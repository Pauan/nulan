NULAN.eval("                                                               \n\
&builtin Number Math Boolean TypeError String Int16Array Float32Array isFinite Array DataView Float64Array ReferenceError SyntaxError Int32Array Uint16Array clearTimeout decodeURIComponent Uint32Array setTimeout eval console URIError unescape Date escape encodeURI Error Int8Array EvalError RangeError NaN isNaN parseInt undefined Object Uint8ClampedArray parseFloat Uint8Array clearInterval Infinity JSON Function setInterval encodeURIComponent decodeURI ArrayBuffer RegExp\n\
                                                                           \n\
&builtin %t = true                                                         \n\
         %f = false                                                        \n\
                                                                           \n\
&builtin this                                                              \n\
                                                                           \n\
                                                                           \n\
###  Core                                                                  \n\
$eval                                                                      \n\
  | vars drop1 =                                                           \n\
      -> f                                                                 \n\
        -> [_ @args] (f @args)                                             \n\
  | ()                                                                     \n\
                                                                           \n\
$eval                                                                      \n\
  | vars $macs                                                             \n\
  | ('$macs).&macro <= drop1 -> @args                                      \n\
      '$eval                                                               \n\
         | vars ,@(args.&map -> [x] x)                                     \n\
         | ,@(args.&map -> [x y] '('x).&macro <= drop1 y)                  \n\
         | ()                                                              \n\
  | ()                                                                     \n\
                                                                           \n\
$macs                                                                      \n\
  $mac -> n f                                                              \n\
    '$macs: n f                                                            \n\
                                                                           \n\
$mac $run -> @body                                                         \n\
  '$eval                                                                   \n\
     | ,@body                                                              \n\
     | ()                                                                  \n\
                                                                           \n\
$mac var -> @args                                                          \n\
  '| ,@(args.&map -> x 'vars x)                                            \n\
                                                                           \n\
$mac $var -> @args                                                         \n\
  '$run                                                                    \n\
     var ,@args                                                            \n\
                                                                           \n\
# TODO partial scope, if it isn't too hard to add in                       \n\
$mac w/var -> @args body                                                   \n\
  'w/new-scope                                                             \n\
     | var ,@args                                                          \n\
     | body                                                                \n\
                                                                           \n\
$mac w/uniq -> @args body                                                  \n\
  'w/var ,@(args.&map -> x 'x = uniq;)                                     \n\
     body                                                                  \n\
                                                                           \n\
$mac w/complex -> x body                                                   \n\
  w/uniq u v                                                               \n\
    'if: &Array.&isArray x                                                 \n\
       w/uniq u v                                                          \n\
         w/var v = x                                                       \n\
               x = u                                                       \n\
           'w/var u = v                                                    \n\
              ,body                                                        \n\
       w/new-scope                                                         \n\
         body                                                              \n\
                                                                           \n\
$mac defs -> @args                                                         \n\
  '| var ,@(args.&map -> [x] x)                                            \n\
   | ,@(args.&map -> [x y] 'x <= y)                                        \n\
                                                                           \n\
$mac def -> n v                                                            \n\
  'defs: n v                                                               \n\
                                                                           \n\
                                                                           \n\
###  Miscellaneous                                                         \n\
# Dynamic box binding. Usage:                                              \n\
#   var foo = 5                                                            \n\
#   w/var! foo = 10                                                        \n\
#     foo                                                                  \n\
#   foo                                                                    \n\
$mac w/var! -> [('\\=) x y] body                                           \n\
  w/uniq u                                                                 \n\
    'w/var u = x                                                           \n\
       | x <= y                                                            \n\
       | finally                                                           \n\
           body                                                            \n\
           x <= u                                                          \n\
                                                                           \n\
$mac w/dict! -> args body                                                  \n\
  'w/var! args = &Object.&create args                                      \n\
     body                                                                  \n\
                                                                           \n\
$mac words -> @args                                                        \n\
  '[,@(args.&map -> x \"@x\")]                                             \n\
                                                                           \n\
$mac str -> @args                                                          \n\
  '\\\" ,@args                                                             \n\
                                                                           \n\
$mac \\|= -> @args                                                         \n\
  w/var r = []                                                             \n\
    | args.&reduce -> x y                                                  \n\
        | r.&push [x y]                                                    \n\
        | x                                                                \n\
    | '\\|| ,@(r.&map -> x '\\== ,@x)                                      \n\
                                                                           \n\
$mac for -> init test incr body                                            \n\
  'w/new-scope                                                             \n\
     | init                                                                \n\
     | while test                                                          \n\
         | body                                                            \n\
         | incr                                                            \n\
                                                                           \n\
$mac w/each -> [('\\=) x y] body                                           \n\
  w/uniq i len                                                             \n\
    w/complex y                                                            \n\
      'w/var len = y.&length                                               \n\
         for (var i = 0) (~= i len) (++ i)                                 \n\
           w/var x = y[i]                                                  \n\
             body                                                          \n\
                                                                           \n\
$mac w/each-dict -> k [('\\=) v x] body                                    \n\
  w/uniq u                                                                 \n\
    w/complex x                                                            \n\
      'w/each u = &Object.&keys x                                          \n\
         w/var k = u                                                       \n\
               v = x[u]                                                    \n\
           body                                                            \n\
                                                                           \n\
$mac w/each-rev -> [('\\=) x y] body                                       \n\
  w/uniq i                                                                 \n\
    w/complex y                                                            \n\
      'w/var i = y.length                                                  \n\
         while i                                                           \n\
           w/var x = y[-- i]                                               \n\
             body                                                          \n\
                                                                           \n\
$mac w/map -> [('\\=) x y] body                                            \n\
  w/uniq u                                                                 \n\
    'w/var u = []                                                          \n\
       | w/each x = y                                                      \n\
           u.&push body                                                    \n\
       | u                                                                 \n\
                                                                           \n\
$mac $pattern! -> n f                                                      \n\
  '$run                                                                    \n\
     ('n).&pattern <= drop1 f                                              \n\
                                                                           \n\
$mac $get! -> n get                                                        \n\
  '$run                                                                    \n\
     ('n).&get <= drop1 get                                                \n\
                                                                           \n\
$mac $set! -> n set                                                        \n\
  '$run                                                                    \n\
     ('n).&set <= drop1 set                                                \n\
                                                                           \n\
$mac alias -> [('\\=) x y]                                                 \n\
  w/uniq u                                                                 \n\
    '| $var x                                                              \n\
     | $get! x -> 'y                                                       \n\
     | $set! x -> u 'y <= u                                                \n\
                                                                           \n\
alias prn = &console.&log                                                  \n\
                                                                           \n\
                                                                           \n\
$mac re -> x                                                               \n\
  'new RegExp x                                                            \n\
                                                                           \n\
$mac re-replace -> in r y                                                  \n\
  'in.replace (re r) y                                                     \n\
                                                                           \n\
                                                                           \n\
$mac has -> x y                                                            \n\
  '~= (x.&indexOf y) (- 1)                                                 \n\
                                                                           \n\
                                                                           \n\
###  Syntax                                                                \n\
# TODO                                                                     \n\
$mac $syntax-protect -> body                                               \n\
  '$run                                                                    \n\
     w/dict! syntax-rules                                                  \n\
       body                                                                \n\
                                                                           \n\
$mac $syntax-rule -> s o                                                   \n\
  '$run                                                                    \n\
     syntax-rules[s] <= o                                                  \n\
                                                                           \n\
$mac $syntax-helper -> n f                                                 \n\
  w/uniq s i o u                                                           \n\
    '$mac n -> s (i = 0) (o = '{})                                         \n\
       w/uniq u  # TODO figure out a way to remove this                    \n\
         '$run                                                             \n\
            # TODO: make it work with w/complex somehow?                   \n\
            w/box u = o                                                    \n\
              | u.priority <= i                                            \n\
              | u.parse <= f                                               \n\
              | $syntax-rule s u                                           \n\
                                                                           \n\
#| TODO: Fix this                                                          \n\
| [].foo <= 10                                                             \n\
| [].bar <= 20                                                             \n\
|#                                                                         \n\
                                                                           \n\
$run                                                                       \n\
  def syntax-infix -> [@l x] s [y @r]                                      \n\
    if: null? x                                                            \n\
      ',@l (s y) ,@r                                                       \n\
      if: null? y                                                          \n\
        &error s \"missing expression on the right side of @s\"            \n\
        ',@l (s x y) ,@r                                                   \n\
  $syntax-helper $syntax-infix syntax-infix                                \n\
                                                                           \n\
$run                                                                       \n\
  def syntax-unary -> l s [y @r]                                           \n\
    ',@l (s y) ,@r                                                         \n\
  $syntax-helper $syntax-unary syntax-unary                                \n\
")
