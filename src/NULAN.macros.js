NULAN.eval("                                                               \n\
###  Core                                                                  \n\
$eval                                                                      \n\
  | box &make-macro =                                                      \n\
      # TODO                                                               \n\
      -> f                                                                 \n\
        -> {_ @args}                                                       \n\
          &compile (f @args)                                               \n\
  | ()                                                                     \n\
                                                                           \n\
$eval                                                                      \n\
  | box $macs                                                              \n\
  | ('$macs).&macro <= &make-macro -> @args                                \n\
      '$eval                                                               \n\
         | box ,@(args.map -> {x} x)                                       \n\
         | ,@(args.map -> {x y} '('x).&macro <= &make-macro y)             \n\
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
# TODO partial scope, if it isn't too hard to add in                       \n\
$mac w/box -> @args body                                                   \n\
  'w/new-scope                                                             \n\
     | box ,@args                                                          \n\
     | body                                                                \n\
                                                                           \n\
$mac &builtin! -> @args                                                    \n\
  '| external! ,@args                                                      \n\
   | $run                                                                  \n\
       external! ,@args                                                    \n\
                                                                           \n\
&builtin! Number Math Boolean TypeError String Int16Array Float32Array isFinite Array DataView Float64Array ReferenceError SyntaxError Int32Array Uint16Array clearTimeout decodeURIComponent Uint32Array setTimeout eval console URIError unescape Date escape encodeURI Error Int8Array EvalError RangeError NaN isNaN parseInt undefined Object Uint8ClampedArray parseFloat Uint8Array clearInterval Infinity JSON Function setInterval encodeURIComponent decodeURI ArrayBuffer RegExp\n\
                                                                           \n\
&builtin! %t = true                                                        \n\
          %f = false                                                       \n\
                                                                           \n\
&builtin! this                                                             \n\
                                                                           \n\
                                                                           \n\
###  Macro utilities                                                       \n\
$mac w/uniq -> @args body                                                  \n\
  'w/box ,@(args.map -> x 'x = make-uniq;)                                 \n\
     body                                                                  \n\
                                                                           \n\
$mac w/complex -> x body                                                   \n\
  w/uniq u v                                                               \n\
    'if: Array.isArray x                                                   \n\
       w/uniq u v                                                          \n\
         w/box v = x                                                       \n\
               x = u                                                       \n\
           'w/box u = v                                                    \n\
              ,body                                                        \n\
       w/new-scope                                                         \n\
         body                                                              \n\
                                                                           \n\
                                                                           \n\
###  Boxes                                                                 \n\
$mac defs -> @args                                                         \n\
  '| box ,@(args.map -> {x} x)                                             \n\
   | ,@(args.map -> {x y} 'x <= y)                                         \n\
                                                                           \n\
#|                                                                         \n\
# TODO: define def in terms of defs?                                       \n\
$mac def -> n v                                                            \n\
  '| box n                                                                 \n\
   | n <= v|#                                                              \n\
                                                                           \n\
$mac def -> n v                                                            \n\
  'defs: n v                                                               \n\
                                                                           \n\
# Dynamic box binding. Usage:                                              \n\
#   box foo = 5                                                            \n\
#   w/box! foo = 10                                                        \n\
#     foo                                                                  \n\
#   foo                                                                    \n\
$mac w/box! -> {('=) x y} body                                             \n\
  w/uniq u                                                                 \n\
    'w/box u = x                                                           \n\
       | x <= y                                                            \n\
         # TODO: should finally use w/new-scope?                           \n\
       | finally body                                                      \n\
           x <= u                                                          \n\
                                                                           \n\
$mac w/dict! -> args body                                                  \n\
  'w/box! args = Object.create args                                        \n\
     body                                                                  \n\
                                                                           \n\
                                                                           \n\
###  Miscellaneous                                                         \n\
$mac w/expression -> x y                                                   \n\
  '| x                                                                     \n\
   | y                                                                     \n\
                                                                           \n\
#|                                                                         \n\
$mac str -> @args                                                          \n\
  '(\") ,@args                                                             \n\
|#                                                                         \n\
                                                                           \n\
$mac words -> @args                                                        \n\
  '{,@(args.map -> x \"@x\")}                                              \n\
                                                                           \n\
#|                                                                         \n\
$mac call -> x y @args                                                     \n\
  'x[y].call ,@args                                                        \n\
                                                                           \n\
$mac call-own -> x y @args                                                 \n\
  'x[y] ,@args                                                             \n\
|#                                                                         \n\
                                                                           \n\
$mac |= -> @args                                                           \n\
  w/box r = {}                                                             \n\
    | args.reduce -> x y                                                   \n\
        | r.push {x y}                                                     \n\
        | x                                                                \n\
    | '|| ,@(r.map -> x '== ,@x)                                           \n\
                                                                           \n\
$mac ~= -> @args                                                           \n\
     # TODO                                                                \n\
  if (== args.length 2)                                                    \n\
    # TODO                                                                 \n\
    '& \"!==\" ,args.0 ,args.1                                             \n\
       # TODO                                                              \n\
    '~ (== ,@args)                                                         \n\
                                                                           \n\
                                                                           \n\
###  Loops                                                                 \n\
$mac for -> init test incr body                                            \n\
  'w/new-scope                                                             \n\
     | init                                                                \n\
     | while test                                                          \n\
         | body                                                            \n\
         | incr                                                            \n\
                                                                           \n\
$mac w/each -> {('=) x y} body                                             \n\
  w/uniq i len                                                             \n\
    w/complex y                                                            \n\
      'w/box len = y.length                                                \n\
         for (box i = 0) (~= i len) (++ i)                                 \n\
           w/box x = y[i]                                                  \n\
             body                                                          \n\
                                                                           \n\
$mac w/each-dict -> k {('=) v x} body                                      \n\
  w/uniq u                                                                 \n\
    w/complex x                                                            \n\
      'w/each u = Object.keys x                                            \n\
         w/box k = u                                                       \n\
               v = x[u]                                                    \n\
           body                                                            \n\
                                                                           \n\
$mac w/each-rev -> {('=) x y} body                                         \n\
  w/uniq i                                                                 \n\
    w/complex y                                                            \n\
      'w/box i = y.length                                                  \n\
         while i                                                           \n\
           w/box x = y[-- i]                                               \n\
             body                                                          \n\
                                                                           \n\
$mac w/map -> {('=) x y} body                                              \n\
  w/uniq u                                                                 \n\
    'w/box u = {}                                                          \n\
       | w/each x = y                                                      \n\
           u.push body                                                     \n\
       | u                                                                 \n\
                                                                           \n\
                                                                           \n\
###  TODO                                                                  \n\
$mac $pattern -> n f                                                       \n\
  '$run                                                                    \n\
     box n                                                                 \n\
     ('n).&pattern <= &make-macro f                                        \n\
                                                                           \n\
$mac $getset -> n get set                                                  \n\
  '$run                                                                    \n\
     box n                                                                 \n\
     ('n).&get <= &make-macro get                                          \n\
     ('n).&set <= &make-macro set                                          \n\
                                                                           \n\
$mac alias -> {('=) x y}                                                   \n\
  w/uniq u                                                                 \n\
    '$getset x                                                             \n\
       -> 'y                                                               \n\
       -> u 'y <= u                                                        \n\
                                                                           \n\
alias prn = console.log                                                    \n\
#|                                                                         \n\
$mac prn -> @args                                                          \n\
  'console.log ,@args                                                      \n\
|#                                                                         \n\
                                                                           \n\
                                                                           \n\
# TODO: these should be in the \"Boxes\" category                          \n\
$mac w/include -> @args body                                               \n\
  '| box {,@args} = w/new-scope                                            \n\
                      | body                                               \n\
                      | {,@args}                                           \n\
   | ()                                                                    \n\
                                                                           \n\
$mac w/exclude -> @args body                                               \n\
  w/box l = {}                                                             \n\
        r = {}                                                             \n\
    | w/each x = args                                                      \n\
        if: bound? x                                                       \n\
          w/uniq u                                                         \n\
            | l.push 'box u = x                                            \n\
            | r.push 'box x = u                                            \n\
          r.push 'del x                                                    \n\
    | '| ,@l                                                               \n\
       | body                                                              \n\
       | ,@r                                                               \n\
       | ()                                                                \n\
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
  '~= (x.index-of y) (- 1)                                                 \n\
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
    '$mac n -> s (i = 0) (o = '[])                                         \n\
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
  def syntax-infix -> {@l x} s {y @r}                                      \n\
    if: null? x                                                            \n\
      ',@l (s y) ,@r                                                       \n\
      if: null? y                                                          \n\
        &error s \"missing expression on the right side of @s\"            \n\
        ',@l (s x y) ,@r                                                   \n\
  $syntax-helper $syntax-infix syntax-infix                                \n\
                                                                           \n\
$run                                                                       \n\
  def syntax-unary -> l s {y @r}                                           \n\
    ',@l (s y) ,@r                                                         \n\
  $syntax-helper $syntax-unary syntax-unary                                \n\
                                                                           \n\
                                                                           \n\
$syntax-unary \"~\"  80                                                    \n\
                                                                           \n\
$syntax-infix \"*\"  70                                                    \n\
$syntax-infix \"/\"  70                                                    \n\
                                                                           \n\
$syntax-infix \"+\"  60                                                    \n\
$syntax-infix \"-\"  60                                                    \n\
                                                                           \n\
$syntax-infix \"<\"  50                                                    \n\
$syntax-infix \"=<\" 50                                                    \n\
$syntax-infix \">\"  50                                                    \n\
$syntax-infix \">=\" 50                                                    \n\
                                                                           \n\
$syntax-infix \"==\" 40                                                    \n\
$syntax-infix \"|=\" 40                                                    \n\
$syntax-infix \"~=\" 40                                                    \n\
                                                                           \n\
$syntax-infix \"&&\" 30                                                    \n\
                                                                           \n\
$syntax-infix \"||\" 20                                                    \n\
")
