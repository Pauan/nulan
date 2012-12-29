NULAN.eval("                                                               \n\
###  Core                                                                  \n\
$eval                                                                      \n\
  | box $run = make-macro -> {_ x}                                         \n\
                 &compile '$eval                                           \n\
                             | x                                           \n\
                             | ()                                          \n\
  | ()                                                                     \n\
                                                                           \n\
# TODO partial scope, if it isn't too hard to add in                       \n\
$run                                                                       \n\
  box w/box = make-macro -> {_ @args body}                                 \n\
                &compile 'w/new-scope                                      \n\
                            | box ,@args                                   \n\
                            | body                                         \n\
                                                                           \n\
$run                                                                       \n\
  box make-macro-wrapper =                                                 \n\
    # TODO                                                                 \n\
    -> f                                                                   \n\
      make-macro -> {_ @args}                                              \n\
        &compile (f @args)                                                 \n\
                                                                           \n\
$run                                                                       \n\
  box $mac = make-macro -> {_ n f}                                         \n\
               &compile '$run                                              \n\
                           | box n                                         \n\
                           | n <= make-macro-wrapper f                     \n\
                                                                           \n\
#|                                                                         \n\
$run                                                                       \n\
  w/box u    = make-uniq;                                                  \n\
        args = make-uniq;                                                  \n\
    console.log u args                                                     \n\
                                                                           \n\
$run                                                                       \n\
  box $mac = make-macro -> {_ n f}                                         \n\
               w/box u    = make-uniq;                                     \n\
                     args = make-uniq;                                     \n\
                 &compile '$run                                            \n\
                             | box n                                       \n\
                             | n <= w/box u = f                            \n\
                                      make-macro -> {_ @args}              \n\
                                        &compile (u @args)                 \n\
|#                                                                         \n\
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
$mac defs -> @body                                                         \n\
  '| box ,@(body.map -> {x} x)                                             \n\
   | ,@(body.map -> {x y} 'x <= y)                                         \n\
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
$mac prn -> @args                                                          \n\
  'console.log ,@args                                                      \n\
                                                                           \n\
                                                                           \n\
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
###  Syntax                                                                \n\
# TODO                                                                     \n\
$mac $syntax-protect -> body                                               \n\
  '$run                                                                    \n\
     w/dict! syntax-rules                                                  \n\
       body                                                                \n\
                                                                           \n\
$mac $syntax-rule -> s o                                                   \n\
  '$run                                                                    \n\
     syntax-rules;[s] <= o                                                 \n\
                                                                           \n\
$mac $syntax-helper -> n f                                                 \n\
  w/uniq s i o u                                                           \n\
    '$mac n -> s (i = 0) (o = '[])                                         \n\
       '$run                                                               \n\
          # TODO: make it work with w/complex somehow?                     \n\
          w/box u = o                                                      \n\
            | u.priority <= i                                              \n\
            | u.parse <= f                                                 \n\
            | $syntax-rule s u                                             \n\
                                                                           \n\
#| TODO: Fix this                                                          \n\
| [].foo <= 10                                                             \n\
| [].bar <= 20                                                             \n\
|#                                                                         \n\
                                                                           \n\
$run                                                                       \n\
  | def syntax-infix -> {@l x} s {y @r}                                    \n\
      if: null? x                                                          \n\
        ',@l (s y) ,@r                                                     \n\
        if: null? y                                                        \n\
          &error s \"missing expression on the right side of @s\"          \n\
          ',@l (s x y) ,@r                                                 \n\
  | $syntax-helper $syntax-infix syntax-infix                              \n\
                                                                           \n\
$run                                                                       \n\
  | def syntax-unary -> l s {y @r}                                         \n\
      ',@l (s y) ,@r                                                       \n\
  | $syntax-helper $syntax-unary syntax-unary                              \n\
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
                                                                           \n\
                                                                           \n\
                                                                           \n\
#|                                                                         \n\
(mac complex (x body)                                                      \n\
  (w/uniq (u v)                                                            \n\
    `(if (isa ,x 'sym)                                                     \n\
         ,body                                                             \n\
         (w/uniq (,u ,v)                                                   \n\
           (withs (,v ,x                                                   \n\
                   ,x ,u)                                                  \n\
              `(let ,,u ,,v                                                \n\
                 ,,body))))))                                              \n\
                                                                           \n\
mac uniqs -> @args body                                                    \n\
  '| box ,@(args.map -> x 'x = uniq;)                                      \n\
   | body                                                                  \n\
                                                                           \n\
mac vars -> @args                                                          \n\
  '(|) ,@(args.map -> x 'var x)                                            \n\
                                                                           \n\
mac w/vars -> @args                                                        \n\
  args.reduceRight -> x y                                                  \n\
    'w/var y x                                                             \n\
                                                                           \n\
mac w/def -> x '(| f @body)                                                \n\
  'w/var x                                                                 \n\
     (|) (x <= f) ,@body                                                   \n\
                                                                           \n\
w/def foo                                                                  \n\
  | -> a (a + 2)                                                           \n\
      foo                                                                  \n\
  | foobar                                                                 \n\
                                                                           \n\
w/def foo ((|) bar 1 2 3)                                                  \n\
                                                                           \n\
$run                                                                       \n\
  w/var y = {1 2 3}                                                        \n\
    w/complex y y                                                          \n\
                                                                           \n\
w/uniq u v                                                                 \n\
  w/var v = y                                                              \n\
        y = u                                                              \n\
    'w/var u = v                                                           \n\
      y                                                                    \n\
|#                                                                         \n\
                                                                           \n\
#|                                                                         \n\
[ foo -> 10                                                                \n\
| bar -> 20                                                                \n\
| qux -> 30 ]                                                              \n\
                                                                           \n\
| foo | bar | qux                                                          \n\
                                                                           \n\
                                                                           \n\
let u {}                                                                   \n\
  | foo 1                                                                  \n\
  | bar 5                                                                  \n\
  | qux 10                                                                 \n\
                                                                           \n\
                                                                           \n\
|||| foo | bar ||| qux                                                     \n\
                                                                           \n\
                                                                           \n\
-> a a | 5                                                                 \n\
                                                                           \n\
                                                                           \n\
{ -> a b c | -> a b c | -> a b c }                                         \n\
                                                                           \n\
                                                                           \n\
foo bar @qux                                                               \n\
                                                                           \n\
'foo bar @qux                                                              \n\
                                                                           \n\
''foo bar @qux                                                             \n\
                                                                           \n\
'''foo bar @qux                                                            \n\
                                                                           \n\
                                                                           \n\
'foo ,bar ,@qux                                                            \n\
                                                                           \n\
''foo ,bar ,@qux                                                           \n\
                                                                           \n\
''foo ,,bar ,,@qux                                                         \n\
                                                                           \n\
                                                                           \n\
if qux corge                                                               \n\
  | a                                                                      \n\
  | b                                                                      \n\
  if foo bar                                                               \n\
    | d                                                                    \n\
    | e                                                                    \n\
    if nou yesno                                                           \n\
      | f                                                                  \n\
      | g                                                                  \n\
      h                                                                    \n\
                                                                           \n\
if qux corge                                                               \n\
  | a                                                                      \n\
  | b                                                                      \n\
  foo bar                                                                  \n\
  | d                                                                      \n\
  | e                                                                      \n\
  nou yesno                                                                \n\
  | f                                                                      \n\
  | g                                                                      \n\
  h                                                                        \n\
|#                                                                         \n\
")
