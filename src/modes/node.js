NULAN.eval("                                                               \n\
builtin Buffer global GLOBAL process root require module                   \n\
                                                                           \n\
box {&src @&args} = process.argv                                           \n\
                                                                           \n\
prn &args                                                                  \n\
                                                                           \n\
$mac delay -> body                                                         \n\
  'process.nextTick -> body                                                \n\
                                                                           \n\
$getset cwd                                                                \n\
  -> 'process.cwd;                                                         \n\
  -> x 'process.chdir x                                                    \n\
                                                                           \n\
$mac w/file -> {('=) n v} body                                             \n\
  w/uniq err                                                               \n\
    '(require \"fs\").read-file v \"utf8\" -> err n                        \n\
       | if err                                                            \n\
           throw err                                                       \n\
       | body                                                              \n\
                                                                           \n\
$mac load-file -> x                                                        \n\
  '(require \"vm\").run-in-new-context                                     \n\
     (require \"fs\").read-file-sync x \"utf8\"                            \n\
     global                                                                \n\
                                                                           \n\
$mac w/exec -> {('=) n v} body                                             \n\
  w/uniq err                                                               \n\
    '(require \"child_process\").exec v -> err n                           \n\
       | if err                                                            \n\
           throw err                                                       \n\
       | body                                                              \n\
                                                                           \n\
# TODO: I'm not fond of how it uses w/exec like that                       \n\
$mac exec -> args                                                          \n\
  w/uniq u                                                                 \n\
                # TODO \"@args\"                                           \n\
    'w/exec u = args                                                       \n\
       ()                                                                  \n\
                                                                           \n\
$mac w/stdin -> n body                                                     \n\
  w/uniq u v                                                               \n\
    'w/box u = {}                                                          \n\
       | process.stdin.set-encoding;                                       \n\
       | process.stdin.on \"data\" -> v                                    \n\
           u.push v                                                        \n\
       | process.stdin.on \"end\" ->                                       \n\
           w/box n = u.join \"\"                                           \n\
             body                                                          \n\
       |  process.stdin.resume;                                            \n\
                                                                           \n\
def expandpath -> x                                                        \n\
  (require \"path\").normalize                                             \n\
    re-replace x \"^~/\" ->                                                \n\
      \"@(process.env.HOME)/\"                                             \n\
                                                                           \n\
def abspath -> @args                                                       \n\
  (require \"path\").resolve @:args.map expandpath                         \n\
")

/*

$mac cwd -> x                                                              \n\
  if: null? x                                                              \n\
    'process.cwd;                                                          \n\
    'w/expression                                                          \n\
       process.chdir x                                                     \n\
       cwd;                                                                \n\
                                                                           \n\
$mac w/cwd! -> x body                                                      \n\
  w/uniq u                                                                 \n\
    'w/box u = cwd;                                                        \n\
       | cwd x                                                             \n\
       | finally body                                                      \n\
           cwd u                                                           \n\

*/
