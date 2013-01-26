NULAN.eval("                                                               \n\
external! Buffer global GLOBAL process root require module                 \n\
                                                                           \n\
external! NUIT  # TODO: this shouldn't be in node.js                       \n\
alias nuit-parse = NUIT.parse                                              \n\
alias nuit-serialize = NUIT.serialize                                      \n\
                                                                           \n\
box {_ &exec &script @&args} = process.argv                                \n\
                                                                           \n\
$mac delay -> body                                                         \n\
  'process.nextTick -> body                                                \n\
                                                                           \n\
$getset cwd                                                                \n\
  -> 'process.cwd;                                                         \n\
  -> x 'process.chdir x                                                    \n\
                                                                           \n\
def readfile -> x                                                          \n\
  (require \"fs\").read-file-sync x \"utf8\"                               \n\
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
                                                                           \n\
def expandpath -> x                                                        \n\
  re-replace x \"^~/\" ->                                                  \n\
    \"@(process.env.HOME)/\"                                               \n\
                                                                           \n\
def path -> @args                                                          \n\
  w/box r = {}                                                             \n\
    | w/each x = args                                                      \n\
        w/box x = expandpath x                                             \n\
          if x                                                             \n\
            if x.0 == \"/\"                                                \n\
              r <= {x}                                                     \n\
              r.push x                                                     \n\
    | (require \"path\").normalize: r.join \"/\"                           \n\
                                                                           \n\
def abspath -> @args                                                       \n\
  (require \"fs\").realpath-sync: path @args                               \n\
                                                                           \n\
def dirpath -> x                                                           \n\
  (require \"path\").dirname x                                             \n\
                                                                           \n\
def filepath -> @args                                                      \n\
  (require \"path\").basename @args                                        \n\
                                                                           \n\
def hiddenpath? -> x                                                       \n\
  (filepath x).0 == \".\"                                                  \n\
                                                                           \n\
def extpath -> x                                                           \n\
  ((require \"path\").extname x).slice 1                                   \n\
                                                                           \n\
def namepath -> x                                                          \n\
  re-replace x \"^(.+)\\\\.[^\\\\.]*$\" -> _ s s                           \n\
                                                                           \n\
def shellpath -> x                                                         \n\
  \"'@(re-replace x \"'\" \"\\\\'\")'\"                                    \n\
                                                                           \n\
                                                                           \n\
def dir -> d                                                               \n\
  w/map f = (require \"fs\").readdir-sync d                                \n\
    path d f                                                               \n\
                                                                           \n\
def dir? -> x                                                              \n\
  ((require \"fs\").stat-sync x).is-directory;                             \n\
                                                                           \n\
def file? -> x                                                             \n\
  ((require \"fs\").stat-sync x).is-file;                                  \n\
                                                                           \n\
def dirs -> x f                                                            \n\
  w/box r = {}                                                             \n\
    | def loop -> x                                                        \n\
        w/each s = dir x                                                   \n\
          | if: dir? s                                                     \n\
              if: f s %t                                                   \n\
                loop s                                                     \n\
              if: f s %f                                                   \n\
                r.push s                                                   \n\
          | ()                                                             \n\
    | loop x                                                               \n\
    | r                                                                    \n\
                                                                           \n\
                                                                           \n\
box import-paths = { (dirpath &script)                                     \n\
                     (abspath (dirpath &exec) \"lib\") }                   \n\
                                                                           \n\
# TODO: don't do a linear search?                                          \n\
def import-file -> f files                                                 \n\
  w/box f = f.split \"/\"                                                  \n\
    | w/each x = files                                                     \n\
        if: file? x                                                        \n\
          prn: files.index-of f                                            \n\
    | prn f                                                                \n\
                                                                           \n\
$mac import -> @args                                                       \n\
  w/uniq u d f                                                             \n\
    'w/box u = {}                                                          \n\
       | w/each d = import-paths                                           \n\
           | u.push d                                                      \n\
           | w/each f = dir d                                              \n\
               | prn d f                                                   \n\
               | u.push: abspath d f                                       \n\
       | prn u                                                             \n\
       | ,@(args.map -> x 'import-file ,\"@x\" u)                          \n\
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
