NULAN.eval("                                                               \n\
builtin Buffer global GLOBAL process root require module                   \n\
                                                                           \n\
$mac delay -> body                                                         \n\
  'process.nextTick -> body                                                \n\
                                                                           \n\
$mac cwd -> x                                                              \n\
  if: null? x                                                              \n\
    'process.cwd;                                                          \n\
    'w/expression                                                          \n\
       process.chdir x                                                     \n\
       cwd;                                                                \n\
                                                                           \n\
$mac w/cwd! -> x body                                                      \n\
  w/uniq u                                                                 \n\
    'w/var u = cwd;                                                        \n\
       | cwd x                                                             \n\
       | finally body                                                      \n\
           cwd u                                                           \n\
                                                                           \n\
$mac load-file -> x                                                        \n\
  w/uniq vm fs                                                             \n\
    'w/var vm = require \"vm\"                                             \n\
           fs = require \"fs\"                                             \n\
       vm.runInNewContext (fs.readFileSync x \"utf8\") global              \n\
")
