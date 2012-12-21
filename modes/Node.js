builtin Buffer global GLOBAL process root require module

$mac delay -> body
  'process.nextTick -> body

$mac cwd -> x
  if: null? x
    'process.cwd;
    'w/expression
       process.chdir x
       cwd;

$mac w/cwd! -> x body
  w/uniq u
    'w/var u = cwd;
       | cwd x
       | finally body
           cwd u
