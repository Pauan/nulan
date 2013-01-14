NULAN.eval("                                                               \n\
&builtin! alert window document                                            \n\
                                                                           \n\
$mac element -> x @args                                                    \n\
  w/uniq u                                                                 \n\
    w/box r = {}                                                           \n\
      | w/each {x @y} = args                                               \n\
          if (sym== x \"style\")                                           \n\
            w/each {x y} = y                                               \n\
              r.push 'u.style[,\"@x\"] <= y                                \n\
            if (sym== x \"on\")                                            \n\
              r.push 'u.add-event-listener ,y.0 ,y.1 %t                    \n\
              &error x \"invalid option @x\"                               \n\
      | 'w/box u = document.create-element x                               \n\
           | ,@r                                                           \n\
           | u                                                             \n\
                                                                           \n\
$mac div -> @args                                                          \n\
  'document.body.append-child: element \"div\" ,@args                      \n\
")
