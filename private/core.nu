$run
  | vars var
  | $mac! var -> x @args
      '| ,@((& map) (-> x 'vars x) [x @args])

  | var $var
  | $mac! $var -> @args
      '$run
         var ,@args

  | var $mac
  | $mac! $mac -> n f
      '| $var n
       | $mac! n f
