=> ($set! foo ($vau Env [X] ($vau Env [Y] [X Y])))
=> ($set! bar (foo 5))
=> (bar 10)
   [5 10]

=> ((foo 20) 10)
   [20 10]

=> (bar 10)
   [5 10]
