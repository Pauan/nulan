#=============================================================================
#  Closures
#=============================================================================
=> $defn foo; X -> Y -> [X Y]
   (&fn foo)

=> $def bar: foo 5
   (&fn)

=> bar 10
   [5 10]

=> ((foo 20) 10)
   [20 10]

=> bar 10
   [5 10]

#=============================================================================
#  Bar
#=============================================================================
=> $set! foo 5
   5

=> [1 2 | 5]
   [1 2 | 5]

=> [1 2 | foo]
   [1 2 | 5]


=> $set! foo [add 1 2 3]
   [(&fn add) 1 2 3]

=> $set! bar ($quote (add 1 2 3))
   (add 1 2 3)

=> [1 2 | foo]
   [1 2 (&fn add) 1 2 3]

=> [1 2 | [add 1 2 3]]
   [1 2 (&fn add) 1 2 3]

=> [1 2 | bar]
   [1 2 add 1 2 3]

=> [1 2 | ($quote (add 1 2 3))]
   [1 2 add 1 2 3]


# TODO: SOLUTION: USE UNWRAP
# TODO: SOLUTION: SPLIT &list INTO &list and &apply-list
# TODO: SOLUTION: IMPLICIT [] AT THE END OF EVERY &list SO THAT [1 2 3] BECOMES (&list 1 2 3 [])


=> [1 2 | (add 1 2 3)]
   [1 2 | 6]

=> [1 2 | ($quote ((add 1 2 3)))]
   [1 2 ((&fn add) 1 2 3)]


=> ($let (a 5) | [($quote a)])
   5
