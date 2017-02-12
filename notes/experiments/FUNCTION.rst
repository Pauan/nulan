(FUNCTION foo :: (-> a a)
| (foo a)
    a)


(CONSTANT
| foo :: (-> a a)
| foo <= (-> a
           (WITH-LOOP foo
           | a <= a
             (MATCHES [a]
             | [a]
                 a))))


(REWRITE-RULE
| (FUNCTION ~name :: ~type ~@a)
    (LET b <= (pair a)
         c <= (transform b -> _
                (UNIQUE))
         d <= (transform b -> _
                (UNIQUE))
      &(CONSTANT
       | ~name :: ~type
       | ~name <= (-> ~@c
                    (WITH-LOOP ~name
                    ~@(TRANSFORM c <= c
                                 d <= d
                        &| ~d <= ~c)
                      (MATCHES [~@d]
                      ~@(flatten
                          (transform b -> (*pair left right)
                            (MATCH left
                            | &| (~(LET name2 <= name2 (equal? name2 name)) ~@left)
                                [ &| [~@left]
                                | right ])))))))))
