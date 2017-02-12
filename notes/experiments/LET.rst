(REWRITE-RULE
| (LET ~var <= ~value ~body)
    &((-> ~var ~body) ~value))


(REWRITE-RULE
| (LET ~@a ~body)
    (LET | b     <= (pair a)
         | left  <= (transform b -> (*pair left _) left)
         | right <= (transform b -> (*pair _ right) right)
      &((-> ~@left ~body) ~@right)))


(LET a <= 1
     b <= 2
  (+ a b))

(LET a <= 1
   | b <= 2
  (+ a b))

(LET a <= 1
     b <= 2
| (+ a b))

(LET
  | a <= 1
  | b <= 2
  (+ a b))

(LET
| a <= 1
| b <= 2
  (+ a b))

(LET
| a <= 1
| b <= 2
    (+ a b))

(LET
| a <= 1
| b <= 2
| (+ a b))

(LET | a <= 1
     | b <= 2
  (+ a b))

(LET { a <= 1
     | b <= 2 }
  (+ a b))

(LET [ a <= 1
     | b <= 2 ]
  (+ a b))

(LET ( a <= 1
     | b <= 2 )
  (+ a b))

(LOCAL-CONSTANT
| a <= 1
| b <= 2
  (+ a b))
