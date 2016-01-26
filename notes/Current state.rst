(TYPE (Foo a))

(TYPE Color
| *red
| *orange
| *yellow
| *green
| *blue
| *indigo
| *violet)

(TYPE RGB
| (*rgba Integer Integer Integer))

(TYPE (List a)
| *empty
| (*value a (List a)))

(PROTOCOL ($transform a)
| transform :: (-> (a b) (-> b c) (a c)))

(CONSTANT
| after :: (REQUIRE ($transform a) ($flatten a)
             (-> (a b) (-> b (a c)) (a c)))
| after <= (-> a fn
             (flatten (transform a fn))))

(FUNCTION after :: (REQUIRE ($transform a) ($flatten a)
                     (-> (a b) (-> b (a c)) (a c)))
| (after a fn)
    (flatten (transform a fn)))

(IMPORT (github { name <= "nulan/flatten"
                | file <= "src/flatten.nul"
                | version <= (major 2) })
| $flatten
| flatten)

(IMPORT (file "foo/bar")
| a <= d
| b
| c
| Maybe
| *some
| *none
| (PROVIDE ($flatten Maybe)
  | flatten)
| (PROVIDE ($transform Maybe)
  | transform <= map)
| (PROVIDE ($yield Maybe)
  | yield))

(PROVIDE ($transform Maybe)
| transform <= map)

(PROVIDE ($transform Maybe)
| transform <= (-> a a))

(EXPORT
| d <= a
| b
| c
| Maybe
| *some
| *none)

(IF test
| then
| else)

(EXPORT-CONSTANT
| foo <= *foo)

(IMPORT-BUILTINS)

(IMPORT (nulan "unsafe/ffi")
| UNSAFE-FFI-LOAD
| javascript)

(UNSAFE-FFI-LOAD { target <= javascript
                 | file <= "foo/bar" }
| a :: (-> Integer Integer Integer)
| b :: Integer
| c :: Integer
| d :: (Foo Integer))

(CONSTANT
| foo :: (-> (-> Integer Integer Integer) Integer)
| foo <= (-> a (a 1 2)))

(MUTUALLY-RECURSIVE
  (FUNCTION even? :: (-> Integer Boolean)
  | (even? 0)
      true
  | (even? a)
      (odd? (- a 1)))

  (FUNCTION odd? :: (-> Integer Boolean)
  | (odd? 0)
      false
  | (odd? a)
      (even? (- a 1))))

(LET a <= 1
     b <= 2
  (+ a b))

(WITH-LOOP loop
| a <= 1
| b <= 2
  (loop a b))

(FUNCTION foo :: (-> (-> Integer Integer Integer) Integer)
| (foo a)
    (a 1 2))

(FUNCTION foo :: (-> Text Text)
| (foo a)
    a)

(FUNCTION bar :: (-> Integer Integer)
| (bar 1)
    2
| (bar a)
    (+ (bar 1) a))

(REWRITE-RULE
| (QUX ~@a)
    &(+ ~@a))

(MUTUALLY-RECURSIVE
  (REWRITE-RULE
  | (FOO ~n <= ~v)
      &(BAR ~n ~v)
  | (FOO ~v)
      &(BAR ~v))

  (REWRITE-RULE
  | (BAR ~a ~@b)
      (MATCH a
      | &~n <= ~v
          &(QUX ~n ~v ~@b)
      | v
          &(QUX 1 ~v ~@b))))

(foo -> a b (+ a b))
(foo (-> a b (+ a b)))

FOO
(FOO)
((FOO))
(((FOO)))

(MUTUALLY-RECURSIVE
  (REWRITE-RULE
  | (UNSTREAM (STREAM ~a))
      a
  | (UNSTREAM ~a)
      &(unstream ~a))

  (REWRITE-RULE
  | (STREAM (UNSTREAM ~a))
      a
  | (STREAM ~a)
      &(stream ~a)))

(DO a <= a
    b <= b
    c)

(DO x <= (read-file "foo")
    (log x)
    (write-file "bar" x)
    (yield null))

(TRANSFORM a <= 1
           b <= 2
           c <= 3
  (+ a b c))

(MATCHES [ a b c ]
| [ 1 2 3 ]
    1
| [ 1 2 a ]
    2
| [ 1 a b ]
    3
| [ a b c ]
    4)

[ 1 2 3 ]

[ 1
| 2
| 3 ]

{ a b }

{ a <= 1 | b <= 2 }

{ a <= 1
| b <= 2 }

(MATCH a
| _
    1
| a
    2
| 1
    3
| "foo"
    4
| { a b c }
    5
| { a <= b | c <= d }
    { b <= a | d <= c }
| (*foo 1)
    6)


# Unsure
(PRAGMA { phase <= run-time
        | target <= javascript }
  foo)

(METADATA impure inline-function synchronous
  foo)

(INLINE
  (-> foo bar))

(MATCH a
| (-> view a)
    9)

(MATCH a
| (LET a <= a (equal? a 1))
    9)

(IMPORT (nulan "unsafe")
| UNSAFE-OPTIMIZATION-RULE)

(UNSAFE-OPTIMIZATION-RULE
| (after a b)
    (flatten (transform a b)))

(UNSAFE-OPTIMIZATION-RULE
| (reduce-left [] a -> b c (push b d))
    (reduce-left [] a -> b c (unsafe-push! b d)))

(UNSAFE-OPTIMIZATION-RULE
| (unstream (stream a))
    a)

(UNSAFE-OPTIMIZATION-RULE
| (stream (unstream a))
    a)

(UNSAFE-OPTIMIZATION-RULE
| (add a b)
    (ADD a b))
