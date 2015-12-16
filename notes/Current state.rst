(TYPE (Foo a))

(TYPE Color
  *red
  *orange
  *yellow
  *green
  *blue
  *indigo
  *violet)

(TYPE RGB
  (*rgba Integer Integer Integer))

(TYPE (List a)
  *empty
  (*value a (List a)))

(PROTOCOL ($transform a)
  transform :: (-> (a b) (-> b c) (a c)))

(FUNCTION after :: (REQUIRE ($transform a) ($flatten a)
                     (-> (a b) (-> b (a c)) (a c)))
  (after a fn)
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
  | (IMPLEMENT ($flatten Maybe) flatten)
  | (IMPLEMENT ($transform Maybe) transform)
  | (IMPLEMENT ($wrap Maybe) wrap))

(EXPORT
  | d <= a
  | b
  | c
  | Maybe
  | *some
  | *none
  | ($flatten Maybe)
  | ($transform Maybe)
  | ($wrap Maybe))

(IMPORT (nulan "ffi")
  | UNSAFE-FFI-IMPORT
  | javascript)

(WITH-TARGET javascript
  (UNSAFE-FFI-LOAD "foo/bar"
    a :: (-> Integer Integer Integer)
    b :: Integer
    c :: Integer
    d :: (Foo Integer)))

(DEFINE foo :: (-> (-> Integer Integer Integer) Integer)
  (foo a)
    (a 1 2))

(DEFINE bar :: (-> Integer Integer)
  (bar 1)
    2
  (bar a)
    (+ (bar 1) a))

(DEFINE-ALIAS
  (QUX @a)
    &(+ ~@a))

(DEFINE-ALIAS
  FOO
    &(BAR 1 2 3 4 5)

  (BAR a @b)
    (MATCH a
      &~n <= ~v
        &(QUX ~n ~v ~@b)
      v
        &(QUX 1 ~v ~@b)))

(foo -> a b (+ a b))
(foo (-> a b (+ a b)))
FOO

(UNSAFE-DEFINE-RULE add :: (-> Integer Integer Integer)
  (add a b)
    (ADD a b))

(DO a <= a
    b <= b
    c)

(DO x <= (read-file "foo")
    (log x)
    (write-file "bar" x))

(DEFINE
  a = 1
  b = 2)

(LET a = 1
     b = 2
  c)

(MATCHES [a b c]
  [1 2 3]
    1
  [1 2 a]
    2
  [1 a b]
    3
  [a b c]
    4)

(MATCH a
  _
    1
  a
    2
  1
    3
  "foo"
    4
  [a b c]
    5
  { a b c }
    6
  { a <= b | c <= d }
    { b <= a | d <= c }
  (*foo 1)
    8
  (-> view a)
    9)

# Unsure
(METADATA impure run-time-only compile-time-only inline-function synchronous
  ...)
