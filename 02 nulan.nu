#|(var quote (vau ~ {x} x))

(var fn
  (vau e x
    (add (eval e {vau ~ @x})
      %fn %t)))

(var let
  (vau e {x y @body}
    (eval e {{fn {x} @body} y})))

(var &get
  (vau e {x}
    (& eval x)))

(var &fn
  (vau e {x}
    (& wrap-fn (&get x))))
|#

(var set!
  (vau e {n v}
    (set-box! (get (unbox e) n)
              (eval e v))))

(var def
  (vau e {n v}
    (eval e {do {var n %f}
                {set! n v}})))

(def fn
  (vau e x
    (add (eval e (list* vau {ignore} x))
      %fn %t)))

(def quote
  (add (vau ~ {x} x)
    %pattern-match (vau e {~ f {p} v}
                     (f e p v))))

(def set-in!
  (fn {b n v}
    (set-box! b (unbox (pattern-match b n v)))
    v))

(def uniqs
  (vau e {x}
    (set-in! e x (make-uniq x))))

(def not
  (fn {x}
    (if x %f %t)))

(def isnt
  (fn {x y}
    (not (is x y))))

(def and
  [ %pattern-match (vau e {~ f p v}
                     (foldl e p -> e x (f e x v))) ])

(var {a (a 5)} )

(def or
  [ %pattern-match (vau e {~ f p v}
                     (any p -> x
                       (on-error (f e x v)
                         %pattern-match -> ~ %f))) ])

(def o
  [ %pattern-match (vau e {~ f {p o} v}
                     (if v
                       (f e p v)
                       (f e p (eval e o)))) ])
#|

#(f (add e s v) p v)

(var (as a {b c d}) {1 2 3})

(var (and s {d e f}) {1 2 3})

(var (or (dict "foo" a "bar" b) {a b}) {1 2 3})

(var (or {a} {a b}) {1 2})


> (var {a (o b 5)} {1})
a -> 1
b -> 5

> (var {a (o b 5)} {1 2})
a -> 1
b -> 2


> (var (o a 5) 2)
a -> 2

> (var (o a 5) %f)
a -> 5
|#
