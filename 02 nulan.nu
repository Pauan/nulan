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
    %pattern-match (vau ~ {~ e {p} v}
                     (if (is p v)
                         e
                         (error %pattern-match p " != " v)))))

(def uniqs
  (vau e {x}
    (prn {var x (make-uniq x)})
    (eval e {var x {quote (make-uniq x)}})))
