#|(var quote (vau ~ {x} x))

(var fn
  (vau e x
    (add (eval e {vau (quote ~) @x})
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

(def quote (vau ~ {x} x))

(def fn
  (vau e x
    (add (eval e (list* vau (quote ~) x))
      %fn %t)))
