#lang racket

(provide (all-defined-out))

;; Layer 0
(define %call          (gensym))
(define %vau           (gensym))
(define %fn            (gensym))
(define %get           (gensym))
(define %pattern-match (string->uninterned-symbol "%pattern-match"))
(define %scope         (gensym))
(define %environment   (gensym))
(define %arguments     (gensym))
(define %body          (gensym))

(define %f             #f)
(define %t             #t)

#|(define %add           (gensym))
(define %rem           (gensym))
(define %lower         (gensym))
(define %upper         (gensym))
(define %len           (gensym))|#

(define (hash-remove* x . a)
  (let loop ((x  x)
             (a  a))
    (if (null? a)
        x
        (loop (hash-remove x (car a))
              (cdr a)))))

#|(define (update x . args)
  (set-box! x (apply hash-set* (unbox x) args)))|#

(define-syntax-rule (match1 x y . body)
  (match-let ((x y)) . body))

(define (nu-error x . args)
  (error (format "~a error: ~a" x
           (let ([o (open-output-string)])
             (for ([x args]) (display x o))
             (get-output-string o)))))

(define (timeit f (time 10000))
  (let ((old (+ (current-milliseconds) time)))
    (let loop ((i 0))
      (if (> (current-milliseconds) old)
          i
          (begin (f)
                 (loop (+ i 1)))))))


;; Layer 1
; (depends match1)
(define (&get _ a)
  (match1 (list x) a
    (eval x)))

; (depends %call %fn %t)
(define (wrap-fn f)
  (hash %call  (lambda (_ a)
                 (apply f (cdr a)))
        %fn    %t))

; (mutual call)
; (depends %get)
(define (get e x k)
  ; TODO: can't do (get x %get) if x has a %get
  (if (hash-has-key? x %get)
      (call (hash-ref x %get)
            e
            (list (hash-remove x %get) k))
      (hash-ref x k)))


;; Layer 2
; (depends match1 wrap-fn)
(define (&fn _ a)
  (match1 (list x) a
    (wrap-fn (eval x))))

; (mutual call)
; (depends get)
; (recursive nu-eval)
(define (nu-eval e x)
  (cond ((symbol? x)
          (get e (unbox e) x))
        ((pair? x)
          (call (nu-eval e (car x)) e (cdr x)))
        (else x)))


;; Layer 3
; (depends %fn %t nu-eval)
(define (fn e a)
  (hash-set (nu-eval e (list* vau '~ a)) %fn %t))

; (depends %f nu-eval)
(define (do e a)
  (if (null? a)
      %f
      (for/last ([x (in-list a)])
        (nu-eval e x))))

; (depends nu-eval)
(define (map-eval e a)
  (map (lambda (x) (nu-eval e x)) a))


;; Layer 4
; (depends match1 map-eval)
(define (& e a)
  (match1 (cons f r) a
    (apply (eval f) (map-eval e r))))

; (mutual get nu-eval)
; (depends %call %fn map-eval)
; (recursive call)
(define (call f e a)
  (if (procedure? f)
      (f e a)
      (let loop ((x  f)
                 (a  a))
        (if (procedure? x)
            (x e (cons f a))
            (loop (get e x %call)
                  (if (hash-has-key? x %fn)
                      (map-eval e a)
                      a))))))


;; Layer 5
; (depends %pattern-match nu-eval get call)
(define (pattern-match1 _ a)
  (match1 (list _ env pat args) a
    (cond ((null? pat)
            env)
          ((pair? pat)
            (let* ((e  (box env))
                   (f  (nu-eval e (car pat)))
                   (c  (get e f %pattern-match)))
              (call c e (list f env (cdr pat) args))))
          ((eq? pat '~)
            env)
          ((symbol? pat)
            (hash-set env pat args))
          (else
            (if (eq? args pat)
                env
                (nu-error %pattern-match pat " != " args))))))


;; Layer 6
; (depends match1 nu-eval pattern-match1)
(define (def e a)
  (match1 (list n v) a
    ;(update e n (nu-eval e v))
    (let ((v (nu-eval e v)))
      (set-box! e (pattern-match1 '~ (list '~ (unbox e) n v)))
      v)))

; (depends pattern-match1)
(define (pattern-match e name dynamic pat args)
  (box (pattern-match1 dynamic
                       (list '~
                             (if (eq? name '~)
                                 e
                                 (hash-set e name dynamic))
                             pat args))))

; (depends %pattern-match %call %fn %t wrap-fn match1 nu-error pattern-match1)
(define nu-list
  (hash %pattern-match
          (lambda (e a)
            (match1 (list x env pat arg) a
              (let loop ((env  env)
                         (pat  pat)
                         (arg  arg))
                (cond ((null? pat)
                        (if (null? arg)
                            env
                            (nu-error %pattern-match pat " != " arg)))
                      ((null? arg)
                        (nu-error %pattern-match pat " != " arg))
                      (else
                        (loop (pattern-match1 '~ (list '~ env (car pat) (car arg)))
                              (cdr pat)
                              (cdr arg)))))))
        %call
          (wrap-fn list)
        %fn %t))


;; Layer 7
; (depends %call %scope %environment %arguments %body get pattern-match nu-eval)
(define vau-proto
  (hash %call (lambda (e a)
                (let* ((x  (car a))
                       (s  (pattern-match (get e x %scope)
                                          (get e x %environment)
                                          e
                                          (get e x %arguments)
                                          (cdr a))))
                  (nu-eval s (get e x %body))))))


;; Layer 8
; (depends %scope %environment %arguments %body match1 vau-proto do)
(define (vau e a)
  (match1 (list-rest x y r) a
    (hash-set* vau-proto
      %scope        (unbox e)
      %environment  x
      %arguments    y
      %body         (cons do r))))


#|
(vau e {a b}
  (+ a 1)
  (+ b 2))

(dict %scope       (dict ...)
      %environment (quote e)
      %arguments   (quote {a b})
      %body        {do @(quote (+ a 1) (+ b 2))}
      %call        ...)


(def do
  (vau e args
    ((fn {~}
       (eval e {do @(cdr args)}))
     (eval e (car args)))))
|#

#|
(nu-eval globals '((fn a (def b 10) (+ b (car a))) 1))
(nu-eval globals 'b)
(nu-eval globals '(& + 1 2 5))
(nu-eval globals '((fn (list a b c) a) 1 2 3))
(nu-eval globals '((fn (list 1 (list a b)) (+ a b)) 1 (list 2 3)))
(nu-eval globals '(def a 5))
|#

#|
(def list
  [ %pattern-match (case-loop
                     ~ e {}       {}       -> e
                     ~ ~ {}       a        -> (error %pattern-match {} a)
                     ~ ~ p        {}       -> (error %pattern-match p {})
                     ~ e {p1 @p2} {a1 @a2} -> (loop ~ (pattern-match e p1 a1) p2 a2))
  | %call          (wrap-fn list)
  | %fn            %t ])
|#

; (depends %pattern-match %call %fn %t wrap-fn)
(define dict
  (hash %pattern-match
          (lambda (e a)
            (displayln a))
        %call
          (wrap-fn hash)
        %fn %t))

(define globals (box (hash
  ;; Vaus
  'vau   vau
  'do    do
  'fn    fn
  'def   def
  '&get  &get
  '&fn   &fn
  '&     &

  ;; Patterns
  'list  nu-list
  'dict  dict

  ;; Functions
  'pattern-match (hash %call pattern-match1 %fn %t)

  'box   (wrap-fn box)
  'unbox (wrap-fn unbox)
  'set!  (wrap-fn set-box!)

  'car   (wrap-fn car)
  'cdr   (wrap-fn cdr)

  'has   (wrap-fn hash-has-key?)
  'get   (wrap-fn hash-ref)
  'add   (wrap-fn hash-set*)
  'rem   (wrap-fn hash-remove*)

  '*     (wrap-fn *)
  '/     (wrap-fn /)
  '+     (wrap-fn +)
  '-     (wrap-fn -)
  'mod   (wrap-fn modulo)

  ;; Uniqs
  '%get  %get
)))

(set-box! globals (hash-set (unbox globals) 'globals globals))

#|(update globals
  'globals globals)|#



#|
(fn ()
  (var a 5)
  (set-var a 10)
  a)

(fn ()
  (var a 5)
  (let a a))

(fn ()
  (var a a))

(fn ()
  (var + +))

(fn ()
  (let a (+ 5 10))
  a)

(let foo 5)
|#

#|

; (depends %add %rem %lower %upper %len get-val)
(define nil (hash
  %add    (lambda (x k v)
            (let ((x (hash-set x k v)))
              (if (integer? k)
                  (let ((l  (min k (get-val x %lower)))
                        (u  (max (+ k 1) (get-val x %upper))))
                    (hash-set* x
                      %lower (lambda (x) l)
                      %upper (lambda (x) u)))
                  x)))
  %rem    (lambda (x k)
            (let ((x (hash-remove x k)))
              (if (integer? k)
                  (let ((l  (get-val x %lower))
                        (u  (get-val x %upper)))
                    (cond ((= k l)
                            (let loop ((i (+ l 1)))
                              (if (or (hash-has-key? x i)
                                      (>= i u))
                                  (hash-set x %lower (lambda (x) i))
                                  (loop (+ i 1)))))
                          ((= k u)
                            (let loop ((i (- u 1)))
                              (if (or (hash-has-key? x i)
                                      (<= i l))
                                  (hash-set x %upper (lambda (x) i))
                                  (loop (- i 1)))))
                          (else x)))
                  x)))
  %lower  (lambda (x) 0)
  %upper  (lambda (x) 0)
  %len    (lambda (x)
            (- (get-val x %upper)
               (get-val x %lower)))))

; (depends %upper nil)
(define (list->hash x)
  (let loop ((x  x)
             (h  nil)
             (i  0))
    (if (null? x)
        (hash-set h %upper (lambda (x) i))
        (loop (cdr x)
              (hash-set h i (car x))
              (+ i 1)))))
|#

#|
(eval `(set-box! ,(box 5) 10))
|#

#|

(let ((x 5))
  (timeit (lambda () x)))
130763562

(timeit (lambda () 5))
130433025

(let ((x (lambda () 5)))
  (timeit (lambda () (x))))
126242739

(let ((x (box-immutable 5)))
  (timeit (lambda () (unbox x))))
125751269

(let ((x (box 5)))
  (timeit (lambda () (unbox x))))
124316219

(let ((x (make-placeholder 5)))
  (timeit (lambda () (placeholder-get x))))
109018903

(let ((x (hash 'a 5)))
  (timeit (lambda () (hash-ref x 'a))))
80959212

(let ((x (hash 'a (lambda () 5))))
  (timeit (lambda () ((hash-ref x 'a)))))
77829131

(let ((x (hash 'a (make-placeholder 5))))
  (timeit (lambda () (placeholder-get (hash-ref x 'a)))))
71638887

(let ((x (make-parameter 5)))
  (timeit (lambda () (x))))
45191656

|#
