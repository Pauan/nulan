#lang racket

(provide (all-defined-out))

(displayln "hello")

(define globals null)

(struct keyword (value))

(struct wrapped (f)
  #:property prop:procedure
             (lambda (self env args)
               ((wrapped-f self)
                env
                (if (pair? args)
                    (map (lambda (x) (nu-eval x env)) args)
                    (nu-eval args env)))))


(define-syntax-rule (ex x)
  (syntax->datum (expand 'x)))

#|
(define-syntax (checker stx)
  (syntax-case stx ()
    [(_ x args . body)
     ;(displayln (syntax-e #'args))
     #`(begin . body)
     #|(if (symbol? (syntax-e #'x))

         #`(if (eqv? x x) (begin . body)
                  )
         )|#
     ]))

(define-syntax (dsb stx)
  (syntax-case stx (cons)
    [(_ (cons (cons x ()) z) args env . body)
      #'(let* ((u     (car args))
               (x     (car u))
               (args  (cdr args))
               (env   (cons (list 'x x) env)))
          (checker x args
            (dsb z args env . body)))]
    [(_ (cons (cons x y) z) args env . body)
      #'(let* ((u     (car args))
               (args  (cdr args)))
          (checker x u env
            (let* ((x     (car u))
                   (u     (cdr u))
                   (env  (cons (list 'x x) env)))
              (dsb y u env
                (dsb z args env . body))))]
    [(_ (cons x ()) args env . body)
      #'(let* ((x    (car args))
               (env  (cons (list 'x x) env)))
          (checker x args . body))]
    [(_ (cons x y) args env . body)
      (symbol? (syntax-e #'x))
      #'(let* ((x     (car args))
               (args  (cdr args))
               (env   (cons (list 'x x) env)))
          (checker x args env
            #'(dsb y args env . body)))]
    [(_ x args env . body)
      #'(let* ((x    args)
               (env  (cons (list 'x x) env)))
          (checker x args . body))]))
|#

#|

;; cons in car
(let ((u     (car args))
      (args  (cdr args)))
  ...)

;; cons
(let* ((x     (car args))
       (args  (cdr args))
       (env   (cons (list 'x x) env)))
  ...)

(if (eq? (car args) 1)
    (let ((args (cdr args)))
      ...)
    (fail))

;; atom by itself or in cdr of cons
(let* ((x    args)
       (env  (cons (list 'x x) env)))
  ...)

(if (eq? args 1)
    ...
    (fail))

|#

(define FAIL "FAILURE!!!")

(define-syntax (dsb stx)
  (define (walker args x env seen body)
    (cond ((pair? args)
            (if (eq? (car args) 'cons)
                (let ((c  (cadr args))
                      (r  (caddr args)))
                  (if (and (pair? c)
                           (eq? (car c) 'cons))
                      #`(let ((u    (car #,x))
                              (#,x  (cdr #,x)))
                          #,(walker c #'u env seen (list (walker r x env seen body))))
                      (if (and (symbol? c)
                               (not (memq c seen)))
                          #`(let* ((#,c    (car #,x))
                                   (#,x    (cdr #,x))
                                   (#,env  (cons (list '#,c #,c) #,env)))
                              #,(walker r x env (cons c seen) body))
                          #`(if (eq? (car #,x) #,c)
                                (let ((#,x (cdr #,x)))
                                  #,(walker r x env seen body))
                                FAIL))))
                (error "cons" (car args))))
          #| if you uncomment this, it makes the list matching non-strict
          ((null? args)
            #`(begin #,@body))|#
          ((and (symbol? args)
                (not (memq args seen)))
            #`(let* ((#,args  #,x)
                     (#,env   (cons (list '#,args #,args) #,env)))
                #,@body))
          (else
            #`(if (eq? #,x #,args)
                  (begin #,@body)
                  FAIL))))
  (syntax-case stx (cons)
    [(_ args x env . body)
      (walker (syntax->datum #'args) #'x #'env null (syntax->datum #'body))]))

#|

(-> (($vau [] env 1) [] [])
    1)

(-> (($vau [] env 1) [] 1)
    "FAILURE!!!")

(-> (($vau [] env 1) [] [1])
    "FAILURE!!!")


(-> (($vau 1 env 1) [] 1)
    1)

(-> (($vau 1 env 1) [] 2)
    "FAILURE!!!")

(-> (($vau 1 env 1) [] [1])
    "FAILURE!!!")


(-> (($vau [X] env X) [] [1])
    1)

(-> (($vau [X] env X) [] [1 2])
    "FAILURE!!!")

(-> (($vau [X] env X) [] 1)
    "FAILURE!!!")


(-> (($vau [X X] env X) [] [1 1])
    1)

(-> (($vau [X X] env X) [] [1 2])
    "FAILURE!!!")


(-> (($vau [X [X]] env X) [] [1 [1]])
    1)

(-> (($vau [X [X]] env X) [] [1 [2]])
    "FAILURE!!!")

(-> (($vau [X [X]] env X) [] [1 1])
    "FAILURE!!!")


(-> (($vau [1 2 . U] u U) [] [1 2 3 4 5])
    (3 4 5))

(-> (($vau [1 2 . U] u U) [] [3 2 3 4 5])
    "FAILURE!!!")

(-> (($vau [1 2 . U] u U) [] [1 3 3 4 5])
    "FAILURE!!!")

(-> (($vau [1 2 . U] u U) [] [3 2 3 4 5])
    "FAILURE!!!")

(-> (($vau [1 2 . u] u u) [] [1 3 3 4 5])
    "FAILURE!!!")


(-> (($vau [[X]] env X) [] [[1]])
    1)

(-> (($vau [[X]] env X) [] [[1 2]])
    "FAILURE!!!")

(-> (($vau [[X]] env X) [] [[1] 2])
    "FAILURE!!!")

(-> (($vau [[X]] env X) [] 1)
    "FAILURE!!!")

(-> (($vau [[X]] env X) [] [1])
    "FAILURE!!!")


(-> (($vau [[X . Y]] env [X Y]) [] [[1]])
    (1 ()))

(-> (($vau [[X . Y]] env [X Y]) [] [[1 2]])
    (1 (2)))

(-> (($vau [[X . Y]] env [X Y]) [] [[1 2] 3])
    "FAILURE!!!")

|#

(define-syntax ($vau stx)
  (syntax-case stx ()
    [(_ args env . body)
      #`(lambda (env u)
          ;#,(datum->syntax stx (list->cons (syntax->datum #'args)))
          (dsb args u env . body))]))


#|
(define (dsb env args x)
  (let self ((args  args)
             (x     x))
    (displayln args)
    (displayln x)
    (newline)
    (cond ((null? args)
            env)
          ((eq? (car args) 'cons)
            )
          ((symbol? (car args))
            )

          (else (self (cdr args)
                      (cdr x))))
        (cons (list (car args)
                    (car x))
              (self (cdr args)
                    (cdr x)))))

(define-syntax ($vau stx)
  (define (list->cons x)
    (if (pair? x)
        `(cons ,(car x) ,(list->cons (cdr x)))
        x))
  (syntax-case stx ()
    [(_ args env . body)
      #`(lambda (env u)
          (let* #,(dsb env '#,(datum->syntax stx (list->cons (syntax->datum #'args))) u)
            . body)
          (set! env (dsb env '#,(datum->syntax stx (list->cons (syntax->datum #'args))) u))
          . body)]))
|#


(define-syntax ($lambda stx)
  (syntax-case stx ()
    [(_ args . body)
      ;; TODO: why the hell is this needed?
      (datum->syntax stx
        (syntax->datum
          #`(wrapped ($vau args u . body))))]))

(define-syntax-rule ($set name x)
  (set! globals (cons (list 'name x) globals)))

(define-syntax-rule ($def name args . body)
  ($set name ($lambda args . body)))

($set $vau (lambda (env args)
             (dsb (Args Env . Body) args
               (vau Args Env Body))))

($defvau $vau (Args Env . Body)
  (vau Args Env Body))

($def wrap (X Y)
  (wrapped X))

($def unwrap (X)
  (wrapped-f X))

#|(set! $vau (lambda (env parms n . body)

             ))|#

(define (lookup x env)
  (let ((x (member x env)))
    (if x (car x) x)))

(define nu-eval
  (case-lambda
    ((x)
      (nu-eval x globals))
    ((x env)
      (cond ((symbol? x)  (lookup x env))
            ((pair? x)    ((nu-eval (car x) env) env (cdr x)))
            (else         x)))))

#|(require (rename-in '#%kernel
           (#%app #%app))|#

(require (only-in '#%kernel #%app))

(define (list->cons x)
  (cond ((pair? x)  `(cons ,(car x) ,(list->cons (cdr x))))
        ;((null? x)  `(quote ,x))
        (else       x)))

(define (parse-square-brackets ch port src line col pos)
  (list->cons (read/recursive port #\[ #f)))

(define reader (make-readtable (current-readtable)
                 #\[ 'terminating-macro parse-square-brackets))

(current-readtable reader)
