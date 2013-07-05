#lang racket/base

(require racket/match)
(provide compile)

;; Utilities
(define (unwrap x)
  (if (syntax? x)
      (syntax-e x)
      x))

(define (deep-unwrap x)
  (if (syntax? x)
      (syntax->datum x)
      x))

(define (my-immutable? x)
  (or (immutable? x)
      (pair? x)))

; Generic equality predicate, based on egal
; http://home.pipeline.com/~hbaker1/ObjectIdentity.html
(define (is? x y)
  (let ((x (unwrap x))
        (y (unwrap y)))
    (if (number? x)
        (if (number? y)
            (= x y)
            #f)
        (if (my-immutable? x)
            (if (my-immutable? y)
                ; TODO: ridiculous that it needs to call deep-unwrap
                (equal? (deep-unwrap x) (deep-unwrap y))
                #f)
            ; need to use eqv? for characters
            (eqv? x y)))))


(define vars (make-parameter (hash)))

(struct bypass (value))

(struct box (uniq [get #:auto #:mutable]
                  [set #:auto #:mutable]
                  [mac #:auto #:mutable])
  #:guard (lambda (uniq _)
            (when (symbol-interned? uniq)
              (set! uniq (string->uninterned-symbol (symbol->string uniq))))
            (values uniq)))

(struct box-const box ())

(define (box-sym? x)
  (or (box? x) (symbol? x)))

(define (->var x)
  (cond ((box? x)
          (box-uniq x))
        ((symbol? x)
          x)
        (else
          (error "not a box or symbol:" x))))

(define (->box x)
  (if (box? x)
      x
      (hash-ref (vars) x (lambda () (error "undefined variable:" x)))))

(define (bind-box x)
  (let ((y (box (->var x))))
    (vars (hash-set (vars) x y))
    y))

(define (compile x)
  (let ((y (unwrap x)))
    (cond ((pair? y)
            (let ((c (unwrap (car y))))
              (if (box-sym? c)
                  (let ((c (->box c)))
                    (if (box-mac c)
                        (compile ((box-mac c) y))
                        (map compile y)))
                  (map compile y))))
          ((box-sym? y)
            (let ((y (->box y)))
              (if (box-get y)
                  ((box-get y))
                  (box-uniq y))))
          ((bypass? y)
            (bypass-value y))
          ((null? y)
            #`(quote #,x))
          (else
            x))))

(define (drop1 f)
  (lambda (_ . rest)
    (apply f rest)))

(define (export-mac x f)
  (set-box-mac! (bind-box x) f))

(define (box-setter set)
  (match-lambda
    [(list _ n f)
      (set (->box (unwrap n))
           (drop1 (eval (compile f))))
      null]))


(export-mac '$get! (box-setter set-box-get!))
(export-mac '$set! (box-setter set-box-set!))
(export-mac '$mac! (box-setter set-box-mac!))

(export-mac 'var (match-lambda
  [(list _ x)
    (match (unwrap x)
      [(list _ n v)
        (let* ((v (compile v))
               (n (compile (bind-box (unwrap n)))))
          (bypass #`(set! #,n #,v)))])]))

#|(export-mac 'vars (match-lambda
  [(list-rest _ x)

    ]))|#
