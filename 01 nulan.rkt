#lang racket/base

(require racket/match)
(require racket/list)

(provide (all-defined-out))

;; Layer 0
(define %call          (gensym))
(define %fn            (gensym))
(define %get           (gensym))
(define %pattern-match (string->uninterned-symbol "%pattern-match"))
(define %scope         (gensym))
(define %environment   (gensym))
(define %arguments     (gensym))
(define %body          (gensym))

(define %f             #f)
(define %t             #t)

(define (hash-remove* x . a)
  (let loop ((x  x)
             (a  a))
    (if (null? a)
        x
        (loop (hash-remove x (car a))
              (cdr a)))))

(define-syntax-rule (match1 x y . body)
  (match-let ((x y)) . body))

(define (nu-error x . args)
  (error (format "~a error: ~a" x
           (let ([o (open-output-string)])
             (for ([x args]) (display x o))
             (get-output-string o)))))


;; Layer 1
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
; (depends get)
; TODO: get rid of this function (inline it into nu-eval)
(define (lookup e n)
  (get e (unbox e) n))


;; Layer 3
; (mutual call)
; (depends lookup)
; (recursive nu-eval)
(define (nu-eval e x)
  (cond ((symbol? x)
          (unbox (lookup e x)))
        ((pair? x)
          (call (nu-eval e (car x)) e (cdr x)))
        (else x)))


;; Layer 4
; (depends %f nu-eval)
(define (do e a)
  (if (null? a)
      %f
      (for/last ([x (in-list a)])
        (nu-eval e x))))

; (depends nu-eval)
(define (map-eval e a)
  (map (lambda (x) (nu-eval e x)) a))


;; Layer 5
; (depends match1 map-eval)
; TODO: make this work in the REPL
(define (& e a)
  (match1 (cons f r) a
    (displayln (eval f))
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


;; Layer 6
; (depends %pattern-match nu-eval get call)
(define (pattern-match1 _ a)
  (match1 (list _ env pat val) a
    (cond ((pair? pat)
                   ; TODO: verify that this is correct behavior, ideally with unit tests
            (let* ((e  (box env))
                   (f  (nu-eval e (car pat)))
                   (c  (get e f %pattern-match)))
              (call c e (list f env (cdr pat) val))))
          ((eq? pat '~)
            env)
          ((symbol? pat)
            (hash-set env pat (box val)))
          (else
            (if (eq? pat val)
                env
                (nu-error %pattern-match pat " != " val))))))


;; Layer 7
; (depends match1 nu-eval pattern-match1)
(define (var e a)
  (match1 (list n v) a
    ;(update e n (nu-eval e v))
    (let ((v (nu-eval e v)))
      (set-box! e (pattern-match1 '~ (list '~ (unbox e) n v)))
      v)))

; (depends pattern-match1)
(define (pattern-match e name dynamic pat val)
  (box (pattern-match1 dynamic
                       (list '~
                             (if (eq? name '~)
                                 e
                                 ; dynamic is doubly wrapped to avoid a bug with (def b b)
                                 (hash-set e name (box dynamic)))
                             pat val))))

; (depends %pattern-match %call %fn %t wrap-fn match1 nu-error pattern-match1)
(define nu-list
  (hash-set (wrap-fn list)
    %pattern-match
      (lambda (_ a)
        (match1 (list _ env pat val) a
          (let loop ((env  env)
                     (pat  pat)
                     (val  val))
            (cond ((null? pat)
                    (if (null? val)
                        env
                        (nu-error %pattern-match pat " != " val)))
                  ((null? val)
                    (nu-error %pattern-match pat " != " val))
                  (else
                    (if (and (pair? (car pat))
                             (eq? (caar pat) 'splice))
                        (if (null? (cdr pat))
                            (pattern-match1 '~ (list '~ env (cadar pat) val))
                            (let-values (((x y) (split-at-right val (length (cdr pat)))))
                              (loop (pattern-match1 '~ (list '~ env (cadar pat) x))
                                    (cdr pat)
                                    y)))
                        (loop (pattern-match1 '~ (list '~ env (car pat) (car val)))
                              (cdr pat)
                              (cdr val))))))))))

; (depends %pattern-match %call %fn %t wrap-fn match1 nu-error pattern-match1)
(define dict
  (hash-set (wrap-fn hash)
    %pattern-match
      (lambda (_ a)
        (match1 (list _ env pat val) a
          (let loop ((env  env)
                     (pat  pat)
                     (val  val))
            (cond ((null? pat)
                    env)
                  ((and (pair? (car pat))
                        (eq? (caar pat) 'splice))
                    (loop (pattern-match1 '~ (list '~ env (cadar pat) val))
                          (cdr pat)
                          val)) ; TODO: not sure what this should be...
                  ((null? (cdr pat))
                    (nu-error %pattern-match "missing pattern " (car pat)))
                  (else
                           ; TODO: verify that this is correct behavior, ideally with unit tests
                    (let* ((e  (box env))
                           (x  (nu-eval e (car pat))))
                      (if (hash-has-key? val x)
                          (loop (pattern-match1 '~ (list '~ env (cadr pat) (get e val x)))
                                (cddr pat)
                                (hash-remove val x))
                          (nu-error %pattern-match "key " (car pat) " not in " val))))))))))


;; Layer 8
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


;; Layer 9
; (depends %scope %environment %arguments %body match1 vau-proto do)
(define (vau e a)
  (match1 (list-rest x y r) a
    (hash-set* vau-proto
      %scope        (unbox e)
      %environment  x
      %arguments    y
      %body         (cons do r))))


(define globals (box (hash
  ;; Vaus
  'var   (box var)
  'vau   (box vau)
  'do    (box do)
  '&     (box &)

  ;; Patterns
  'list  (box nu-list)
  'dict  (box dict)

  ;; Functions
  'pattern-match (box (hash %call pattern-match1 %fn %t))

  'box       (box (wrap-fn box))
  'unbox     (box (wrap-fn unbox))
  'set-box!  (box (wrap-fn (lambda (b v) (set-box! b v) v)))

  'list* (box (wrap-fn list*))
  'car   (box (wrap-fn car))
  'cdr   (box (wrap-fn cdr))

  'eval  (box (wrap-fn nu-eval))

  'has   (box (wrap-fn hash-has-key?))
  'get   (box (wrap-fn hash-ref))
  'add   (box (wrap-fn hash-set*))
  'rem   (box (wrap-fn hash-remove*))

  'prn   (box (wrap-fn displayln))
  'shell-arguments (box (wrap-fn current-command-line-arguments))

  '*     (box (wrap-fn *))
  '/     (box (wrap-fn /))
  '+     (box (wrap-fn +))
  '-     (box (wrap-fn -))
  'mod   (box (wrap-fn modulo))

  ;; Uniqs
  '%call           (box %call)
  '%fn             (box %fn)
  '%get            (box %get)
  '%pattern-match  (box %pattern-match)
  '%scope          (box %scope)
  '%environment    (box %environment)
  '%arguments      (box %arguments)
  '%body           (box %body)
  '%f              (box %f)
  '%t              (box %t)
)))

(set-box! globals (hash-set (unbox globals) 'globals globals))


;; Stuff for the "nulan" executable
(define (nu-eval-string s)
  (nu-eval globals (read (open-input-string s))))

(define (nu-eval-file s)
  ;; This is so that it's possible to retrieve the column/line of an input port
  (parameterize ((port-count-lines-enabled #t))
    (call-with-input-file s
      (lambda (p)
        (let loop ()
          (let ((x (read p)))
            (unless (eof-object? x)
              (nu-eval globals x)
              (loop))))))))


;(require readline/pread)

(define (nu-repl)
  ;; http://arclanguage.org/item?id=10344
  (let ((interactive (terminal-port? (current-input-port))))
    (when interactive
      (namespace-require 'readline/rep-start)
      ;(namespace-require 'readline/pread)
      ;(dynamic-require 'readline/rep-start #f)
      ;(current-prompt #"=> ")
      )

    (let loop ()
      ;; This causes Ctrl+C to return to the REPL, rather than aborting.
      ;; Technique was taken from Racket's (read-eval-print-loop) which
      ;; I found in /usr/share/racket/collects/racket/private/misc.rkt
      (call-with-continuation-prompt
        (lambda ()
          ;; http://arclanguage.org/item?id=10344
          (let* ((it    (if interactive
                            ((current-prompt-read))
                            (read)))
                 (expr  (if (syntax? it)
                            (syntax->datum it)
                            it)))
            (if (eof-object? expr)
                (when interactive (newline))
                (begin (write (nu-eval globals expr))
                       (newline)
                       (when interactive (newline))
                       ;; Abort to loop. (Calling `repl` directly would not be a tail call.)
                       (abort-current-continuation (default-continuation-prompt-tag))))))
        (default-continuation-prompt-tag)
        (lambda _ (loop))))))
