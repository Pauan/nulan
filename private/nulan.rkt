#lang racket/base

(provide (all-from-out racket/base))
#|
;; Layer 0
(define %f             #f)
(define %t             #t)
(define pattern-seen   (make-parameter #f))

(define (make-uniq [s '%])
  (string->uninterned-symbol (symbol->string s)))

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
; (depends make-uniq)
(define %call          (make-uniq '%call))

(define %has           (make-uniq '%has))
(define %get           (make-uniq '%get))
(define %set           (make-uniq '%set))
(define %rem           (make-uniq '%rem))

(define %pattern-match (make-uniq '%pattern-match))
(define %scope         (make-uniq '%scope))
(define %environment   (make-uniq '%environment))
(define %arguments     (make-uniq '%arguments))
(define %body          (make-uniq '%body))
(define %splice        (make-uniq '%splice))


;; Layer 2
; (depends %pattern-match match1)
(define ~
  (hash %pattern-match
    (lambda (e a)
      (match1 (list _ _ (list) _) a
        e))))

; (mutual call nu-has)
; (depends %get is?)
(define (nu-get e x k)
  (cond ((hash? x)
          ; TODO: can't do (get x %get) if x has a %get
          (if (nu-has e x %get)
              (call (hash-ref x %get)
                    e
                    ; TODO: use nu-remove?
                    (list (hash-remove x %get) k))
              (hash-ref x k)))
        ((procedure? x)
          (if (is? k %call)
              x
              (hash-ref x k)))))


;; Layer 3
; (depends nu-get)
; TODO: get rid of this function (inline it into nu-eval)
(define (lookup e n)
  (nu-get e (unbox e) n))


;; Layer 4
; (mutual call)
; (depends lookup)
; (recursive nu-eval)
(define (nu-eval e x)
  (cond ((symbol? x)
          (unbox (lookup e x)))
        ((pair? x)
          (call (nu-eval e (car x)) e (cdr x)))
        (else x)))


;; Layer 5
; (depends nu-eval)
(define (nu-if e a)
  ; TODO: simplify this
  (if (null? a)
      %f
      (let ((c (nu-eval e (car a))))
        (if c
            (if (null? (cdr a))
                c
                (nu-eval e (cadr a)))
            (nu-if e (cddr a))))))

; (depends %f nu-eval)
(define (do e a)
  (if (null? a)
      %f
      (for/last ([x (in-list a)])
        (nu-eval e x))))

; (depends nu-eval)
(define (map-eval e a)
  (map (lambda (x) (nu-eval e x)) a))


;; Layer 6
; (depends map-eval)
(define (vau->fn f)
  (lambda (e a)
    (f e (map-eval e a))))

; (depends map-eval)
(define (racket->fn f)
  (lambda (e a)
    (apply f (map-eval e a))))

; (depends match1 map-eval)
; TODO: make this work in the REPL
(define (& e a)
  (match1 (cons f r) a
    (displayln (eval f))
    (apply (eval f) (map-eval e r))))

; (mutual nu-get nu-eval)
; (depends %call map-eval)
; (recursive call)
(define (call f e a)
  (if (procedure? f)
      (f e a)
      (let loop ((x f))
        (if (procedure? x)
            (x e (cons f a))
            (loop (nu-get e x %call))))))


;; Layer 7
; (mutual nu-get)
; (depends %has call is?)
(define (nu-has e x k)
  (cond ((hash? x)
          (if (hash-has-key? x %has)
              (call (nu-get x %has)
                    e
                    ; TODO: use nu-remove?
                    (list (hash-remove x %has) k))
              (hash-has-key? x k)))
        ((procedure? x)
          (if (is? k %call)
              %t
              %f))))


;; Layer 8
; (depends %call %set call nu-get nu-has)
(define (nu-set e x . a)
  (cond ((hash? x)
          (let loop ((x  x)
                     (a  a))
            (cond ((null? a)
                    x)
                  ((nu-has x %set)
                    (loop (call (nu-get x %set)
                                e
                                (list x (car a) (cadr a)))
                          (cddr a)))
                  (else
                    (loop (hash-set x (car a) (cadr a))
                          (cddr a))))))
        ((procedure? x)
          (apply hash-set* (hash %call x) a))))

; (depends %call %rem nu-has nu-get call is?)
(define (nu-rem e x k . rest)
  (cond ((hash? x)
          (let loop ((x  x)
                     (k  rest))
            (cond ((null? k)
                    x)
                  ((nu-has e x %rem)
                    (loop (call (nu-get x %rem)
                                e
                                (list (hash-remove x %rem) (car k)))
                          (cdr k)))
                  (else
                    (loop (hash-remove x k)
                          (cdr k))))))
        ((procedure? x)
          (if (is? k %call)
              (hash)
              x))))

; (depends %pattern-match pattern-seen match1 nu-eval nu-get call)
(define (pattern-match1 _ a)
  (match1 (list _ b p v) a
    (cond ((pair? p)
                  ; TODO: verify that this is correct behavior, ideally with unit tests
            (let ((f (nu-eval b (car p))))
              (call (nu-get b f %pattern-match) b (list f pattern-match2 (cdr p) v))))
          ((symbol? p)
            (let ((seen (pattern-seen)))
              (if (hash-has-key? seen p)
                  (if (is? (hash-ref seen p) v)
                      b
                      (nu-error %pattern-match (hash-ref seen p) " != " v))
                  (begin (pattern-seen (hash-set seen p v))
                         (box (nu-set b (unbox b) p (box v)))))))
          (else
            (if (is? p v)
                b
                (nu-error %pattern-match p " != " v))))))

; (mutual pattern-match1)
; (depends %call %t)
(define pattern-match2 (vau->fn pattern-match1))


; (depends pattern-seen pattern-match1 ~)
(define (pattern-match b p v)
  (parameterize ((pattern-seen (hash)))
    (call pattern-match1 b (list ~ (box (unbox b)) p v))
    (pattern-match1 ~ (list ~ (box (unbox b)) p v))))


; (depends match1 nu-eval pattern-match)
(define (var e a)
  (match1 (list n v) a
    (let ((v (nu-eval e v)))
      (set-box! e (unbox (pattern-match e n v)))
      v)))

; (depends %pattern-match %call %t racket->fn match1 nu-error)
(define nu-list
  (hash %call (racket->fn list)
    %pattern-match
      (lambda (e a)
        (match1 (list _ f pat val) a
          (let loop ((e    e)
                     (pat  pat)
                     (val  val))
            (cond ((null? pat)
                    (call f e (list e pat val)))
                  ((null? val)
                    (loop (call f e (list e (car pat) %f))
                          (cdr pat)
                          val))
                  (else
                    (if (and (pair? (car pat))
                             (is? (caar pat) %splice)) ; TODO
                        (if (null? (cdr pat))
                            (call f e (list e (cadar pat) val))
                            (let-values (((x y) (split-at-right val (length (cdr pat)))))
                              (loop (call f e (list e (cadar pat) x))
                                    (cdr pat)
                                    y)))
                        (loop (call f e (list e (car pat) (car val)))
                              (cdr pat)
                              (cdr val))))))))))

; (depends %pattern-match %call %t racket->fn match1 nu-error nu-get)
(define dict
  (hash %call (racket->fn hash)
    %pattern-match
      (lambda (e a)
        (match1 (list _ f e pat val) a
          (let loop ((e    e)
                     (pat  pat)
                     (val  val))
            (cond ((null? pat)
                    e)
                  ((and (pair? (car pat))
                        (is? (caar pat) %splice)) ; TODO
                    (loop (call f e (list e (cadar pat) val))
                          (cdr pat)
                          val)) ; TODO: not sure what this should be...
                  ((null? (cdr pat))
                    (nu-error %pattern-match "missing pattern " (car pat)))
                  (else
                          ; TODO: verify that this is correct behavior, ideally with unit tests
                    (let ((x (nu-eval e (car pat))))
                      (if (nu-has e val x)
                          (loop (call f e (list e (cadr pat) (nu-get e val x)))
                                (cddr pat)
                                (nu-rem e val x))
                          ; (nu-error %pattern-match "key " (car pat) " not in " val)
                          (call f e (list e (cadr pat) %f)))))))))
    #|%pattern-match-splicing
      (lambda (_ a)
        (match1 (list _ env pat val) a

          ))|#
          ))

; (depends %pattern-match racket->fn match1)
(define nu-box
  (hash %call (racket->fn box)
    %pattern-match
      (lambda (e a)
        (match1 (list _ f (list pat) val) a
          (call f e (list e pat (unbox val)))))))


;; Layer 9
; (depends %call nu-set match1 map-eval call)
(define (fn e a)
  (match1 (list f) a
    (nu-set e f %call (lambda (e a)
                        (call f e (map-eval e a))))))

; (depends fn ~ vau)
(define (nu-fn e a)
  (fn e (list (vau e (cons (list ~) a)))))

; (depends %splice dict nu-list)
(current-readtable
  (make-readtable #f #\[ 'terminating-macro
                       (lambda (ch port src line col pos)
                         (cons dict (read/recursive port ch #f)))
                     #\{ 'terminating-macro
                       (lambda (ch port src line col pos)
                         (cons nu-list (read/recursive port ch #f)))
                     #\@ 'terminating-macro
                       (lambda (ch port src line col pos)
                         (list %splice (read/recursive port #f #f)))
                     #\~ 'non-terminating-macro
                       (lambda (ch port src line col pos)
                         (list ~))
                     #\- 'terminating-macro
                       (lambda (ch port src line col pos)
                         (if (eqv? (peek-char port) #\>)
                             (begin (read-char port)
                                    (let loop ((args null))
                                      (let ((c (peek-char port)))
                                        (displayln (eqv? c #\newline))
                                        (cond ((eqv? c #\space)
                                                (read-char port)
                                                (loop args))
                                              ((or (eqv? c #\))
                                                   (eqv? c #\])
                                                   (eqv? c #\})
                                                   (eqv? c #\|)
                                                   (eqv? c #\newline)
                                                   (eqv? c #\return)
                                                   (eof-object? c))
                                                `(,nu-fn ,(reverse (cdr args)) ,(car args)))
                                              (else
                                                (loop (cons (read/recursive port) args)))))))
                             (read/recursive port ch #f)))))

; (depends %call %scope %environment %arguments %body nu-get pattern-match nu-eval)
(define vau-proto
  ; TODO: using nu-get is potentially insecure: it can leak out hidden gensyms
  (hash %call (lambda (e a)
                (let* ((x  (car a))
                       (s  (pattern-match (pattern-match (box (nu-get e x %scope))
                                                         (nu-get e x %environment)
                                                         e)
                                          (nu-get e x %arguments)
                                          (cdr a))))
                  (nu-eval s (nu-get e x %body))))))


;; Layer 10
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
  'if    (box nu-if)
  'var   (box var)
  'vau   (box vau)
  'do    (box do)
  '&     (box &)

  ;; Patterns
  'list    (box nu-list)
  'dict    (box dict)
  'ignore  (box ~)

  ;; Functions
  'fn  (box (vau->fn fn))

  'error  (box (racket->fn nu-error))

  'make-uniq (box (racket->fn make-uniq))
  'uniq?     (box (racket->fn (lambda (x) (not (symbol-interned? x)))))

  'is  (box (racket->fn is?))

  'pattern-match (box (racket->fn pattern-match))

  'box       (box nu-box)
  'unbox     (box (racket->fn unbox))
  'set-box!  (box (racket->fn (lambda (b v) (set-box! b v) v)))

  'list* (box (racket->fn list*))
  'car   (box (racket->fn car))
  'cdr   (box (racket->fn cdr))

  'eval  (box (racket->fn nu-eval))

  'has   (box (racket->fn nu-has))
  'get   (box (racket->fn nu-get))
  'add   (box (racket->fn nu-set))
  'rem   (box (racket->fn nu-rem))

  'prn   (box (racket->fn displayln))
  'shell-arguments (box (racket->fn current-command-line-arguments))

  '*     (box (racket->fn *))
  '/     (box (racket->fn /))
  '+     (box (racket->fn +))
  '-     (box (racket->fn -))
  'mod   (box (racket->fn modulo))

  ;; Uniqs
  '%call           (box %call)
  '%get            (box %get)
  '%pattern-match  (box %pattern-match)
  '%scope          (box %scope)
  '%environment    (box %environment)
  '%arguments      (box %arguments)
  '%body           (box %body)
  '%f              (box %f)
  '%t              (box %t)
  '%splice         (box %splice)
)))

(set-box! globals (hash-set (unbox globals) 'globals (box globals)))


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
|#
