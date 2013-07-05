#lang racket/base

(require syntax/strip-context
         "compile.rkt")

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(define (my-read in)
  (syntax->datum
    (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (strip-context
    #`(module nulan "private/nulan.rkt"
        #,@(let loop ()
             (let ((x (read-syntax src in)))
               (if (eof-object? x)
                   null
                   (cons (compile x) (loop))))))))
