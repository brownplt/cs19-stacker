#lang racket
(provide (rename-out [my-module-begin #%module-begin])
         (rename-out [my-top-interaction #%top-interaction]))
(require "./s-exp-of-state.rkt")
(require "./pict-of-state.rkt")
(require "./parse.rkt")
(require "./runtime.rkt")

(define (my-pict-of-state state)
  ((pict-of-state #t #t) ((s-exp-of-state #f) state)))

(define (run show-closures? e)
  (define check void)
  (eval show-closures? check my-pict-of-state (parse e))
  #;(eval show-closures? check pictify (parse e))
  )

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ #:show-closures form ...)
     #'(#%module-begin (run #t '(form ...)))]
    [(_ form ...)
     #'(#%module-begin (run #f '(form ...)))]))

(define-syntax (my-top-interaction stx)
  (syntax-case stx ()
    [(_ . form)
     #'(#%top-interaction . (displayln "Please run programs in the editor window."))]))
