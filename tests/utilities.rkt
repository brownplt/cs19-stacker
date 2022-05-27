#lang racket
(provide (all-defined-out))
(require racket/sandbox)

(sandbox-path-permissions
 (list*
  ;;; (list 'exists "/lib64")
  ;;; (list 'exists "/usr/lib64")
  ;;; (list 'exists (current-directory))
  ;;; (list 'exists "/System")
  ;;; (list 'exists "/Users")
  (list 'exists (current-directory))
  (sandbox-path-permissions)
  ))
(define (eval-in-smol-step program)
  (parameterize ([sandbox-output 'string]
                 [sandbox-eval-limits (list 10 #f)]
                 [sandbox-propagate-exceptions #f])
    (define ev (make-module-evaluator
                `(module m smol-step/hof/semantics
                   #:no-trace
                   ,@program)))
    (normalize (get-output ev))))

(define (eval-in-smol program)
  (define port (open-output-string))
  (parameterize ([sandbox-output port]
                 [sandbox-eval-limits (list 10 #f)]
                 [sandbox-propagate-exceptions #f])
    ;; sandbox-propagate-exceptions fails to catch some errors (e.g. `(defvar equal? equal?)`)
    (with-handlers ([any/c (lambda (e) "error")])
      (define ev (make-module-evaluator
                  `(module m smol/hof/semantics
                     ,@program)))
      (normalize (get-output-string port)))))

(define (test-equivalent program)
  (displayln ";; Program")
  (writeln program)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([oe-standard (eval-in-smol program)]
           [oe-step (eval-in-smol-step program)]
           [r (equal? oe-standard oe-step)])
      (when (not r)
        (displayln "-----------------------------")
        (displayln ";; smol")
        (writeln oe-standard)
        (displayln "-----------------------------")
        (displayln ";; smol-step")
        (writeln oe-step))))
  (newline))

(define (test-expect/smol program expect)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([result (eval-in-smol program)]
           [expect (normalize-expect expect)]
           [r (equal? result expect)])
      (when (not r)
        (begin
          (displayln ";; Program")
          (writeln program)
          (displayln "-----------------------------")
          (displayln ";; smol actual")
          (writeln result)
          (displayln ";; expected")
          (writeln expect)
          (newline))))))

(define (test-expect/smol-step program expect)
  (with-handlers ([any/c (lambda (e)
                           (displayln ";; exception")
                           (displayln e)
                           (newline))])
    (let* ([result (eval-in-smol-step program)]
           [r (equal? result (normalize-expect expect))])
      (when (not r)
        (begin
          (displayln ";; Program")
          (writeln program)
          (displayln "-----------------------------")
          (displayln ";; smol-step actual")
          (writeln result)
          (displayln ";; expected")
          (writeln expect)
          (newline))))))

(define (normalize output)
  ((compose
    (lambda (output)
      (regexp-replace* #rx" $" output ""))
    (lambda (output)
      (regexp-replace* #rx"\n" output " "))
    (lambda (output)
      (regexp-replace* #rx"#<procedure:[^\n]*>" output "#<procedure>"))
    (lambda (output)
      (regexp-replace* #rx"error:[^\n]*" output "error"))
    )
   output))

(define (normalize-expect expect)
  (string-append expect ""))