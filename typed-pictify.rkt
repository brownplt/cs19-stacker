#lang plait
(require "datatypes.rkt")
(require "utilities.rkt")
(require "stringify.rkt")
(require (opaque-type-in pict (Pict pict?))
         (typed-in pict
                   (vl-append : (Number Pict Pict -> Pict))
                   (vc-append : (Number Pict Pict -> Pict))
                   (hc-append : (Number Pict Pict -> Pict))
                   (cc-superimpose : (Pict Pict -> Pict))
                   (blank : (Number Number -> Pict))
                   (frame : (Pict -> Pict))
                   (filled-rectangle : (Number Number Boolean -> Color))
                   (pict-width : (Pict -> Number))
                   (pict-height : (Pict -> Number))))
(require (typed-in pict/color
                   (white : (Pict -> Pict))))
(require (typed-in racket
                   (partition : (('a -> Boolean) (Listof 'a) -> ((Listof 'a) * (Listof 'a))))))
(define (vl-concat n p*)
  (ind-List (rest p*)
            (first p*)
            (lambda (IH p)
              (vl-append n p IH))))

(define (pict-of-e (t : Term))
  (text (stringify-e t)))
(define (pict-of-env (env : Env))
  (text (stringify-env env)))
(define (pict-of-ectx (ectx : ECtx))
  (text (stringify-ectx ectx)))
(define (pict-of-sf sf)
  (local [(define-values (env ectx ann) sf)]
    (bg color-stack-item
        (frame
         (pad padding
              (vl-append padding
                         (field "Context" ectx)
                         (field "Environment @" env)))))))
(define (pict-of-stack (stack : Stack))
  (box
   (vl-concat
    0
    (cons
     (field-label "Stack")
     (map pict-of-sf (reverse stack))))
   color-stack-bg))
(define (pict-of-heap heap)
    (let-values ([(envs others) (partition is-env?
                                           (filter heapitem-interesting? heapitems))])
      (ht-append
        padding
       (apply vl-append padding
              (map pict-of-heapitem envs))
       (apply vl-append padding
              (map pict-of-heapitem others)))))

(define (field-label name)
    (white (text name)))

(define (bg color p)
    (cc-superimpose
     (filled-rectangle
      (pict-width p)
      (pict-height p)
      #:draw-border?
      #f
      #:color color)
     p))
(define (pad n p)
  (hc-append (blank n)
             (vc-append (blank n) p (blank n))
             (blank n)))
(define padding 5)

(define (pict-of-state state)
  (local [(define-values (the-heap other-state) state)]
    (type-case OtherState state
      [(before-call fun arg* env ectx stack clos-env arg-x* def* body)
       (standard-pict
        (pict-before-call
         (pict-of-e (t-app (t-quote fun) (map t-quote arg*)))
         (pict-of-env env)
         (pict-of-ectx ectx))
        (pict-of-stack stack)
        (pict-of-heap the-heap))]
      [(after-call e env ectx stack)
       (standard-pict
        (pict-after-call
         (pict-of-e e)
         (pict-of-env env)
         (pict-of-ectx ectx))
        (pict-of-stack stack)
        (pict-of-heap the-heap))]
      [(return v env ectx stack)
       (standard-pict
        (pict-after-return
         (pict-of-v v)
         (pict-of-env env)
         (pict-of-ectx ectx))
        (pict-of-stack stack)
        (pict-of-heap the-heap))]
      [(ref x env ectx stack)
       (standard-pict
        (pict-before-ref
         (pict-of-x x)
         (pict-of-env env)
         (pict-of-ectx ectx))
        (pict-of-stack stack)
        (pict-of-heap the-heap))]
      [(s-finish v*)
       (finish-pict v* (pict-of-heap the-heap))]
      [(s-error)
       (error-pict (pict-of-heap the-heap))])))

(define color-blue (make-object color% 68 119 170))
(define color-cyne (make-object color% 102 204 238))
(define color-green (make-object color% 34 136 51))
(define color-yellow (make-object color% 204 187 68))
(define color-black (make-object color% 0 0 0))
(define color-purple (make-object color% 170 51 119))
(define color-grey (make-object color% 187 187 187))
(define color-red (make-object color% 238 102 119))

;;; The color palette is from https://personal.sron.nl/~pault/#fig:scheme_bright
;;; dark blue
(define color-stack-item color-blue)
;;; light blue
(define color-stack-bg color-cyne)
;;; green
(define color-env color-green)
;;; yellow
(define color-closure color-yellow)
;;; black
(define color-vector color-black)
;;; purple
(define color-cons color-purple)
;;; grey
(define color-other color-grey)
;; A special color
(define color-error color-red)

(define color-comp color-closure)
(define color-return color-stack-item)
(define color-terminate color-stack-bg)
(define color-refer color-other)

(define (text s)
  (apply vl-append (map (lambda (s) (pict-text s 'modern)) (string-split s "\n"))))

(define (standard-pict info stack heap)
  (ht-append padding
             (vl-append padding
                        stack
                        info)
             heap))
(define (field name value)
    (ht-append padding (field-label name) (field-value value)))

(define (pict-before-call e env ectx)
  (bg color-stack-item
      (frame
       (pad padding
            (vl-concat padding
                       (list
                        (field "Calling" e)
                        (field "Context" ectx)
                        (field "Environment @" env)))))))

#;
(define (pict-of-state hide-closure? hide-env-lable?)
  (define (pict-of-focus focus)
    (match focus
      [`("Computing" ,term)
       (box (field "Computing" term) color-comp)]
      [`("Returned" ,term)
       (box (field "Returned" term) color-return)]
      [`("Terminated" ,term)
       (box (field "Terminated" term) color-terminate)]
      [`("Referring to" ,term)
       (box (field "Returned" term) color-refer)]))

  (define (pict-of-state state)
    (define p (match state
                [`("Errored" ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack empty)
                                           (box (field-label "Errored") color-error))
                                (pict-of-heap heap)))]
                [`("Terminated" ,o* ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack empty)
                                           (pict-of-focus `("Terminated" (,block ,@o*))))
                                (pict-of-heap heap)))]
                [`(,message ,term ,env ,ectx ,stack ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack (cons (list env ectx term) stack))
                                           (pict-of-focus `(,message ,term)))
                                (pict-of-heap heap)))]))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (min (/ 700 (pict-height p)) (/ 1200 (pict-width p)))))

  (define (pict-of-stack stack)
    (box
     (apply vl-append
            (field-label "Stack")
            (map pict-of-sf (reverse stack)))
     color-stack-bg))

  (define (is-env? heapitem)
    (match-define (list addr hv) heapitem)
    (match hv
      [`(Environment ,@_)
       #t]
      [else
       #f]))

  (define (pict-of-heap heap)
    (pict-of-heapitems heap))

  (define (heapitem-interesting? item)
    (match-define `(,this-addr ,hv) item)
    (and (string? this-addr) ;; string addresses means the address is not primitive (symbol address)
         (if hide-closure?
             (not (is-closure? hv))
             #t)))
  (define (is-closure? hv)
    (match hv
      [`(Closure ,@_) #t]
      [else #f]))

  (define (pict-of-heapitems heapitems)
    (let-values ([(envs others) (partition is-env?
                                           (filter heapitem-interesting? heapitems))])
      (ht-append
        padding
       (apply vl-append padding
              (map pict-of-heapitem envs))
       (apply vl-append padding
              (map pict-of-heapitem others)))))
  (define (color-of-environment addr)
    (define addr-as-num (string->number addr))
    (define ratio (/ (- addr-as-num 1000) 1000))
    (define green (* ratio 200))
    (make-object color% 0 (round green) 0))
  (define (pict-of-heapitem item)
    (match-define `(,this-addr ,hv) item)
    (match hv
      [`(Environment ,bindings ,outer-addr)
       (plate (vl-append
               (field "@" (immediate this-addr))
               (if hide-env-lable?
                   (blank)
                   (white (text "Environment Frame")))
               (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key (compose symbol->string car))))))
               (field "Rest @" (immediate outer-addr)))
              color-env)]
      [`(Closure ,env ,name ,code)
       (plate (vl-append padding
                         (field "@" (immediate this-addr))
                         (field "Environment @" env)
                         (field "Code" (string-of-s-exp code)))
              color-closure)]
      [`,vec
       #:when (vector? vec)
       (plate (vl-append padding
                         (field "@" (immediate this-addr))
                         (field-pict "mvec" (apply hb-append padding (map field-value (vector->list vec)))))
              color-vector)]
      [`(Cons ,v1 ,v2)
       (plate (vl-append padding
                         (field "@" (immediate this-addr))
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2)))))
              color-cons)]
      [else
       (plate (vl-append padding
                         (field "@" (immediate this-addr))
                         (field "content" hv))
              color-other)]))
  (define (plate p color)
    (define w (pict-width p))
    (define h (pict-height p))
    (cc-superimpose
     (filled-ellipse
      (* (sqrt 2) (+ (pict-width p) 6))
      (* (sqrt 2) (+ (pict-height p) 6))
      #:color color)
     p))

  (define (box p color)
    (frame (bg color (pad padding p))))
  (define (pict-of-binding binding)
    (match-define (list x v) binding)
    (ht-append padding
               (field-value x)
               (field-label "↦")
               (field-value v)))

  (define padding 5)

  (struct immediate (it))

  (define (field name value)
    (ht-append padding (white (text name)) (field-value value)))
  (define (field-label name)
    (white (text name)))
  (define (field-value value)
    (bg "white" (text (if (immediate? value) (format "~a" (immediate-it value)) (string-of-s-exp value)))))
  (define (field-pict name p)
    (ht-append padding (white (text name)) p))

  (define (bg color p)
    (cc-superimpose (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color color) p))

  (define (pict-of-sf sf)
    (match-define (list env ectx ann) sf)
    (bg color-stack-item
        (frame
         (pad padding
              (vl-append padding
                         (field "Context" ectx)
                         (field "Environment @" env))))))

  (define (pad n p)
    (hc-append (blank n)
               (vc-append (blank n) p (blank n))
               (blank n)))

  pict-of-state)