#lang racket/gui

(provide snake+c% grid node-x node-y get-snake-x get-snake-y get-snake-body)

;; grid struct
(define-struct/contract grid ([width positive?] [height positive?]) #:transparent)

;; direction contract
(define direction/c (symbols 'N 'S 'W 'E))

;; node struct
(define-struct (node grid) (x y color direction) #:transparent #:mutable)

;; move function
(define (move-snake snake direction)
  (begin
    (define body (get-snake-body snake))
    (let change-direction ([rbody (reverse body)])
      (unless (< (length rbody) 2)
        (set-node-direction! (first rbody) (node-direction (second rbody)))
        (change-direction (rest rbody))))
    (set-node-direction! (first body) direction)
    (let go-forward ([nodes body])
      (unless (zero? (length nodes))
        (let* ([current (first nodes)])
          (case (node-direction current)
            [(N) (set-node-y! current (sub1 (node-y current)))]
            [(S) (set-node-y! current (add1 (node-y current)))]
            [(W) (set-node-x! current (sub1 (node-x current)))]
            [(E) (set-node-x! current (add1 (node-x current)))]))
        (go-forward (rest nodes))))
    (set-snake-x! snake (node-x (first body)))
    (set-snake-y! snake (node-y (first body)))))

;; interface function
(define (draw-snake snake canvas dc)
  (begin
    (define-values (body old-pen old-brush PEN-WIDTH STYLE)
      (values (get-snake-body snake) (send dc get-pen) (send dc get-brush) 1 'solid))
    (define (draw nodes)
      (unless (zero? (length nodes))
        (let* ([node (first nodes)]
               [width (grid-width node)]
               [height (grid-height node)]
               [x (* (node-x node) width)]
               [y (* (node-y node) height)]
               [color (node-color node)]
               [style 'solid])
          (send dc set-pen color 1 style)
          (send dc set-brush color style)
          (send dc draw-ellipse x y width height) (draw (rest nodes)))))
    (define-values (head nodes) (values (list (first body)) (rest body)))
    (draw (list (first body)))
    (draw (rest body))
    (send dc set-pen old-pen)
    (send dc set-brush old-brush)))

;; initial direction
(define HEAD-DIRECTION 'N)

;; snake class
(define snake%
  (class* object% ((interface () [draw (->m (is-a?/c canvas%) (is-a?/c dc<%>) any)]))
    (super-new)
    (init-field size x y)
    (field [body (let* ([width (grid-width size)]
                        [height (grid-height size)]
                        [HEAD-COLOR (make-object color% 0 0 0)])
                   (list (node width height x y HEAD-COLOR HEAD-DIRECTION)))])
    (grow (make-object color% (random #xff) (random #xff) (random #xff)))
    (define/public (grow color)
      (let ([tail (struct-copy node (last body))])
        (case (node-direction tail)
          [(N) (set-node-y! tail (add1 (node-y tail)))]
          [(S) (set-node-y! tail (sub1 (node-y tail)))]
          [(W) (set-node-x! tail (add1 (node-x tail)))]
          [(E) (set-node-x! tail (sub1 (node-x tail)))])
        (begin (set-node-color! tail color) (set! body (append body (list tail))))))
    (define/public (draw canvas dc) (draw-snake this canvas dc))
    (define/public (move direction) (move-snake this direction))))

(define-values (get-snake-x set-snake-x!)
  (values (class-field-accessor snake% x) (class-field-mutator snake% x)))

(define-values (get-snake-y set-snake-y!)
  (values (class-field-accessor snake% y) (class-field-mutator snake% y)))

(define get-snake-body (class-field-accessor snake% body))

;; snake class with contract
(define/contract snake+c%
  (class/c (init-field [size grid?]
                       [x exact-nonnegative-integer?]
                       [y exact-nonnegative-integer?])
           (field [body list?])
           [grow (->m (is-a?/c color%) any)]
           [move (->m direction/c any)])
  snake%)
