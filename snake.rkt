#lang racket

(provide snake-food+c
         snake-food+c-x snake-food+c-y snake-food+c-color
         snake+c+abstract%
         set-snake-node-x! set-snake-node-y!
         snake-node-x snake-node-y snake-node-color)

(require racket/draw)

(define-struct/contract snake-food+c
  ([x exact-integer?] [y exact-integer?] [color (is-a?/c color%)]))

(define snake-direction? (symbols 'left 'up 'right 'down))

(define-struct/contract snake-node
  ([x exact-integer?] [y exact-integer?] [color (is-a?/c color%)] [color+ (is-a?/c color%)] [direction snake-direction?])
  #:mutable #:transparent)

(define/contract snake+c+abstract%
  (class/c [eat (->m snake-food+c? any)]
           [move (->m snake-direction? any)]
           [set-body-color (->m (is-a?/c color%) any)])
  (class object%
    (super-new)
    (abstract draw)
    (init-field x y)
    (field [head (let ([color (send the-color-database find-color "black")])
                   (make-snake-node x y color color 'up))]
           [body (let ([color (make-object color% (random 255) (random 255) (random 255))])
                   (list (make-snake-node x (add1 y) color color 'up)))])
    (define/public (eat food)
      (let* ([tail (struct-copy snake-node (last body) [color (snake-food+c-color food)])]
             [tail-x (snake-node-x tail)]
             [tail-y (snake-node-y tail)])
        (case (snake-node-direction tail)
          [(left) (set-snake-node-x! tail (add1 tail-x))]
          [(up) (set-snake-node-y! tail (add1 tail-y))]
          [(right) (set-snake-node-x! tail (sub1 tail-x))]
          [(down) (set-snake-node-y! tail (sub1 tail-y))])
        (set! body (append body (list tail)))))
    (define/public (move direction)
      (let move-node ([nodes (reverse body)])
        (unless (zero? (length nodes))
          (let ([node (first nodes)]
                [before (if (> (length nodes) 1) (second nodes) head)])
            (set-snake-node-direction! node (snake-node-direction before))
            (case (snake-node-direction node)
              [(left right) (set-snake-node-x! node (snake-node-x before))]
              [(up down) (set-snake-node-y! node (snake-node-y before))]))
          (move-node (rest nodes))))
      (case direction
        [(left) (set-snake-node-x! head (sub1 (snake-node-x head)))]
        [(up) (set-snake-node-y! head (sub1 (snake-node-y head)))]
        [(right) (set-snake-node-x! head (add1 (snake-node-x head)))]
        [(down) (set-snake-node-y! head (add1 (snake-node-y head)))])
      (set-snake-node-direction! head direction))
    (define/public (set-body-color color)
      (for/list ([node (in-list body)])
        (set-snake-node-color+! node (snake-node-color node))
        (set-snake-node-color! node color)))
    (define/public (unset-body-color)
      (for/list ([node (in-list body)])
        (set-snake-node-color! node (snake-node-color+ node))))
    (define/public (get-nodes) (append (list head) body))
    (define/public (inspect) (print (get-nodes)))))