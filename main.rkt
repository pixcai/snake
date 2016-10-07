#lang racket/gui

(require "snake.rkt" racket/undefined)

;; width and height
(define-values (GRID-SIZE WORLD-SIZE) (values 20 30))
(define WINDOW-SIZE (* GRID-SIZE WORLD-SIZE))

;; game variable
(define-values (snake-failed? snake snake-food snake-direction)
  (values undefined undefined undefined 'N))

;; game window
(define snake-game
  (new frame% [label "SNAKE"] [width WINDOW-SIZE] [height WINDOW-SIZE]))

;; game initialize
(define (snake-game-init)
  (let* ([failed? #f] [point (quotient WORLD-SIZE 2)] [INTERVAL 300])
    (set! snake-failed? failed?)
    (set! snake (make-object snake+c% (grid GRID-SIZE GRID-SIZE) point point))
    (set! snake-food (random-food))
    (send snake-timer start INTERVAL)))

;; game failed dialog
(define (snake-failed-callback canvas dc)
  (let* ([failed-text "GAME OVER"]
         [tips-text "press enter to restart"]
         [failed-font (make-object font% 20 'system)])
    (send dc set-font failed-font)
    (define-values (failed-width failed-height failed-distance failed-extra)
      (send dc get-text-extent failed-text))
    (define-values (tips-width tips-height tips-distance tips-extra)
      (send dc get-text-extent tips-text))
    (let* ([block-height (+ failed-height tips-height)]
           [dialog-height (* block-height 3)]
           [y (/ (- WINDOW-SIZE dialog-height) 3)]
           [failed-x (/ (- WINDOW-SIZE failed-width) 2)]
           [failed-y (/ (- WINDOW-SIZE failed-height) 3)]
           [tips-x (/ (- WINDOW-SIZE tips-width) 2)]
           [tips-y (+ failed-y failed-height tips-height)]
           [color (make-object color% #xff #xff #xff)]
           [style 'solid])
      (send dc set-pen color 1 style)
      (send dc set-brush color style)
      (send dc draw-rectangle 0 y WINDOW-SIZE dialog-height)
      (send dc set-font failed-font)
      (send dc set-pen (make-object color% 0 0 0) 1 style)
      (send dc draw-text failed-text failed-x failed-y)
      (send dc draw-text tips-text tips-x tips-y))))

(define-struct food (x y color) #:transparent)

(define (random-food)
  (begin
    (define world (make-vector (* WORLD-SIZE WORLD-SIZE)))
    (for ([i (in-range WORLD-SIZE)] #:when #t [j (in-range WORLD-SIZE)])
      (vector-set! world (+ j (* WORLD-SIZE i)) (list j i)))
    (let ([body (get-snake-body snake)])
      (for/list ([node (in-list body)])
        (set! world (vector-filter-not
                     (lambda (item) (and (eq? (first item) (node-x node))
                                         (eq? (last item) (node-y node))))
                     world))))
    (let* ([seed (- (random (vector-length world)) 1)]
           [point (vector-ref world seed)])
      (food (first point) (last point) (make-object color%
                                         (random #xff)
                                         (random #xff)
                                         (random #xff))))))

;; helper line
(define show-line? #f)

(define (snake-paint-callback canvas dc)
  (let* ([brush-color (food-color snake-food)]
         [pen-color (make-object color% #xcc #xcc #xcc)]
         [style 'solid]
         [food-x (* GRID-SIZE (food-x snake-food))]
         [food-y (* GRID-SIZE (food-y snake-food))])
    (send dc set-pen pen-color 1 style)
    (for ([i (in-range WORLD-SIZE)])      
      (send dc draw-line 0 (* GRID-SIZE i) WINDOW-SIZE (* GRID-SIZE i))
      (send dc draw-line (* GRID-SIZE i) 0 (* GRID-SIZE i) WINDOW-SIZE))
    (send snake draw canvas dc)
    (send dc set-pen brush-color 1 style)
    (send dc set-brush brush-color style)
    (send dc draw-ellipse food-x food-y GRID-SIZE GRID-SIZE)
    (unless (not show-line?)
      (let* ([color (make-object color% 0 #xff 0)]
             [style 'long-dash]
             [old-pen (send dc get-pen)]
             [head (first (get-snake-body snake))]
             [x1 (+ food-x (/ GRID-SIZE 2))]
             [y1 (+ food-y (/ GRID-SIZE 2))]
             [x2 (+ (* (node-x head) GRID-SIZE) (/ GRID-SIZE 2))]
             [y2 (+ (* (node-y head) GRID-SIZE) (/ GRID-SIZE 2))])
        (send dc set-pen color 1 style)
        (send dc draw-line x1 y1 x2 y2)
        (send dc set-pen old-pen)))
    (unless (not snake-failed?) (snake-failed-callback canvas dc))))

(define snake-canvas
  (new (class canvas%
         (super-new)
         (define/override (on-char ch)
           (case (send ch get-key-code)
             [(up) (set! snake-direction 'N)]
             [(down) (set! snake-direction 'S)]
             [(left) (set! snake-direction 'W)]
             [(right) (set! snake-direction 'E)]
             [(#\return) (unless (not snake-failed?) (snake-game-init))])))
       [parent snake-game]
       [paint-callback snake-paint-callback]))

;; active canvas
(send snake-canvas focus)

(define snake-timer
  (make-object timer%
    (lambda ()
      (send snake move snake-direction)
      (define head (first (get-snake-body snake)))
      (define-values (fx hx fy hy)
        (values (food-x snake-food) (node-x head)
                (food-y snake-food) (node-y head)))
      (unless (not (and (eq? hx fx) (eq? hy fy)))
        (send snake grow (food-color snake-food))
        (set! snake-food (random-food)))
      (begin (set! fx (food-x snake-food)) (set! fy (food-y snake-food)))
      (unless (and (>= (get-snake-y snake) 0)
                   (<= (get-snake-y snake) (sub1 WORLD-SIZE))
                   (>= (get-snake-x snake) 0)
                   (<= (get-snake-x snake) (sub1 WORLD-SIZE))
                   (empty?
                    (filter (lambda (item)
                              (and (eq? (node-x item) (node-x head))
                                   (eq? (node-y item) (node-y head))))
                            (rest (get-snake-body snake)))))
        (begin (send snake-timer stop) (set! snake-failed? #t)))
      (let* ([min-x (min fx hx)]
             [max-x (max fx hx)]
             [min-y (min fy hy)]
             [max-y (max fy hy)]
             [nodes (rest (get-snake-body snake))])
        (cond
          [(eq? fx hx)
           (unless
               (not (empty? (filter
                        (lambda (node)
                          (let* ([nx (node-x node)] [ny (node-y node)])
                            (and (eq? nx fx) (> ny min-y) (< ny max-y))))
                        nodes)))
             (set! show-line? #t))]
          [(eq? fy hy)
           (unless
               (not (empty? (filter
                        (lambda (node)
                          (let* ([nx (node-x node)] [ny (node-y node)])
                            (and (eq? ny fy) (> nx min-x) (< nx max-x))))
                        nodes)))
             (set! show-line? #t))]
          [else (set! show-line? #f)]))
      (send snake-canvas refresh-now))))

(snake-game-init)
(send snake-game show #t)