#lang racket/gui

(require "snake.rkt" racket/undefined)

(define-values (GRID-SIZE WORLD-SIZE SNAKE-SPEED) (values 20 30 360))
(define WINDOW-SIZE (* GRID-SIZE WORLD-SIZE))

;; variables
(define-values (snake-stopped? snake-failed? snake snake-food snake-direction)
  (values #f undefined undefined undefined 'N))

;; window
(define snake-game
  (new frame% [label "SNAKE"] [width WINDOW-SIZE] [height WINDOW-SIZE]))

;; initialize
(define (snake-game-init)
  (let* ([xy (quotient WORLD-SIZE 2)])
    (set! snake-stopped? #f) (set! snake-failed? #f)
    (set! snake (make-object snake+c% (grid GRID-SIZE GRID-SIZE) xy xy))
    (set! snake-food (random-food)) (send snake-timer start SNAKE-SPEED)))

;; failed
(define (snake-failed-dialog)
  (let* ([failed-text "GAME OVER"]
         [tips-text "PRESS ENTER TO RESTART"]
         [failed-font (make-object font% 20 'system)]
         [dc (send snake-canvas get-dc)])
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
      (send dc set-brush color style)
      (send dc draw-rectangle 0 y WINDOW-SIZE dialog-height)
      (send dc set-font failed-font)
      (send dc set-text-foreground (send (send dc get-pen) get-color))
      (send dc draw-text failed-text failed-x failed-y)
      (send dc draw-text tips-text tips-x tips-y))))

(define-struct food (x y color) #:transparent)

;; generate a food that has a random position and color
(define (random-food)
  (begin
    (define world (make-vector (* WORLD-SIZE WORLD-SIZE)))
    (for ([i (in-range WORLD-SIZE)] #:when #t [j (in-range WORLD-SIZE)])
      (vector-set! world (+ j (* WORLD-SIZE i)) (list j i)))
    (let ([body (get-snake-body snake)])
      (for/list ([node (in-list body)])
        (set! world
              (vector-filter-not
               (lambda (item) (and (eq? (first item) (node-x node))
                                   (eq? (last item) (node-y node))))
               world))))
    (let* ([seed (random (sub1 (vector-length world)))]
           [point (vector-ref world seed)])
      (food (first point) (last point)
            (make-object color% (random #xff) (random #xff) (random #xff))))))

;; guide
(define snake-guide? #f)

(define (snake-paint-callback canvas dc)
  (let* ([food-color (food-color snake-food)]
         [grid-color (make-object color% #xcc #xcc #xcc)]
         [style 'solid]
         [food-x (* GRID-SIZE (food-x snake-food))]
         [food-y (* GRID-SIZE (food-y snake-food))])
    (send dc set-pen grid-color 1 style)
    ;; draw grid
    (for ([i (in-range WORLD-SIZE)])
      (send dc draw-line 0 (* GRID-SIZE i) WINDOW-SIZE (* GRID-SIZE i))
      (send dc draw-line (* GRID-SIZE i) 0 (* GRID-SIZE i) WINDOW-SIZE))
    ;; draw snake
    (send snake draw canvas dc)
    (send dc set-pen food-color 1 style)
    (send dc set-brush food-color style)
    ;; draw food
    (send dc draw-ellipse food-x food-y GRID-SIZE GRID-SIZE)
    ;; draw guide
    (when snake-guide?
      (let* ([guide-color (make-object color% 0 #xff 0)]
             [guide-style 'long-dash]
             [old-pen (send dc get-pen)]
             [head (first (get-snake-body snake))]
             [x1 (+ food-x (/ GRID-SIZE 2))]
             [y1 (+ food-y (/ GRID-SIZE 2))]
             [x2 (+ (* (node-x head) GRID-SIZE) (/ GRID-SIZE 2))]
             [y2 (+ (* (node-y head) GRID-SIZE) (/ GRID-SIZE 2))])
        (send dc set-pen (send old-pen get-color) 1 guide-style)
        (send dc draw-line x1 y1 x2 y2)
        (send dc set-pen old-pen)))))

;; pause
(define (snake-stopped-dialog)
  (let* ([pause-text "GAME PAUSE"]
         [pause-font (make-object font% 20 'system)]
         [dc (send snake-canvas get-dc)])
    (send dc set-font pause-font)
    (define-values (pause-width pause-height pause-distance pause-extra)
      (send dc get-text-extent pause-text))
    (let* ([x (/ (- WINDOW-SIZE pause-width) 2)]
           [y (/ (- WINDOW-SIZE pause-height) 3)]
           [dialog-height (* pause-height 3)]
           [color (make-object color% #xff #xff #xff)]
           [style 'solid]
           [dc (send snake-canvas get-dc)])
      (send dc set-brush color style)
      (send dc draw-rectangle 0 (- y pause-height) WINDOW-SIZE dialog-height)
      (send dc set-text-foreground (send (send dc get-pen) get-color))
      (send dc draw-text pause-text x y))))

(define snake-canvas
  (new (class canvas%
         (super-new)
         ;; handling keyboard events
         (define/override (on-char ch)
           (case (send ch get-key-code)
             [(#\w    up) (set! snake-direction 'N)]
             [(#\s  down) (set! snake-direction 'S)]
             [(#\a  left) (set! snake-direction 'W)]
             [(#\d right) (set! snake-direction 'E)]
             [(#\return )
              (if snake-failed?
                  (snake-game-init)
                  ;; pause
                  (if (not snake-stopped?)
                      (begin (set! snake-stopped? #t)
                             (send snake-timer stop) (snake-stopped-dialog))
                      (begin (set! snake-stopped? #f)
                             (send snake-timer start SNAKE-SPEED))))])))
       [parent snake-game]
       [paint-callback snake-paint-callback]))

;; active canvas
(send snake-canvas focus)

(define snake-timer
  (make-object timer%
    (lambda ()
      ;; update position 
      (send snake move snake-direction)
      (define-values (head nodes)
        (values (first (get-snake-body snake)) (rest (get-snake-body snake))))
      (define-values (hx hy fx fy fc)
        (values (node-x head) (node-y head)
                (food-x snake-food) (food-y snake-food) (food-color snake-food)))
      ;; eating food
      (when (and (eq? hx fx) (eq? hy fy))
        (send snake grow fc) (set! snake-food (random-food))
        (set! fx (food-x snake-food)) (set! fy (food-y snake-food)))
      ;; check bound
      (unless (and (>= hy 0) (<= hy (sub1 WORLD-SIZE))
                   (>= hx 0) (<= hx (sub1 WORLD-SIZE))
                   (empty?
                    (filter (lambda (node)
                              (and (eq? (node-x node) hx)
                                   (eq? (node-y node) hy)))
                            nodes)))
        (set! snake-failed? #t) (send snake-timer stop) (snake-failed-dialog))
      ;; check if should show or not show guide
      (let* ([min-x (min fx hx)] [max-x (max fx hx)]
             [min-y (min fy hy)] [max-y (max fy hy)])
        (cond
          [(eq? min-x max-x)
           (if (empty?
                (filter (lambda (node)
                          (let* ([nx (node-x node)] [ny (node-y node)])
                            (and (eq? nx max-x) (> ny min-y) (< ny max-y))))
                        nodes))
             (set! snake-guide? #t) (set! snake-guide? #f))]
          [(eq? min-y max-y)
           (if (empty?
                (filter (lambda (node)
                          (let* ([nx (node-x node)] [ny (node-y node)])
                            (and (eq? ny max-y) (> nx min-x) (< nx max-x))))
                        nodes))
             (set! snake-guide? #t) (set! snake-guide? #f))]
          [else (set! snake-guide? #f)]))
      ;; update canvas
      (unless (or snake-failed? snake-stopped?) (send snake-canvas refresh-now)))))

;; bootstrap
(snake-game-init)
(send snake-game show #t)