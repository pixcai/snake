#lang racket/gui

(require "snake.rkt")

(struct size (width height) #:mutable)

(define-values (cell world) (values (size 20 20) (size 30 30)))

(define frame
  (size (* (size-width cell) (size-width world)) (* (size-height cell) (size-height world))))

(define snake-frame
  (new frame%
       [label "SNAKE"]
       [width (size-width frame)]
       [height (size-height frame)]
       [min-width (size-width frame)]
       [min-height (size-height frame)]))

(define-struct/contract (snake-food-with-type snake-food+c)
  ([type (symbols 'nul 'speed 'penetrate)]))

(define snake%
  (class snake+c+abstract%
    (inherit-field head body)
    (define/override (eat food)
      (case (snake-food-with-type-type food)
        [(nul) (begin (when snake-speedup?
                        (send snake-timer stop)
                        (send snake-timer start snake-speed))
                      (when (or snake-speedup? snake-penetrating?)
                        (send snake unset-body-color)
                        (when snake-speedup?
                          (set! snake-speedup? #f))
                        (when snake-penetrating?
                          (set! snake-penetrating? #f)))
                      (super eat food))]
        [(speed) (begin (if snake-penetrating?
                          (begin (send snake unset-body-color)
                                 (send snake set-body-color (send the-color-database find-color "blue")))
                          (begin (unless snake-speedup?
                                   (send snake set-body-color (send the-color-database find-color "green"))
                                   (send snake-timer stop)
                                   (send snake-timer start (/ snake-speed 4)))))
                        (set! snake-speedup? #t))]
        [(penetrate) (begin (if snake-speedup?
                              (begin (send snake unset-body-color)
                                     ((send snake set-body-color (send the-color-database find-color "blue"))))
                              (begin (unless snake-penetrating?
                                       (send snake set-body-color (send the-color-database find-color "red")))))
                            (set! snake-penetrating? #t))]))
    (define/override (draw dc)
      (let ([cell-width (size-width cell)]
            [cell-height (size-height cell)]
            [old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)])
        (send dc set-pen (snake-node-color head) (/ (min cell-width cell-height) 4) 'solid)
        (send dc set-brush (snake-node-color head) 'transparent)
        (send dc draw-ellipse (* (snake-node-x head) cell-width) (* (snake-node-y head) cell-height) cell-width cell-height)
        (send dc set-pen old-pen)
        (for/list ([node (in-list body)])
          (send dc set-brush (snake-node-color node) 'solid)
          (send dc draw-ellipse (* (snake-node-x node) cell-width) (* (snake-node-y node) cell-height) cell-width cell-height))
        (send dc set-brush old-brush)))
    (super-new)))

(require racket/undefined)

(define snake undefined)

(define (random-snake-food)
  (let-values ([(world-width world-height) (values (size-width world) (size-height world))])
    (define food-position
      (let ([maybe-positions empty])
        (for ([x (in-range world-width)] #:when #t [y (in-range world-height)])
          (when (empty? (filter (lambda (node) (and (= (snake-node-x node) x) (= (snake-node-y node) y)))
                                (send snake get-nodes)))
            (set! maybe-positions (append maybe-positions (list (cons x y))))))
        (list-ref maybe-positions (random (length maybe-positions)))))
    (make-snake-food-with-type (car food-position)
                               (cdr food-position)
                               (make-object color% (random 255) (random 255) (random 255))
                               (let ([p (random 100)])
                                 (cond [(<= p 10) 'speed]
                                       [(and (> p 10) (< p 90)) 'nul]
                                       [(>= p 90) 'penetrate])))))

(define-values (snake-food
                snake-speed
                snake-direction
                snake-init?
                snake-died?
                snake-pause?
                snake-guide?
                snake-speedup?
                snake-penetrating?)
  (values undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined))

(define (snake-init)
  (begin
    (set! snake (make-object snake% (quotient (size-width world) 2) (quotient (size-height world) 2)))
    (set!-values (snake-food
                  snake-speed
                  snake-direction
                  snake-init?
                  snake-died?
                  snake-pause?
                  snake-guide?
                  snake-speedup?
                  snake-penetrating?)
                 (values (random-snake-food) 320 'up #t #f #f #f #f #f))))

(define (draw-dialog dc title text color)
  (let*-values ([(title-font text-font)
                (values (make-object font% 22 'default) (make-object font% 16 'default))]
               [(title-width title-height title-distance title-extra)
                (send dc get-text-extent title title-font)]
               [(text-width text-height text-distance text-extra)
                (send dc get-text-extent text text-font)]
               [(frame-width frame-height)
                (values (size-width frame) (size-height frame))])
    (when (equal? color #f)
      (set! color (make-object color% 0 0 0)))
    (define old-pen (send dc get-pen))
    (send dc set-pen color 1 'solid)
    (send dc draw-rectangle 0 (* 1/3 frame-height) frame-width (* 1/3 frame-height))
    (send dc set-font title-font)
    (send dc draw-text title (* 1/2 (- frame-width title-width)) (- (* 5/12 frame-height) (* 1/2 title-height)))
    (send dc set-font text-font)
    (send dc draw-text text (* 1/2 (- frame-width text-width)) (- (* 7/12 frame-height) text-height))
    (send dc set-pen old-pen)))

(define (snake-paint-callback canvas dc)
  (let ([cell-width (size-width cell)]
        [cell-height (size-height cell)]
        [frame-width (size-width frame)]
        [frame-height (size-height frame)])
    (send dc set-smoothing 'aligned)
    (let draw-cell ([old-pen (send dc get-pen)]
                    [cell-color (send the-color-database find-color "lightgray")])
      (send dc set-pen cell-color 1 'solid)
      (for ([i (in-range (size-width world))] #:when #t [j (in-range (size-height world))])
        (send dc draw-line 0 (* cell-height j) frame-width (* cell-height j))
        (send dc draw-line (* cell-width i) 0 (* cell-width i) frame-height))
      (send dc set-pen old-pen))
    (if snake-init?
        (draw-dialog dc "SNAKE" "PRESS ENTER TO START" #f)
        (begin
          (send snake draw dc)
          (let draw-snake-food ([food-x (* (snake-food+c-x snake-food) cell-width)]
                                [food-y (* (snake-food+c-y snake-food) cell-height)]
                                [old-brush (send dc get-brush)])
            (send dc set-brush (snake-food+c-color snake-food) 'solid)
            (if (equal? 'nul (snake-food-with-type-type snake-food))
                (send dc draw-ellipse food-x food-y cell-width cell-height)
                (send dc draw-rectangle food-x food-y cell-width cell-height))
            (send dc set-brush old-brush))
          (when snake-guide?
            (let* ([old-pen (send dc get-pen)]
                   [guide-color (send the-color-database find-color "black")]
                   [food-center-x (* (+ (snake-food+c-x snake-food) 1/2) cell-width)]
                   [food-center-y (* (+ (snake-food+c-y snake-food) 1/2) cell-height)]
                   [head (first (send snake get-nodes))]
                   [head-center-x (* (+ (snake-node-x head) 1/2) cell-width)]
                   [head-center-y (* (+ (snake-node-y head) 1/2) cell-height)])
              (send dc set-pen guide-color 4 'solid)
              (send dc draw-point food-center-x food-center-y)
              (send dc draw-point head-center-x head-center-y)
              (send dc set-pen guide-color 1 'long-dash)
              (send dc draw-line food-center-x food-center-y head-center-x head-center-y)
              (send dc set-pen old-pen)))
          (when snake-pause?
            (draw-dialog dc "GAME PAUSE" "PRESS ENTER TO CONTINUE" #f))
          (when snake-died?
            (draw-dialog dc "GAME OVER" "PRESS ENTER TO RESTART" #f))))))

(define snake-timer
  (make-object timer%
    (lambda ()
      (let ([world-width (size-width world)]
            [world-height (size-height world)]
            [head (first (send snake get-nodes))])
        (send snake move snake-direction)
        (define-values (nodes head-x head-y)
          (values (rest (send snake get-nodes)) (snake-node-x head) (snake-node-y head)))
        (let ([food-x (snake-food+c-x snake-food)]
              [food-y (snake-food+c-y snake-food)])
          (when (and (= head-x food-x)
                     (= head-y food-y))
            (send snake eat snake-food)
            (set! snake-food (random-snake-food))
            (set! food-x (snake-food+c-x snake-food))
            (set! food-y (snake-food+c-y snake-food)))
          (let ([guide? #f])
            (when (= head-x food-x)
              (set! guide? (empty? (filter
                                    (lambda (node)
                                      (and (= (snake-node-x node) head-x)
                                           (> (snake-node-y node) (min head-y food-y))
                                           (< (snake-node-y node) (max head-y food-y))))
                                    nodes))))
            (when (= head-y food-y)
              (set! guide? (empty? (filter
                                    (lambda (node)
                                      (and (= (snake-node-y node) head-y)
                                           (> (snake-node-x node) (min head-x food-x))
                                           (< (snake-node-x node) (max head-x food-x))))
                                    nodes))))
            (if guide?
                (set! snake-guide? #t)
                (set! snake-guide? #f))))
        (if snake-penetrating?
            (begin (cond
                     [(> head-x world-width) (set-snake-node-x! head 0)]
                     [(< head-x 0) (set-snake-node-x! head (sub1 world-width))]
                     [(> head-y world-height) (set-snake-node-y! head 0)]
                     [(< head-y 0) (set-snake-node-y! head (sub1 world-height))]))
            (unless (and (empty? (filter
                              (lambda (node)
                                (and (= (snake-node-x node) head-x)
                                     (= (snake-node-y node) head-y)))
                              nodes))
                     (>= head-x 0)
                     (< head-x world-width)
                     (>= head-y 0)
                     (< head-y world-height))
              (set! snake-died? #t)))
        (send snake-canvas refresh-now)
        (when snake-died?
          (send snake-timer stop))))))

(define snake-canvas%
  (class canvas%
    (define/override (on-char ch)
      (case (send ch get-key-code)
        [(#\a left) (unless (equal? snake-direction 'right)
                      (set! snake-direction 'left))]
        [(#\w up) (unless (equal? snake-direction 'down)
                    (set! snake-direction 'up))]
        [(#\d right) (unless (equal? snake-direction 'left)
                       (set! snake-direction 'right))]
        [(#\s down) (unless (equal? snake-direction 'up)
                      (set! snake-direction 'down))]
        [(#\return) (begin (if snake-init?
                             (begin (snake-init)
                                    (set! snake-init? #f)
                                    (send snake-timer start snake-speed))
                             (if snake-pause?
                                 (begin (set! snake-pause? #f)
                                        (send snake-timer start snake-speed))
                                 (begin (set! snake-pause? #t)
                                        (send snake-timer stop))))
                           (when snake-died?
                             (snake-init)
                             (set! snake-died? #f)
                             (send snake-timer start snake-speed))
                           (send snake-canvas refresh-now))]))
    (super-new)))

(define snake-canvas
  (make-object snake-canvas% snake-frame null snake-paint-callback))

(send snake-canvas focus)
(send snake-frame show #t)