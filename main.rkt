#lang racket
(require (only-in racket/gui sleep/yield)
         racket/generator
         racket/random
         data/priority-queue
         raylib/generated/unsafe)


;;; Macros
(define-syntax-rule (inc var)
  (set! var (add1 var)))


(define (rectangles-touching? x1 y1 w1 h1 x2 y2 w2 h2)
  (and
   (or (<= x1 x2 (+ x1 w1))
       (<= x2 x1 (+ x2 w2)))
   (or (<= y1 y2 (+ y1 h1))
       (<= y2 y1 (+ y2 h2)))))


;;; Important constants
(define screen-width 1280)
(define screen-height 720)
(define debug-mode? #f)
(define show-hitboxes? #f)
(define game-state 'tutorial)
(define (transition-to new-state)
  (for/list ([e (es)])
    (send e transition new-state))
  (set! game-state new-state))
(define (debug . args)
  (when debug-mode?
    (if (= (length args) 1)
        (println (car args))
        (apply printf args))))


;;; Key definitions for gameplay
;; When used with IsKeyUp/Down/Pressed functions, these refer to key locations, not characters
(define key-up KEY_W)
(define key-down KEY_S)
(define key-left KEY_A)
(define key-right KEY_D)
(define key-shoot KEY_SPACE)


;;; Graphics and textures
(define font (delay (LoadFontEx "tex/SourceCodePro-Medium.ttf" 24 #f 0)))
(define tex (delay (LoadTexture "tex/ship-sheet.png")))
(define shot-tex (delay (LoadTexture "tex/shot.png")))
(define basic-tex (delay (LoadTexture "tex/basic-sheet.png")))
(define clunker-tex (delay (LoadTexture "tex/clunker-sheet.png")))
(define dripper-tex (delay (LoadTexture "tex/dripper.png")))
(define boss-tex (delay (LoadTexture "tex/boss.png")))
(define background-tex (delay (LoadTexture "tex/blackboard.png")))
(define health-background-tex (delay (LoadTexture "tex/health-background.png")))
(define health-frame-tex (delay (LoadTexture "tex/health-frame.png")))
(define health-bars-tex (delay (LoadTexture "tex/health-bars.png")))
(define enemy-shot-tex (delay (LoadTexture "tex/enemy-shot-sheet.png")))
(define explosion-tex (delay (LoadTexture "tex/explosion-sheet.png")))
(define wasd-tutorial-tex (delay (LoadTexture "tex/wasd.png")))
(define space-tutorial-tex (delay (LoadTexture "tex/spaceshoot.png")))
(define game-over-tex (delay (LoadTexture "tex/gameover.png")))
(define chalk-texes
  (vector (list 425 324 (delay (LoadTexture "tex/chalk-angel.png")))
          (list 442 510 (delay (LoadTexture "tex/chalk-heart.png")))
          (list 237 299 (delay (LoadTexture "tex/chalk-icecream.png")))
          (list 236 213 (delay (LoadTexture "tex/chalk-threes.png")))
          (list 187 327 (delay (LoadTexture "tex/chalk-uwu.png")))
          (list 218 158 (delay (LoadTexture "tex/chalk-vv.png")))
          (list 236 213 (delay (LoadTexture "tex/chalk-star.png")))))


(define (eval-mixin %)
  (define ns (module->namespace 'racket))
  (define -eval eval)
  (class % (super-new)
    (define methods (interface->method-names (object-interface this)))
    (define/public (eval str)
      (debug "cmd: ~v~n" str)
      (define op (open-input-string str))
      (define cmds
        (for/list ([cmd (in-producer (λ _ (let ([cmd (read op)])
                                            (if (eof-object? cmd) #f cmd))) #f)])
          cmd))
      (define template
        `(λ (self)
           ,@(for/list ([method methods])
               `(define (,method . args) (send/apply self ,method args)))
           ,@cmds))
      (debug "to eval: ~v~n" template)
      ((-eval template ns) this))))


;;; A thing with a presence in the game world. It can be moved, drawn, ticked, and deleted.
(define entities (make-priority-queue (λ (a b) (< (get-field order a) (get-field order b)))))
;;; A parameter of sorted entities, for faster access to the latest data.
(define es (make-parameter #f))
(define (update-es!)
  (es (priority-queue->sorted-vector entities)))
(update-es!)
(define entity%
  (eval-mixin
   (class object% (super-new)
     (init-field x)
     (init-field y)
     (init-field order)
     (field [dead? #f])

     (define/public (tick)
       (void))

     (define/public (draw)
       (void))

     (define/public (letter chars)
       (void))

     (define/public (transition new-state)
       (void))

     (define/public (die)
       (debug "dead: ~a~n" this%)
       (priority-queue-remove! entities this)
       (set! dead? #t))

     (priority-queue-insert! entities this))))


(define (sprite-mixin %)
  (class % (super-new) (inherit-field x y)
    (init-field width)
    (init-field height)
    (init-field scale)
    (init-field sprite-tex)
    (init-field hitboxes)
    (init-field [sprites-across 1])
    (init-field [sprites-in-sheet 1])
    (init-field [sprite-index 0])

    (define/public (center-x)
      (+ x (/ (* width scale) 2)))

    (define/public (center-y)
      (+ y (/ (* height scale) 2)))

    (define/override (draw)
      (super draw)
      (define u (* width (exact->inexact (modulo sprite-index sprites-across))))
      (define v (* height (exact->inexact (quotient sprite-index sprites-across))))
      (define source (make-Rectangle u v width height))
      (DrawTexturePro (force sprite-tex) ;; texture
                      source
                      (make-Rectangle x y (* width scale) (* height scale)) ;; dest
                      (make-Vector2 0.0 0.0) ;; origin
                      0.0 ;; rotation
                      WHITE)
      (when show-hitboxes?
        (for ([hitbox hitboxes])
          (DrawRectangleRec
           (make-Rectangle
            (+ x (* scale (vector-ref hitbox 0)))
            (+ y (* scale (vector-ref hitbox 1)))
            (* scale (vector-ref hitbox 2))
            (* scale (vector-ref hitbox 3)))
           (make-Color 255 0 0 150)))))

    (define/public (touching? that)
      (define sprites-touching?
        (rectangles-touching?
         (get-field x this) (get-field y this) (* (get-field scale this) (get-field width this)) (* (get-field scale this) (get-field height this))
         (get-field x that) (get-field y that) (* (get-field scale that) (get-field width that)) (* (get-field scale that) (get-field height that))))
      (if (not sprites-touching?)
          #f
          (for*/or ([hitbox1 (get-field hitboxes this)]
                    [hitbox2 (get-field hitboxes that)])
            (rectangles-touching?
             (+ (get-field x this) (* (get-field scale this) (vector-ref hitbox1 0)))
             (+ (get-field y this) (* (get-field scale this) (vector-ref hitbox1 1)))
             (* (get-field scale this) (vector-ref hitbox1 2))
             (* (get-field scale this) (vector-ref hitbox1 3))
             (+ (get-field x that) (* (get-field scale that) (vector-ref hitbox2 0)))
             (+ (get-field y that) (* (get-field scale that) (vector-ref hitbox2 1)))
             (* (get-field scale that) (vector-ref hitbox2 2))
             (* (get-field scale that) (vector-ref hitbox2 3))))))))


(define hitbox-controller%
  (class entity%
    (super-new [x 0.0] [y 0.0] [order 0])

    (define/override (letter chars)
      (super letter chars)
      (when (memq (char->integer #\b) chars)
        (set! show-hitboxes? (not show-hitboxes?))))))

(define (flipper-mixin %)
  (class % (super-new) (inherit-field sprite-index sprites-in-sheet)
    (init-field flipper-ticks)
    (define flipper-count 0)

    (define/override (tick)
      (super tick)
      (set! flipper-count (add1 flipper-count))
      (when (flipper-count . >= . flipper-ticks)
        (set! sprite-index (add1 sprite-index))
        (when (sprite-index . >= . sprites-in-sheet)
          (set! sprite-index 0))
        (set! flipper-count 0)))))


;;; The ship that the player controls.
(define ship%
  (class (flipper-mixin (sprite-mixin entity%))
    (define initial-x 220.0)
    (define initial-y 250.0)
    (define initial-hp 5)
    (super-new [x initial-x]
               [y initial-y]
               [order 40]
               [width 471.0]
               [height 391.0]
               [scale 0.5]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [sprite-tex tex]
               [flipper-ticks 20]
               [hitboxes '(#(152 125 54 155)
                           #(206 60 39 275)
                           #(245 100 20 205)
                           #(265 155 15 125)
                           #(280 170 75 90)
                           #(355 196 37 36))])
    (inherit-field x y width height scale dead?)
    (field [speed 5]
           [last-shot 0]
           [shot-spacing 180]
           [hp initial-hp]
           [health-gauge (new health-gauge%
                              [offset-x 60]
                              [offset-y 190])])

    (define wasd-tutorial (new wasd-tutorial%))
    (define space-tutorial (new space-tutorial%))
    (define movement-frames-until-space-tutorial 40)

    (define/override (transition new-state)
      (super transition new-state)
      (when (and (eq? game-state 'dead) (eq? new-state 'main))
        (set! x initial-x)
        (set! y initial-y)
        (set! hp initial-hp)
        (set! dead? #f)))

    (define (damage by)
      (set! hp (sub1 hp))
      (when (= hp 0)
        (set! dead? #t)
        (transition-to 'dead))
      (new explosion% [x (send by center-x)] [y (send by center-y)]))

    (define/override (tick)
      (unless dead?
        (super tick)
        (define moved-this-frame? #f)
        (when (IsKeyDown key-right)
          (set! x (+ x speed))
          (set! moved-this-frame? #t))
        (when (IsKeyDown key-left)
          (set! x (- x speed))
          (set! moved-this-frame? #t))
        (when (IsKeyDown key-up)
          (set! y (- y speed))
          (set! moved-this-frame? #t))
        (when (IsKeyDown key-down)
          (set! y (+ y speed))
          (set! moved-this-frame? #t))
        (when moved-this-frame?
          (set! movement-frames-until-space-tutorial (sub1 movement-frames-until-space-tutorial))
          (when (= movement-frames-until-space-tutorial 0)
            (send space-tutorial show)
            (set! movement-frames-until-space-tutorial -1)))
        (when (IsKeyDown key-shoot)
          ;; ensure time spacing between shots
          (when ((current-milliseconds) . > . (+ last-shot shot-spacing))
            (for ([ys (list 178.0 230.0)])
              (new shot% [x (+ x (* (- width 70) scale))] [y (+ y (* ys scale))]))
            (set! last-shot (current-milliseconds))))
        (for ([entity (in-vector (es))])
          (when (and (or (is-a? entity enemy%) (is-a? entity enemy-shot%))
                     (send this touching? entity))
            (damage entity)
            (send entity contact)))))

    (define/override (draw)
      (unless dead?
        (super draw)
        (send health-gauge draw x y hp)))))


;;; Shots fired by the player ship.
(define shot%
  (class (sprite-mixin entity%)
    (super-new [order 90]
               [width 74.0]
               [height 20.0]
               [scale 0.5]
               [sprite-tex shot-tex]
               [hitboxes '(#(4 1 67 18))])
    (inherit-field x y width height)

    (define speed 8)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed))
      (for ([entity (in-vector (es))])
        (when (and (is-a? entity enemy%) (send this touching? entity))
          (send entity damage)
          (send this die)))
      (when (x . > . screen-width)
        (send this die)))

    (define/override (transition new-state)
      (super transition new-state)
      (when (eq? new-state 'dead)
        (send this die)))))


(define (die-offscreen-mixin %)
  (class %
    (super-new)
    (inherit-field x y width height scale)

    (define/public (offscreen)
      (send this die))

    (define/override (tick)
      (super tick)
      (when (or ((+ x (* scale width)) . < . 0)
                ((+ y (* scale height)) . < . 0)
                (x . > . screen-width)
                (y . > . screen-height))
        (offscreen)))))


;;; Shots fired by the enemy ships.
(define enemy-shot%
  (class (die-offscreen-mixin (flipper-mixin (sprite-mixin entity%)))
    (super-new)
    (define/public (contact)
      (send this die))

    (define/override (transition new-state)
      (super transition new-state)
      (when (and (eq? game-state 'dead) (eq? new-state 'main))
        (send this die)))))


(define enemy-shot-aimed%
  (class enemy-shot%
    (super-new [order 90]
               [width 49.0]
               [height 44.0]
               [scale 0.5]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [flipper-ticks 15]
               [sprite-tex enemy-shot-tex]
               [hitboxes '(#(7 11 8 25)
                           #(15 5 23 36)
                           #(38 10 5 21))])
    (inherit-field x y)
    (define ship
      (for/first ([e (es)]
                  #:when (is-a? e ship%))
        e))
    (define speed 4)
    ;; I forgot the trig calculations to do this, so time for a funny hack with complex numbers
    (define difference-complex (make-rectangular (- (send ship center-x) (send this center-x)) (- (send ship center-y) (send this center-y))))
    (define velocity-complex (make-polar speed (angle difference-complex)))
    (define velocity-x (real-part velocity-complex))
    (define velocity-y (imag-part velocity-complex))

    (define/override (tick)
      (super tick)
      (set! x (+ x velocity-x))
      (set! y (+ y velocity-y)))))


(define explosion-shot%
  (class enemy-shot%
    (super-new [order 91]
               [width 49.0]
               [height 44.0]
               [scale 0.5]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [flipper-ticks 15]
               [sprite-tex enemy-shot-tex]
               [hitboxes '(#(7 11 8 25)
                           #(15 5 23 36)
                           #(38 10 5 21))])
    (init-field ang)
    (inherit-field x y)
    (define speed 1.8)

    (define/override (tick)
      (super tick)
      (set! speed (* speed 1.025))
      (define velocity-complex (make-polar speed ang))
      (define velocity-x (real-part velocity-complex))
      (define velocity-y (imag-part velocity-complex))
      (set! x (+ x velocity-x))
      (set! y (+ y velocity-y)))))


;;; Mixin for an enemy that regularly fires directly at the player
(define (shot-regular-mixin %)
  (class %
    (super-new)
    (inherit-field x y)
    (init-field shot-time-ticks)
    (init-field shot-class%)
    (define shot-time-count (/ shot-time-ticks 2))

    (define/override (tick)
      (super tick)
      (case game-state
        [(main)
         (set! shot-time-count (add1 shot-time-count))
         (when (shot-time-count . >= . shot-time-ticks)
           (new shot-class% [x (send this center-x)] [y (send this center-y)])
           (set! shot-time-count 0))]))))


(define health-gauge%
  (class object%
    (super-new)
    (init-field offset-x offset-y)
    (init-field [width 179.0]
                [height 48.0]
                [scale 0.5])
    (define bar-breakpoints #(0.0 44.0 76.0 107.0 140.0 179.0))

    (define/public (draw parent-x parent-y hp)
      (case game-state
        [(main)
         (define x (+ parent-x offset-x))
         (define y (+ parent-y offset-y))
         ;; background
         (DrawTexturePro (force health-background-tex) ;; texture
                         (make-Rectangle 0.0 0.0 width height) ;; source
                         (make-Rectangle x y (* width scale) (* height scale)) ;; dest
                         (make-Vector2 0.0 0.0) ;; origin
                         0.0 ;; rotation
                         WHITE)
         ;; bars
         (define bar-draw-width (vector-ref bar-breakpoints hp))
         (DrawTexturePro (force health-bars-tex) ;; texture
                         (make-Rectangle 0.0 0.0 bar-draw-width height) ;; source
                         (make-Rectangle x y (* bar-draw-width scale) (* height scale)) ;; dest
                         (make-Vector2 0.0 0.0) ;; origin
                         0.0 ;; rotation
                         WHITE)
         ;; frame
         (DrawTexturePro (force health-frame-tex) ;; texture
                         (make-Rectangle 0.0 0.0 width height) ;; source
                         (make-Rectangle x y (* width scale) (* height scale)) ;; dest
                         (make-Vector2 0.0 0.0) ;; origin
                         0.0 ;; rotation
                         WHITE)]))))


(define wasd-tutorial%
  (class (sprite-mixin entity%)
    (super-new [x 0.0]
               [y 0.0]
               [order 61]
               [width 512.0]
               [height 347.0]
               [scale 0.5]
               [sprite-tex wasd-tutorial-tex]
                [hitboxes null])
    (inherit-field x y)

    (define/override (transition new-state)
      (super transition new-state)
      (case game-state
        [(main)
         (send this die)]))

    (define/override (draw)
      (case game-state
        [(tutorial)
         (define ship
           (for/first ([e (es)]
                       #:when (is-a? e ship%))
             e))
         (when ship
           (set! x (+ (get-field x ship) -20))
           (set! y (+ (get-field y ship) 200)))
         (super draw)]))))


(define space-tutorial%
  (class (sprite-mixin entity%)
    (super-new [x (- screen-width (* 305.0 0.5 1.5))]
               [y (- (/ screen-height 2) (* 142.0 0.5 0.5))]
               [order 61]
               [width 305.0]
               [height 142.0]
               [scale 0.5]
               [sprite-tex space-tutorial-tex]
               [hitboxes null])
    (inherit-field x y)

    (define visible? #f)

    (define/public (show)
      (set! visible? #t))

    (define/override (tick)
      (when (and visible? (IsKeyDown key-shoot))
        (transition-to 'main)))

    (define/override (transition new-state)
      (super transition new-state)
      (case game-state
        [(main)
         (set! visible? #f)
         (send this die)]))

    (define/override (draw)
      (when (and visible? (eq? game-state 'tutorial))
        (super draw)))))


(define game-over%
  (class (sprite-mixin entity%)
    (super-new [x (- (/ screen-width 2) (* 1708.0 0.5 0.5))]
               [y (- (/ screen-height 2) (* 625.0 0.5 0.5))]
               [order 99]
               [width 1708.0]
               [height 625.0]
               [scale 0.5]
               [sprite-tex game-over-tex]
               [hitboxes null])
    (inherit-field x y)

    (define/override (letter chars)
      (when (and (eq? game-state 'dead) (memq (char->integer #\r) chars))
        (transition-to 'main)))

    (define/override (draw)
      (when (eq? game-state 'dead)
        (super draw)))))


;;; An enemy that can be shot and attacked to evaluate its code. Most enemies will simply have their code as (die).
(define enemy%
  (class (die-offscreen-mixin (flipper-mixin (sprite-mixin entity%)))
    (super-new [order 50])
    (inherit-field x y width scale)

    (field [shots-taken 0]
           [can-be-hit-this-tick? #f])
    (init-field [base-command "hit"])

    (define command (string-append "(" base-command ")"))

    (field [display-text ""])
    (define (compute-display-text)
      (set! display-text
            (case shots-taken
              [(0) base-command]
              [(1) (string-append base-command ")")]
              [else (string-append "(" base-command ")")])))
    (compute-display-text)

    (define/override (tick)
      (super tick)
      ;; ship has to come 70% of the way screen before it can be hit
      (when ((- screen-width x) . > . (* width scale 70/100))
        (set! can-be-hit-this-tick? #t)))

    (define/public (hit)
      (send this die))

    (define/public (contact)
      (send this die))

    (define/public (damage)
      (when can-be-hit-this-tick?
        (set! shots-taken (add1 shots-taken))
        (set! can-be-hit-this-tick? #f)
        (compute-display-text)
        (when (shots-taken . >= . 3)
          (send this eval command))))

    (define/override (transition new-state)
      (super transition new-state)
      (when (and (eq? game-state 'dead) (eq? new-state 'main))
        (send this die)))))


(define enemy-basic%
  (class (shot-regular-mixin enemy%)
    (super-new [width 225.0]
               [height 121.0]
               [scale 0.5]
               [sprite-tex basic-tex]
               [sprites-across 3]
               [sprites-in-sheet 3]
               [flipper-ticks 6]
               [shot-time-ticks 110]
               [shot-class% enemy-shot-aimed%]
               [hitboxes '(#(20 56 52 13)
                           #(72 42 23 37)
                           #(95 35 85 50)
                           #(114 17 39 18)
                           #(115 85 37 16)
                           #(188 41 20 30))])
    (inherit-field x y shots-taken display-text)

    (define speed -2)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed)))

    (define/override (draw)
      (super draw)
      (define open-width (if (= shots-taken 2)
                             -11
                             0))
      (DrawTextEx (force font) display-text (make-Vector2 (+ x 46 open-width) (+ y 18)) 24.0 0.0 WHITE))))


(define enemy-clunker%
  (class enemy%
    (super-new [width 273.0]
               [height 209.0]
               [scale 0.5]
               [sprite-tex clunker-tex]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [flipper-ticks 30]
               [hitboxes '(#(54 75 21 71)
                           #(75 66 98 89)
                           #(95 38 66 28)
                           #(98 155 62 25)
                           #(173 68 30 82))])
    (inherit-field x y shots-taken display-text)

    (define speed -1.5)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed)))

    (define/override (draw)
      (super draw)
      (define open-width (if (= shots-taken 2)
                             -11
                             0))
      (DrawTextEx (force font) display-text (make-Vector2 (+ x 40 open-width) (+ y 44)) 24.0 0.0 WHITE))))


(define enemy-dripper%
  (class enemy%
    (super-new [width 395.0]
               [height 232.0]
               [scale 0.5]
               [sprite-tex dripper-tex]
               [flipper-ticks +inf.0]
               [base-command "explode"]
               [hitboxes '(#(45 40 170 160)
                           #(215 65 60 105)
                           #(275 55 20 105)
                           #(295 70 65 65))])
    (inherit-field x y shots-taken display-text)

    (define speed -1.5)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed)))

    (define/public (explode)
      (for ([ang (in-range pi (- pi) (/ (* -2 pi) 16))])
        (new explosion-shot% [x (send this center-x)] [y (send this center-y)] [ang ang]))
      (send this hit))

    (define/override (draw)
      (super draw)
      (define open-width (if (= shots-taken 2)
                             -11
                             0))
      (DrawTextEx (force font) display-text (make-Vector2 (+ x 40 open-width) (+ y 44)) 24.0 0.0 WHITE))))

(define enemy-boss%
  (class enemy%
    (super-new [width 542.0]
               [height 283.0]
               [scale 1.0]
               [sprite-tex boss-tex]
               [flipper-ticks +inf.0]
               [base-command "let loop ()\n  (cond [(= (get-i) 30) (hit)]\n        [#t (blast) (increment-i)])"]
               [hitboxes '(#(57 48 435 55)
                           #(117 103 375 95)
                           #(62 198 430 30))])
    (inherit-field x y shots-taken display-text)
    (define i 0)

    (define/public (get-i) i)
    (define/public (increment-i) (inc i))

    (define speed -1.5)

    (define/override (tick)
      (super tick)
      (when (x . > . (/ screen-width 2))
        (set! x (+ x speed))))

    (define/public (blast)
      (define ship
        (for/first ([e (es)]
                    #:when (is-a? e ship%))
          e))
      (define base-ang (angle (make-rectangular (- (send ship center-x) (send this center-x)) (- (send ship center-y) (send this center-y)))))
      (for ([ang (in-range pi (- pi) (/ (* -2 pi) 16))])
        (define combined-ang (+ ang base-ang))
        (new explosion-shot% [x (send this center-x)] [y (send this center-y)] [ang combined-ang])))

    (define/override (contact)
      (void))

    (define/override (draw)
      (super draw)
      (define open-width (if (= shots-taken 2)
                             -11
                             0))
      (DrawTextEx (force font) display-text (make-Vector2 (+ x 79 open-width) (+ y 74)) 24.0 0.0 WHITE))))


;;; Explosion when the player is hit.
(define explosion%
  (class (sprite-mixin entity%)
    (super-new [order 80]
               [width 135.0]
               [height 168.0]
               [scale 0.5]
               [sprite-tex explosion-tex]
               [sprites-in-sheet 3]
               [sprites-across 3]
               [sprite-index 0]
               [hitboxes null])
    (inherit-field x y width height scale sprite-index)
    (init-field [explosion-total-time 27])
    (define ticks-alive 0)

    (set! x (- x (* (/ width 2) scale)))
    (set! y (- y (* (/ height 2) scale)))

    (define/override (tick)
      (super tick)
      (set! ticks-alive (add1 ticks-alive))
      (set! sprite-index
            (cond [(< ticks-alive (* explosion-total-time 1/5)) 0]
                  [(< ticks-alive (* explosion-total-time 2/5)) 1]
                  [(< ticks-alive (* explosion-total-time 3/5)) 2]
                  [(< ticks-alive (* explosion-total-time 4/5)) 1]
                  [(< ticks-alive (* explosion-total-time 5/5)) 0]
                  [(< ticks-alive (* explosion-total-time 6/5)) (send this die) 0])))))


;;; Screen background.
(define background%
  (class (sprite-mixin entity%)
    (super-new [order 10]
               [width 3153.0]
               [height 720.0]
               [scale 1.0]
               [x 0.0]
               [y 0.0]
               [sprite-tex background-tex]
               [hitboxes null])
    (inherit-field x y width height scale)
    (init-field i)

    (set! x (exact->inexact (* i width scale)))

    (define speed -1)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed))
      (when (x . < . (- (* width scale)))
        (set! x (+ x (* width scale 2)))))))


(define chalk%
  (class (die-offscreen-mixin (sprite-mixin entity%))
    (define-values (width height tex) (apply values (random-ref chalk-texes)))
    (super-new [order 11]
               [width (exact->inexact width)]
               [height (exact->inexact height)]
               [scale 1.0]
               [x (exact->inexact screen-width)]
               [y (exact->inexact (* (random) (- screen-height height)))]
               [sprite-tex tex]
               [hitboxes null])
    (inherit-field x)
    (define speed -1)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed)))))


(define chalk-spawner%
  (class entity%
    (super-new [order 0] [x 0.0] [y 0.0])
    (field [last-spawn 0]
           [spawn-frequency 400])

    (define/override (tick)
      (super tick)
      (inc last-spawn)
      (when (last-spawn . >= . spawn-frequency)
        (set! last-spawn 0)
        (set! spawn-frequency (random 1000 1700))
        (new chalk%)))))


;;; Spawn enemies according to the defined waves
(define wave-spawner%
  (class entity%
    (super-new [order 0] [x 0.0] [y 0.0])
    (define ticks 0)
    (define wave-index 0)
    (define pointer 0)
    (define wave-enemies (mutable-set))

    (define/override (transition new-state)
      (when (and (eq? game-state 'dead) (eq? new-state 'main))
        (set! ticks 0)
        (if (wave-index . >= . 2)
            (set! wave-index 2)
            (set! wave-index 0))
        (set! pointer 0)
        (set-clear! wave-enemies)))

    (define/override (tick)
      (super tick)
      (case game-state
        [(main)
         (inc ticks)
         ;; current wave (list of spawning occurrences)
         (when (wave-index . < . (vector-length waves))
           (define wave (vector-ref waves wave-index))
           (define enemies (mutable-set))
           (let loop ()
             ;; has this whole wave been spawned yet?
             (define wave-all-spawned? (pointer . >= . (vector-length wave)))
             (if (not wave-all-spawned?)
                 ;; no - check if we should spawn the next thing
                 (let ([next (vector-ref wave pointer)])
                   (when (ticks . >= . (car next))
                     (define enemy (new (second next) [x (exact->inexact screen-width)] [y (exact->inexact (third next))]))
                     (set-add! wave-enemies enemy)
                     (set! pointer (add1 pointer))
                     (loop)))
                 ;; yes - wait for all enemies to be killed, then proceed to the next wave
                 (when (for/and ([enemy (in-set wave-enemies)]) (get-field dead? enemy))
                   (set! pointer 0)
                   (set! ticks -45)
                   (set-clear! wave-enemies)
                   (inc wave-index)))))]))))


;;; Wave definitions
(define waves-def
  `(((wait 60)
     (spawn ,enemy-clunker% 300)
     (wait 80)
     (spawn ,enemy-clunker% 80))
    ((spawn ,enemy-basic% 100)
     (wait 100)
     (spawn ,enemy-basic% 300)
     (wait 20)
     (spawn ,enemy-basic% 400))
    ((spawn ,enemy-basic% 100)
     (wait 60)
     (spawn ,enemy-basic% 300)
     (wait 20)
     (spawn ,enemy-basic% 500)
     (wait 60)
     (spawn ,enemy-clunker% 200)
     (wait 10)
     (spawn ,enemy-basic% 400)
     (wait 90)
     (spawn ,enemy-basic% 100)
     (wait 10)
     (spawn ,enemy-basic% 300))
    ((spawn ,enemy-dripper% 150))
    ((spawn ,enemy-basic% 80)
     (wait 45)
     (spawn ,enemy-basic% 55)
     (wait 75)
     (spawn ,enemy-dripper% 90)
     (spawn ,enemy-basic% 400)
     (wait 40)
     (spawn ,enemy-basic% 420)
     (wait 40)
     (spawn ,enemy-basic% 440)
     (wait 30)
     (spawn ,enemy-basic% 350)
     (wait 100)
     (spawn ,enemy-dripper% 150))
    ((spawn ,enemy-boss% 150))))

(define current-tick 0)
(define waves
  (for/vector ([wave-def waves-def])
    (for/vector ([row
                  (for/list ([row wave-def])
                    (case (car row)
                      [(wait) (set! current-tick (+ current-tick (second row))) #f]
                      [(spawn) (cons current-tick (cdr row))]))]
                 #:when row)
      (set! current-tick 0)
      row)))


;;; MAIN

(define background0 (new background% [i 0]))
(define background1 (new background% [i 1]))
(define chalk-spawner (new chalk-spawner%))
(new hitbox-controller%)
(define ship (new ship%))
(define game-over (new game-over%))
(define wave-spawner (new wave-spawner%))


(define (main)
  (SetTargetFPS 60)

  ;; must initialise window (and opengl context) before any textures can be loaded
  (InitWindow screen-width screen-height "Eval-em-up!")

  (eval '(void)) ;; prevent small lag spike when shooting first enemy

  (collect-garbage)

  (let loop ((close? #f))
    (collect-garbage 'incremental)

    ;; give repl a chance to run
    (sleep/yield 0)

    (define chars-pressed-this-frame
      (for/list ([char (in-producer (λ _ (GetCharPressed)) 0)])
        char))

    ;; compute sorted entities ahead of time
    (update-es!)

    ;; tick all entities
    (for ([entity (in-vector (es))])
      (send entity tick)
      (when (pair? chars-pressed-this-frame)
        (send entity letter chars-pressed-this-frame)))

    ;; recompute sorted entities as some may have died
    (update-es!)

    ;; draw all entities
    (BeginDrawing)
    (ClearBackground RAYWHITE)
    (for ([entity (in-vector (es))])
      (send entity draw))
    (EndDrawing)

    ;; loop until closed
    (if close?
        (CloseWindow)
        (loop (WindowShouldClose)))))

(thread-wait (thread main)) ;; TODO: ensure this line includes (thread-wait ...) before compiling
