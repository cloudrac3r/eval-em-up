#lang racket
(require (only-in racket/gui sleep/yield)
         racket/generator
         data/priority-queue
         raylib/generated/unsafe)


;;; Macros
(define-syntax-rule (inc var)
  (set! var (add1 var)))


;;; Important constants
(define screen-width 1280)
(define screen-height 720)
(define debug-mode #f)
(define (debug . args)
  (when debug-mode
    (if (= (length args) 1)
        (println (car args))
        (apply printf args))))

#|
;;; Workman layout
(define key-up KEY_R)
(define key-down KEY_H)
(define key-left KEY_S)
(define key-right KEY_T)
(define key-shoot KEY_SPACE)
|#
;;; QWERTY layout
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
(define background-tex (delay (LoadTexture "tex/wood-background.png")))
(define health-background-tex (delay (LoadTexture "tex/health-background.png")))
(define health-frame-tex (delay (LoadTexture "tex/health-frame.png")))
(define health-bars-tex (delay (LoadTexture "tex/health-bars.png")))


(define (eval-mixin %)
  (define ns (module->namespace 'racket))
  (define -eval eval)
  (class % (super-new)
    (define methods (interface->method-names (object-interface this)))
    (define/public (eval str)
      (define template
        `(λ (self)
           ,@(for/list ([method methods])
               `(define (,method . args) (send/apply self ,method args)))
           ,(read (open-input-string str))))
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

     (define/public (tick)
       (void))

     (define/public (draw)
       (void))

     (define/public (die)
       (debug "dead: ~a~n" this%)
       (priority-queue-remove! entities this))

     (priority-queue-insert! entities this))))


(define (sprite-mixin %)
  ;; (printf "building sprite-mixin off ~v~n" %)
  ;; (println (interface->method-names (class->interface %)))
  (class % (super-new) (inherit-field x y)
    (init-field width)
    (init-field height)
    (init-field scale)
    (init-field sprite-tex)
    (init-field [sprites-across 1])
    (init-field [sprites-in-sheet 1])
    (init-field [sprite-index 0])

    (define/override (draw)
      (super draw)
      (define u (* width (exact->inexact (modulo sprite-index sprites-across))))
      (define v (* height (exact->inexact (quotient sprite-index sprites-across))))
      #;(define source (make-Rectangle 0.0 (add1 height) 100.0 height))
      #;(define source (make-Rectangle 0.0 0.0 100.0 height))
      (define source (make-Rectangle u v width height))
      (DrawTexturePro (force sprite-tex) ;; texture
                      source
                      (make-Rectangle x y (* width scale) (* height scale)) ;; dest
                      (make-Vector2 0.0 0.0) ;; origin
                      0.0 ;; rotation
                      WHITE))

    (define/public (touching? that)
      (and
       (or (<= (get-field x this) (get-field x that) (+ (get-field x this) (get-field width this)))
           (<= (get-field x that) (get-field x this) (+ (get-field x that) (get-field width that))))
       (or (<= (get-field y this) (get-field y that) (+ (get-field y this) (get-field height this)))
           (<= (get-field y that) (get-field y this) (+ (get-field y that) (get-field height that)))))
      #;(and ((abs (- (get-field x this) (get-field x that))) . < . 10)
           ((abs (- (get-field y this) (get-field y that))) . < . 10)))))

(define (flipper-mixin %)
  ;; (printf "building flipper-mixin off ~v~n" %)
  ;; (println (interface->method-names (class->interface %)))
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
    (super-new [order 40]
               [width 471.0]
               [height 391.0]
               [scale 0.5]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [sprite-tex tex]
               [flipper-ticks 20])
    (inherit-field x y width height scale)
    (field [speed 4]
           [last-shot 0]
           [shot-spacing 180]
           [hp 2]
           [health-gauge (new health-gauge%
                              [offset-x 60]
                              [offset-y 190])])

    (define/override (tick)
      (super tick)
      (when (IsKeyDown key-right)
        (set! x (+ x speed)))
      (when (IsKeyDown key-left)
        (set! x (- x speed)))
      (when (IsKeyDown key-up)
        (set! y (- y speed)))
      (when (IsKeyDown key-down)
        (set! y (+ y speed)))
      (when (IsKeyDown key-shoot)
        ;; ensure time spacing between shots
        (when ((current-milliseconds) . > . (+ last-shot shot-spacing))
          (for ([ys (list 178.0 230.0)])
            (new shot% [x (+ x (* (- width 70) scale))] [y (+ y (* ys scale))]))
          (set! last-shot (current-milliseconds)))))

    (define/override (draw)
      (super draw)
      (send health-gauge draw x y hp))))


;;; Shots fired by the player ship.
(define shot%
  (class (sprite-mixin entity%)
    (super-new [order 90]
               [width 74.0]
               [height 20.0]
               [scale 0.5]
               [sprite-tex shot-tex])
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
        (send this die)))))


(define health-gauge%
  (class object%
    (super-new)
    (init-field offset-x offset-y)
    (init-field [width 179.0]
                [height 48.0]
                [scale 0.5])
    (define bar-breakpoints #(0.0 44.0 76.0 107.0 140.0 179.0))

    (define/public (draw parent-x parent-y hp)
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
                      WHITE))))


;;; An enemy that can be shot and attacked to evaluate its code. Most enemies will simply have their code as (die).
(define enemy%
  (class (flipper-mixin (sprite-mixin entity%))
    (super-new [order 50])
    (inherit-field x y)

    (field [shots-taken 0]
           [can-be-hit-this-tick? #t])
    (define speed -2)
    (field [base-command "die"])

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
      (set! can-be-hit-this-tick? #t)
      (set! x (+ x speed))
      (when (x . < . 0)
        (send this die)))

    (define/public (damage)
      (when can-be-hit-this-tick?
        (set! shots-taken (add1 shots-taken))
        (set! can-be-hit-this-tick? #f)
        (compute-display-text)
        (when (shots-taken . >= . 3)
          (send this eval command))))))


(define enemy-basic%
  (class enemy%
    (super-new [width 225.0]
               [height 121.0]
               [scale 0.5]
               [sprite-tex basic-tex]
               [sprites-across 3]
               [sprites-in-sheet 3]
               [flipper-ticks 6])
    (inherit-field x y shots-taken display-text)

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
               [flipper-ticks 30])
    (inherit-field x y shots-taken display-text)

    (define/override (draw)
      (super draw)
      (define open-width (if (= shots-taken 2)
                             -11
                             0))
      (DrawTextEx (force font) display-text (make-Vector2 (+ x 40 open-width) (+ y 44)) 24.0 0.0 WHITE))))


(define spawner%
  (class entity%
    (super-new [order 0] [x 0.0] [y 0.0])
    (field [last-spawn 0]
           [spawn-frequency 80]
           [next-y-pos (infinite-generator
                        (yield 50.0)
                        (yield 250.0)
                        (yield 400.0)
                        #;(yield 600.0))]
           [next-enemy (infinite-generator
                        (yield enemy-clunker%)
                        (yield enemy-basic%))])
    (define/override (tick)
      (super tick)
      (inc last-spawn)
      (when (last-spawn . >= . spawn-frequency)
        (set! last-spawn 0)
        (new (next-enemy) [x (exact->inexact screen-width)] [y (next-y-pos)])))))


;;; Screen background.
(define background%
  (class (sprite-mixin entity%)
    (super-new [order 10]
               [width 3444.0]
               [height 1440.0]
               [scale 0.5]
               [x 0.0]
               [y 0.0]
               [sprite-tex background-tex])
    (inherit-field x y width height scale)
    (init-field i)

    (set! x (exact->inexact (* i width scale)))

    (define speed -1)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed))
      (when (x . < . (- (* width scale)))
        (set! x (+ x (* width scale 2)))))))


;;; MAIN

(define background0 (new background% [i 0]))
(define background1 (new background% [i 1]))
(define ship (new ship% [x 20.0] [y 400.0]))
(define enemy (new enemy-basic% [x (- (exact->inexact screen-width) 200)] [y 400.0]))
(define spawner (new spawner%))

(define (main)
  (SetTargetFPS 60)

  ;; must initialise window (and opengl context) before any textures can be loaded
  (InitWindow screen-width screen-height "Basic Window")

  (collect-garbage)

  (let loop ((close? #f))
    (collect-garbage 'incremental)

    ;; give repl a chance to run
    (sleep/yield 0)

    ;; compute sorted entities ahead of time
    (update-es!)

    ;; tick all entities
    (for ([entity (in-vector (es))])
      (send entity tick))

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

(thread main)
