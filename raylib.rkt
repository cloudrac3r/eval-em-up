#lang racket
(require raylib/generated/unsafe)


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
(define font (delay (LoadFontEx "SourceCodePro-Medium.ttf" 24 #f 0)))
(define tex (delay (LoadTexture "ship-sheet.png")))
(define shot-tex (delay (LoadTexture "shot.png")))


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
(define entities (mutable-set))
(define entity%
  (eval-mixin
   (class object% (super-new)
     (set-add! entities this)

     (init-field x)
     (init-field y)

     (define/public (tick)
       (void))

     (define/public (draw)
       (void))

     (define/public (die)
       (debug "dead: ~a~n" this%)
       (set-remove! entities this))

     (define/public (touching? that)
       (and ((abs (- (get-field x this) (get-field x that))) . < . 10)
            ((abs (- (get-field y this) (get-field y that))) . < . 10))))))


;;; Maybe use macros to preload each texture properly? Or if I can just do that with functions?
;;; Then, class or mixin for an object with a hitbox. Use the sprite sheet size as the hitbox size. Have the collision detection be based on this class and use the hitbox.

(define (sprite-mixin-first %)
  ;; (printf "building sprite-mixin-first off ~v~n" %)
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
                      ; scale
                      WHITE))))

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
  (class (flipper-mixin (sprite-mixin-first entity%))
    (super-new [width 471.0]
               [height 391.0]
               [scale 0.5]
               [sprites-across 2]
               [sprites-in-sheet 2]
               [sprite-tex tex]
               [flipper-ticks 20])
    (inherit-field x y width height scale)
    (field [speed 3.45]
           [last-shot 0]
           [shot-spacing 180])

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
          (set! last-shot (current-milliseconds)))))))


;;; Shots fired by the player ship.
(define shot%
  (class (sprite-mixin-first entity%)
    (super-new [width 74.0]
               [height 20.0]
               [scale 0.5]
               [sprite-tex shot-tex])
    (inherit-field x y width height)

    (define speed 8)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed))
      (for ([entity (in-set entities)])
        (when (and (is-a? entity enemy%) (send this touching? entity))
          (send entity damage)
          (send this die)))
      (when (x . > . screen-width)
        (send this die)))))


;;; An enemy that can be shot and attacked to evaluate its code. Most enemies will simply have their code as (die).
(define enemy%
  (class entity% (super-new)
    (inherit-field x y)

    (field [shots-taken 0])
    (define speed -1)
    (field [base-command "die"])

    (define command (string-append "(" base-command ")"))

    (define display-text "")
    (define (compute-display-text)
      (set! display-text
            (case shots-taken
              [(0) base-command]
              [(1) (string-append base-command ")")]
              [else (string-append "(" base-command ")")])))
    (compute-display-text)

    (define/override (tick)
      (super tick)
      (set! x (+ x speed))
      (when (x . < . 0)
        (send this die)))

    (define/public (damage)
      (set! shots-taken (add1 shots-taken))
      (compute-display-text)
      (when (shots-taken . >= . 3)
        (send this eval command)))

    (define/override (draw)
      (super draw)
      (DrawTextEx (force font) display-text (make-Vector2 x y) 24.0 0.0 RED))))


;;; MAIN

(define ship (new ship% [x 20.0] [y 400.0]))
(define enemy (new enemy% [x (exact->inexact screen-width)] [y 400.0]))

(define (main)
  (SetTargetFPS 60)

  ;; must initialise window (and opengl context) before any textures can be loaded
  (InitWindow screen-width screen-height "Basic Window")

  (define ball-position (make-Vector2 (/ screen-width 2.0) (/ screen-height 2.0)))
  ;; (define tex (LoadTexture "/home/cadence/Downloads/hearts.png"))

  (let loop ((close? #f))

    (sleep)

    (for ([entity (in-set entities)])
      (send entity tick))

    (BeginDrawing)
    (ClearBackground RAYWHITE)
    (DrawTextEx (force font) "you'll DIE" (make-Vector2 10.0 10.0) 24.0 0.0 BLACK)
    (for ([entity (in-set entities)])
      (send entity draw))
    (EndDrawing)

    (if close?
        (CloseWindow)
        (loop (WindowShouldClose)))))

(thread main)
