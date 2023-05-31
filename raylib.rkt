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

     (define/pubment (tick)
       (inner (void) tick))

     (define/public (die)
       (debug "dead: ~a~n" this%)
       (set-remove! entities this))

     (define/public (touching? that)
       (and ((abs (- (get-field x this) (get-field x that))) . < . 10)
            ((abs (- (get-field y this) (get-field y that))) . < . 10))))))


;;; The ship that the player controls.
(define ship%
  (class entity% (super-new)
    (inherit-field x y)

    (define/augment (tick)
      (when (IsKeyDown key-right)
        (set! x (+ x 2)))
      (when (IsKeyDown key-left)
        (set! x (- x 2)))
      (when (IsKeyDown key-up)
        (set! y (- y 2)))
      (when (IsKeyDown key-down)
        (set! y (+ y 2)))
      (when (IsKeyPressed key-shoot)
        (new shot% [x (+ 4 x)] [y y])))

    (define/public (draw)
      (DrawTextEx (force font) "SHIP" (make-Vector2 x y) 24.0 0.0 BLUE))))


;;; Shots fired by the player ship.
(define shot%
  (class entity% (super-new)
    (inherit-field x y)
    (inherit die)

    (define speed 4)

    (define/augment (tick)
      (set! x (+ x speed))
      (for ([entity (in-set entities)])
        (when (and (is-a? entity enemy%) (send this touching? entity))
          (send entity damage)
          (send this die)))
      (when (x . > . screen-width)
        (send this die)))

    (define/public (draw)
      (DrawTextEx (force font) "=" (make-Vector2 x y) 24.0 0.0 YELLOW))))


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

    (define/augment (tick)
      (set! x (+ x speed))
      (when (x . < . 0)
        (send this die)))

    (define/public (damage)
      (set! shots-taken (add1 shots-taken))
      (compute-display-text)
      (when (shots-taken . >= . 3)
        (send this eval command)))

    (define/public (draw)
      (DrawTextEx (force font) display-text (make-Vector2 x y) 24.0 0.0 RED))))


;;; MAIN

(define ship (new ship% [x 20.0] [y 400.0]))
(define enemy (new enemy% [x (exact->inexact screen-width)] [y 400.0]))

(define (main)
  (SetTargetFPS 60)

  (InitWindow screen-width screen-height "Basic Window")

  (define ball-position (make-Vector2 (/ screen-width 2.0) (/ screen-height 2.0)))
  (define tex (LoadTexture "/home/cadence/Downloads/hearts.png"))

  (let loop ((close? #f))

    (for ([entity (in-set entities)])
      (send entity tick))

    (BeginDrawing)
    (ClearBackground BLACK)
    (DrawTextEx (force font) "you'll DIE" (make-Vector2 10.0 10.0) 24.0 0.0 WHITE)
    ;; (DrawTextureV tex ball-position (make-Color 255 255 255 255))

    (for ([entity (in-set entities)])
      (send entity draw))
    (EndDrawing)

    (if close?
        (CloseWindow)
        (loop (WindowShouldClose)))))

(main)
