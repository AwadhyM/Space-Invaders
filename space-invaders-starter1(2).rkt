;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |space-invaders-starter1(2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT 0 "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;Extra data definition - ListOfMissilines
;Data that represents how many missiles are in the game 
;ListOfMissiles is one of:
 ; - empty
 ; - cons Missile ListOfMissiles 

(define LOM0 empty) ; no missiles in the game
(define LOM1 (cons M1 empty)) ; 1 missile
(define LOM2 (list (make-missile 100 150) (make-missile 200 210) (make-missile 50 50))) ;multiple missiles

;template for functions

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                   ;BASE CASE
        [else (... (first lom)                 ;String
                   (fn-for-lom (rest lom)))])) ;NATURAL RECURSION

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G4 (make-game empty empty T2))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

; Function design

(define (start-game game)
  (big-bang game                   ; game
            (on-tick  move-data)     ; game -> game
            (to-draw  render-data)   ; game -> Image
            ;(stop-when game-over?)      ; game -> Boolean
            (on-mouse  shoot-missile)      ; game Integer Integer MouseEvent -> game
            (on-key    change-direction)))    ;game key -> game

;Functions

; #1 - Change-direction
;game key --> game
;Change the direction that the tank is moving in depending on which key is pressed
;(define (change-direction game ke) game)

(check-expect (change-direction (make-game empty empty T1) "left") (make-game empty empty T2))
(check-expect (change-direction (make-game empty empty T1) "right") (make-game empty empty T1))
(check-expect (change-direction (make-game empty empty T2) "up") (make-game empty empty T2))

(define (change-direction game ke)
  (cond [(same-direction? game ke) game]
        [else 
         (if (key=? ke "left")
             (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1)) 
             (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 1)))]))

; #2 - same-direction? (helper function)
; game + ke --> boolean
;Produce true if key pressed matches the direction that the tank is already going in
;(define (same-direction? ke game) true)

(check-expect (same-direction? (make-game empty empty T1) "left") false)
(check-expect (same-direction? (make-game empty empty T1) "up") true)
(check-expect (same-direction? (make-game empty empty T1) "right") true)

(define (same-direction? game ke)
  (cond [(and (key=? ke "left") (= (tank-dir (game-tank game)) -1)) true]
        [(and (key=? ke "right") (= (tank-dir (game-tank game)) 1)) true]
        [(false? (or (key=? ke "left") (key=? ke "right"))) true]
        [else
         false]))

; #3 - render-data
;ListOfInvaders + ListOfMissiles + tank --> Image
;(render-data (invaders missiles tank))
;Present the data in the game as an image
;(define (render-data invaders missiles tank))

;Tests for showing tank
(check-expect (render-data G0) (place-image TANK (tank-x T0) 480 BACKGROUND))
(check-expect (render-data (make-game empty empty T1)) (place-image TANK (tank-x T1) 480 BACKGROUND))
(check-expect (render-data (make-game empty empty (make-tank 140 -1))) (place-image TANK 140 480 BACKGROUND))


(define (render-data game)
     (overlay
      (render-invaders (game-invaders game))
      (render-missiles (game-missiles game))
      (render-tank (game-tank game))        
              ))

; #4 - place-tank
;tank --> image
;Helper function that allows the render-data game to render the tank
;(define (render-tank tank) TANK)

(check-expect (render-tank T1) (place-image TANK (tank-x T1) 480 BACKGROUND))

(define (render-tank tank)
  (place-image TANK (tank-x tank) 480 BACKGROUND))

; #5 - move-data
; Game --> Game
;Ensure that data is changing with each second to match flow of the game
;(define (move-data game) game)

;Tests for movement of tank
;(check-expect (move-data G0)(make-game empty empty (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))) ;Tank going right
;(check-expect (move-data (make-game empty empty T2))(make-game empty empty (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))) ;Tank going left

(define (move-data game) 
  (make-game
       (next-invaders(game-invaders game) (game-missiles game))
       (next-missiles (game-missiles game) (game-invaders game))
       (move-tank (game-tank game))))

; #6 - move-tank (helper function)
;;tank --> tank
; Change the x coordinate of the tank
;(define (move-tank tank) game)

(check-expect (move-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))

(define (move-tank tank)
  (cond [(and (out-of-bounds? tank) (= (tank-dir tank) 1) (> (tank-x tank) 279)) tank]
        [(and (out-of-bounds? tank) (= (tank-dir tank) -1) (< (tank-x tank) 21)) tank]
        [else
         (if (> (tank-dir tank) 0)
             (make-tank (+ (tank-x tank) TANK-SPEED) (tank-dir tank))
             (make-tank (- (tank-x tank) TANK-SPEED) (tank-dir tank)))]))


; #7 - out-of-bounds? (helper function)
;; tank --> boolean
;; Check the x coordinate of the tank to prevent it from going out of the screen
;(define (out-of-bounds? tank) true)

(check-expect (out-of-bounds? (make-tank 300 1)) true)
(check-expect (out-of-bounds? (make-tank 3 -1)) true)
(check-expect (out-of-bounds? (make-tank 50 1)) false)

(define (out-of-bounds? tank)
  (if (or (> (tank-x tank) 280) (< (tank-x tank) 20))
      true
      false))


; #8- Functions for the missiles
; game x y me --> game
; Add missiles to the game 
;(define (shoot-missile game x y me) game)

(check-expect (shoot-missile (make-game empty empty (make-tank 50 1)) 100 150 "button-down") (make-game empty (list (make-missile 51 TANK-HEIGHT/2)) (make-tank 50 1))) ; valid click
(check-expect (shoot-missile (make-game empty empty (make-tank 50 1)) 100 150 "drag") (make-game empty empty (make-tank 50 1))) ;invalid me
(check-expect (shoot-missile (make-game empty
                                        (list (make-missile 100 150) (make-missile 20 30) (make-missile 80 90)) (make-tank 50 1)) 40 70 "button-down")
                                        (make-game empty (list (make-missile 51 TANK-HEIGHT/2)(make-missile 100 150) (make-missile 20 30) (make-missile 80 90)) (make-tank 50 1)))


(define (shoot-missile game x y me)
  (cond [(mouse=? me "button-down") (make-game (game-invaders game) (cons (make-missile (+ (tank-dir (game-tank game)) (tank-x (game-tank game))) TANK-HEIGHT/2) (game-missiles game)) (game-tank game))]
        [else game]))

; #9 - Function for the missiles - next-missiles
;lom --> lom
; Advance and filter the list of missiles
(define (next-missiles lom loi)
  (on-screen-only (advance-missiles (filter-missiles lom loi))))


(check-expect (next-missiles empty empty) empty)
(check-expect (next-missiles (list (make-missile 15 15) (make-missile 100 500) (make-missile 30 20) (make-missile 50 500)) empty) (list (make-missile 15 25) (make-missile 30 30))) 

; #10 - Function for moving the Y coordinate of missiles - advance-missiles
;lom --> lom
;(define (advance-missiles lom) lom)

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list (make-missile 50 50) (make-missile 30 500) (make-missile 80 90))) (list (make-missile 50 60) (make-missile 30 510) (make-missile 80 100)))

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom)))]))

; #11 - function to move each missile
;missile ---> missile
;(define advance-missile lom)

(check-expect (advance-missile empty) empty)
(check-expect (advance-missile (make-missile 50 500)) (make-missile 50 510))

(define (advance-missile missile)
  (cond [(empty? missile) empty]
        [else 
         (make-missile (missile-x missile) (+ MISSILE-SPEED (missile-y missile)))]))

; #12 - Produce list of missiles of only missiles that are on screen
; lom --> lom
;(define (on-screen-only lom) lom)

(check-expect (on-screen-only (list (make-missile 10 10) (make-missile 50 510) (make-missile 70 70) (make-missile 80 510))) (list (make-missile 10 10) (make-missile 70 70)))
(check-expect (on-screen-only empty) empty)

(define (on-screen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (on-screen? (first lom))
          (cons (first lom) (on-screen-only (rest lom)))
          (on-screen-only (rest lom)))]))

; #13 - check if individual missile is on screen
; missile --> boolean
;(define (on-screen? missile) true)

(check-expect (on-screen? empty) false)
(check-expect (on-screen? (make-missile 50 50)) true)
(check-expect (on-screen? (make-missile 50 505)) false)

(define (on-screen? missile)
  (cond [(empty? missile) false]
        [else
         (if (> (missile-y missile) 500)
             false
             true)]))

; #14 - render-missiles - Display all the missiles currently on the screen
;lom --> image
;(define (render-missiles lom) BACKGROUND)

(check-expect (render-missiles empty) BACKGROUND)
(check-expect (render-missiles (list (make-missile 50 50))) (place-image MISSILE 50 420 BACKGROUND)) ; game with 1 missile
(check-expect (render-missiles (list (make-missile 50 50) (make-missile 100 100) (make-missile 30 40))) (place-image MISSILE 50 420 
                                                                                                         (place-image MISSILE 100 370
                                                                                                         (place-image MISSILE 30 430 BACKGROUND))))
(define (render-missiles lom)
 (cond [(empty? lom) BACKGROUND]                   ;BASE CASE
   [else (place-missile (first lom)               ;String
       (render-missiles (rest lom)))])) ;NATURAL RECURSION

; #15 - place-missile - Place a missile onto the screen
  ;missile --> image
;(define (place-missile missile) BACKGROUND)

(check-expect (place-missile empty BACKGROUND) BACKGROUND)
(check-expect (place-missile (make-missile 50 50) BACKGROUND) (place-image MISSILE 50 420 BACKGROUND))

(define (place-missile missile img)
  (cond [(empty? missile) BACKGROUND]
        [else
         (place-image MISSILE (missile-x missile) (- (- HEIGHT 30) (missile-y missile)) img)]))

; #16 - next-invaders -
;Generate Invaders and move existing invaders
; loi -> loi

(define (next-invaders loi lom)
  (create-invaders (advance-invaders (filter-invaders loi lom))))

;(check-random (next-invaders empty) (list (make-invader (random WIDTH) 0 10))) ;empty list so just has to add 1
;(check-random (next-invaders (list (make-invader 50 0 10))) (list (make-invader (random WIDTH) 0 10) (make-invader 55 5 10))) ;1 space invader so advance it and then add another
;(check-random (next-invaders (list (make-invader 80 0 10) (make-invader 55 5 10))) (list (make-invader (random WIDTH) 0 10) (make-invader 85 5 10) (make-invader 60 10 10)))


; #17 - create-invaders
; Add a new invader with random height and width to loi
; loi -> loi
;(define (create-invaders loi) loi)
;!!!


;(check-random (create-invaders (list (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10))) (list (make-invader (random WIDTH) 0 10)
                                                                                                                  ; (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10))
(check-random (create-invaders empty) (if (= 2 (random 150)) (cons (make-invader (random WIDTH) 0 10)) empty))

(check-random (create-invaders (list (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10))) (if (= 2 (random 250)) (list (make-invader (random WIDTH) 0 10)
                                                                                                                  (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10))
                                                                                                                  (list (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10))))

(define (create-invaders loi)
  (if (= 2 (random 50))
      (cons (make-invader (random WIDTH) HEIGHT 8) loi)
      loi))

; #18 - advance-invaders
; Move each existing invader in list by 45 degrees
; loi --> loi
;(define (advance-invaders loi) loi)

(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 150 100 12) (make-invader 55 5 10) (make-invader 60 10 10))) (list (make-invader 156 106 12) (make-invader 60 10 10) (make-invader 65 15 10)))

(define (advance-invaders loi)
  (cond [(empty? loi) empty]                   ;BASE CASE
        [else (cons (advance-invader (first loi))                 ;String
                   (advance-invaders (rest loi)))])) ;NATURAL RECURSION

; #19 - advance-invader
; Move invader by 45 degrees
;invader -> invader
;(define (advance-invader invader) invader)

(check-expect (advance-invader empty) empty)
(check-expect (advance-invader (make-invader 150 100 12)) (make-invader 156 106 12))


(define (advance-invader invader)
  (cond [(empty? invader) empty]
        [(hit-right-edge? invader) (make-invader (- WIDTH 15) (- (invader-y invader) 2) (- 0 (invader-dx invader)))]
        [(hit-left-edge? invader) (make-invader (+ (- WIDTH WIDTH) 15) (- (invader-y invader) 2)  (- 0 (invader-dx invader)))]
        [else
         (make-invader (+ (/ (invader-dx invader) 2) (invader-x invader)) (- (invader-y invader) 2) (invader-dx invader))]))
        
; #20 - hit-right-edge?
;invader - Boolean
; Function that suggests if an invader needs to change direction
;(define (hit-right-edge? invader) true)

(check-expect (hit-right-edge? (make-invader 55 5 10)) false) ;does not need to change direction
(check-expect (hit-right-edge? (make-invader WIDTH 80 10)) true) ;does need to change direction

(define (hit-right-edge? invader)
  (if (> (invader-x invader) (- WIDTH 15))
        true
         false))

; #21 - hit-left-edge?
; invader -> Boolean
; Function that suggests if an invader needs to change direction
;(define (hit-left-edge? invader) true)

(check-expect (hit-left-edge? (make-invader 55 5 10)) false)
(check-expect (hit-left-edge? (make-invader (- WIDTH WIDTH) 80 10)) true)

(define (hit-left-edge? invader)
  (if (< (invader-x invader) 15)
        true
         false))

; #21 - render invaders
;loi --> image
;display all the invaders onto the screen!
;(define (render-invaders loi) BACKGROUND)

(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (list (make-invader 55 5 10) (make-invader 150 100 12) (make-invader 156 106 12))) (place-image INVADER 55 5 (place-image INVADER 150 100 (place-image INVADER 156 106 BACKGROUND))))

(define (render-invaders loi)
 (cond [(empty? loi) BACKGROUND]                   ;BASE CASE
   [else (place-invader (first loi)               ;String
       (render-invaders (rest loi)))])) ;NATURAL RECURSION

; #22 - place-invader
; invader -> image
;Place an individual invader onto the screen
;(define (place-invader invader img) BACKGROUND)

(check-expect (place-invader (make-invader 55 5 10) BACKGROUND) (place-image INVADER 55 5 BACKGROUND))

                                                                                                                                             
(define (place-invader invader img)
  (cond [(empty? invader) BACKGROUND]
        [else
         (place-image INVADER (invader-x invader) (- HEIGHT (invader-y invader)) img)]))


; #23 - filter-invaders
; loi + lom ---> loi
; Filter out loi to remove any invaders that have been hit by a missile
;(define (filter-invaders loi lom) loi)

(check-expect (filter-invaders empty empty) empty)
(check-expect (filter-invaders (list (make-invader 50 50 10)) empty) (list (make-invader 50 50 10)))
(check-expect (filter-invaders (list (make-invader 50 50 10)) (list (make-missile 30 20) (make-missile 50 50))) empty)
(check-expect (filter-invaders (list (make-invader 50 50 10) (make-invader 20 21 10)) (list (make-missile 40 40) (make-missile 50 50))) (list (make-invader 20 21 10)))

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]                   
        [else (if (invader-collision? (first loi) lom)
                  (filter-invaders (rest loi) lom)
                  (cons (first loi) (filter-invaders (rest loi) lom)))]))

; #24 - invader-collision?
; invader + lom --> boolean
;check if an invader has collided with any of the missiles on the screen
;(define (invader-collision? invader lom) true)

(check-expect (invader-collision? (make-invader 50 50 10) empty) false)
(check-expect (invader-collision? (make-invader 50 50 10) (list (make-missile 30 20) (make-missile 70 60) (make-missile 100 10))) false) ;No collisions
(check-expect (invader-collision? (make-invader 50 50 10) (list (make-missile 30 20) (make-missile 50 50) (make-missile 60 60))) true) ;1 collision
(check-expect (invader-collision? (make-invader 50 50 10) (list (make-missile 20 30) (make-missile 50 50) (make-missile 49 49))) true) ;multiple collisions

(define (invader-collision? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (has-collided? invader (first lom))
             true
             (invader-collision? invader (rest lom)))]))

; #25 - has-collided?
; invader + missile --> boolean
;check if an invader has collided with a specific missile
;(define (has-collided? invader missile) true)

(check-expect (has-collided? (make-invader 50 50 10) (make-missile 20 20)) false)
(check-expect (has-collided? (make-invader 50 50 10) (make-missile 50 50)) true)
(check-expect (has-collided? (make-invader 50 50 10) (make-missile 60 60)) true)

(define (has-collided? invader missile)
  (and (<= (abs (- (invader-x invader) (missile-x missile))) HIT-RANGE)
           (<= (abs (- (invader-y invader) (missile-y missile))) HIT-RANGE)))

;#26-filter-missiles
; lom + loi ---> lom
; Filter out lom to remove any missiles that have hit an invader 
;(define (filter-missiles lom loi) lom)

(check-expect (filter-missiles empty empty) empty)
(check-expect (filter-missiles (list (make-missile 50 50)) empty) (list (make-missile 50 50)))
(check-expect (filter-missiles (list (make-missile 50 50) (make-missile 20 20)) (list (make-invader 20 50 10) (make-invader invader 50 50 10))) (list (make-missile 20 20)))


(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]                   
        [else (if (missile-collision? (first lom) loi)
                  (filter-missiles (rest lom) loi)
                  (cons (first lom) (filter-missiles (rest lom) loi)))]))


; #27 - missile-collision?
; missile + loi --> boolean
;Check if a missile has collided with any of the invaders on the screen
;(define (missile-collision? missile loi) true)

(check-expect (missile-collision? (make-missile 30 20) empty) false)
(check-expect (missile-collision? (make-missile 30 20) (list (make-invader 50 50 10) (make-invader 30 20))) true)
(check-expect (missile-collision (make-missile 30 20) (list (make-invader 50 50 10) (make-invader 60 60 10))) false)

(define (missile-collision? missile loi)
  (cond [(empty? loi) false]
        [else
         (if (missile-has-collided? missile (first loi))
             true
             (missile-collision? missile (rest loi)))]))

; #28 - missile-has-collided?
; missile + invader --> boolean
; check if missile has collided with selected invader
;(define (missile-has-collided? missile invader) true)

(check-expect (missile-has-collided? (make-missile 20 20) (make-invader 50 50 10)) false)
(check-expect (missile-has-collided? (make-missile 50 50) (make-invader 50 50 10)) true)
(check-expect (missile-has-collided? (make-missile 60 60) (make-invader 50 50 10)) true)

(define (missile-has-collided? missile invader)
  (and (<= (abs (- (invader-x invader) (missile-x missile))) HIT-RANGE)
           (<= (abs (- (invader-y invader) (missile-y missile))) HIT-RANGE)))