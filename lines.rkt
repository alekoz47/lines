;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (round-five n)
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;lines

;;================
;;Constants:

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))
(define SPEED 10)
(define TICK-SPEED 1)

;;================
;;Data definitions:

(define-struct point (x y))
;;Point is (make-point Number Number)
#;
(define (fn-for-point p)
  (... (point-x p)
       (point-y p)))

(define-struct ball (pos vel))
;;Ball is (make-ball Point Point)
#;
(define (fn-for-ball b)
  (... (fn-for-point (ball-pos b))
       (fn-for-point (ball-vel b))))

;;ListOfBall is one of:
;;- empty
;;- (cons Ball ListOfBall)
#;
(define (fn-for-lob lob)
  (cond ((empty? lob) (...))
        (else
         (... (fn-for-ball (first lob))
              (fn-for-lob (rest lob))))))

;;================
;;Functions:

;;ListOfBall -> ListOfBall
;;start world with (main ...)
(define (main lob)
  (big-bang lob
            (on-tick tock TICK-SPEED)
            (to-draw render)
            (on-mouse handle-mouse)))

;;ListOfBall -> ListOfBall
;;cycle through list of balls
(define (tock lob)
  (cond ((empty? lob) empty)
        (else
         (cons (move (first lob))
               (rest lob)))))

;;Ball -> Ball
;;if ball is on or beyond wall, bounce, else move forwards
(define (move b)
  (cond ((or (<= 0 (point-x (ball-pos b)))
             (>= WIDTH (point-x (ball-pos b))))
         (bounce "left/right" b))
        ((or (<= 0 (point-y (ball-pos b)))
             (>= HEIGHT (point-y (ball-pos b))))
         (bounce "up/down" b))
        (else (forward b))))

;;Ball -> Ball
;;move ball forwards with respect to velocity
(define (forward b)
  (make-ball (make-point (+ (* SPEED (point-x (ball-vel b)))
                            (point-x (ball-pos b)))
                         (+ (* SPEED (point-y (ball-vel b)))
                            (point-y (ball-pos b))))
             (make-point (point-x (ball-vel b))
                         (point-y (ball-vel b)))))

;;Ball String -> Ball
;;bounce ball off horizontal or vertical wall
(define (bounce s b)
  (cond ((string=? "up/down" s)
         (make-ball (make-point
                     (- (point-x (ball-pos b))
                        (* SPEED (point-x (ball-vel b))))
                     (+ (point-y (ball-pos b))
                        (* SPEED (point-x (ball-vel b)))))
                    (make-point
                     (point-x (ball-vel b))
                     (- 0 (point-y (ball-vel b))))))
        ((string=? "left/right" s)
         (make-ball (make-point
                     (+ (point-x (ball-pos b))
                        (* SPEED (point-x (ball-vel b))))
                     (+ (point-y (ball-pos b))
                        (* SPEED (point-x (ball-vel b)))))
                    (make-point
                     (- 0 (point-x (ball-vel b)))
                     (point-y (ball-vel b)))))))

;;ListOfBall -> Image
;;render lines between closest points
(define (render lob)
  (cond ((empty? lob) MTS)
        (else
         (add-line (render (rest lob))
                   (point-x (ball-pos (first lob)))
                   (point-y (ball-pos (first lob)))
                   (/ WIDTH 2)
                   (/ HEIGHT 2)
                   "black"))))

;;MouseEvent ListOfBall -> ListOfBall
;;make additional ball with random velocity and mouse position
(define (handle-mouse lob x y me)
  (cond ((mouse=? "button-down" me)
         (cons (make-ball
                (make-point x y)
                (make-point (round-five (cos (+ x y)))
                            (round-five (sin (+ x y)))))
               lob))
        (else lob)))

;;================
;;Run:

(main empty)
