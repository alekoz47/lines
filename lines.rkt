;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;lines

;;================
;;Constants:

(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))
(define SPEED 10)
(define TICK-SPEED 0.015)
(define TEST-X (/ WIDTH 2))
(define TEST-Y (/ HEIGHT 2))
(define TEST-V (round-five (/ 1 (sqrt 2))))

;;================
;;Data definitions:

(define-struct point (x y))
;;Point is (make-point Number Number)
#;
(define (fn-for-point p)
  (... (point-x p)
       (point-y p)))

(define-struct ball (pos vel))
;;Ball is (make-ball (Point Point))
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
(define (main b)
  (big-bang b
            (on-tick tock)
            (to-draw render)
            (on-mouse handle-mouse)))

;;ListOfBall -> ListOfBall
(check-expect (tock empty) empty)
(check-expect (tock (cons (make-ball
                           (make-point TEST-X TEST-Y)
                           (make-point TEST-V TEST-V)) empty))
              (cons (make-ball
                     (make-point (+ (* SPEED TEST-V) TEST-X)
                                 (+ (* SPEED TEST-V) TEST-Y))
                     (make-point TEST-V TEST-V)) empty))
(check-expect (tock (cons (make-ball
                           (make-point WIDTH TEST-Y)
                           (make-point TEST-V TEST-V)) empty))
              (cons (make-ball
                     (make-point (+ (* SPEED (- 0 TEST-V)) TEST-X)
                                 (+ (* SPEED TEST-V) TEST-Y))
                     (make-point (- 0 TEST-V) TEST-V)) empty))
(check-expect (tock (cons (make-ball
                           (make-point TEST-X HEIGHT)
                           (make-point TEST-V TEST-V)) empty))
              (cons (make-ball
                     (make-point (+ (* SPEED TEST-V) TEST-X)
                                 (+ (* SPEED (- 0 TEST-V)) TEST-Y))
                     (make-point TEST-V (- 0 TEST-V))) empty))
(define (tock lob)
  (cond ((empty? lob) empty)
        (else
         (cons (move (first lob))
               (rest lob)))))

;;Ball -> Ball
;;if ball is on or beyond wall, bounce, else move forwards
(check-expect (move (make-ball (make-point TEST-X TEST-Y)
                               (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED TEST-V) TEST-X)
                                     (+ (* SPEED TEST-V) TEST-Y))
                         (make-point TEST-V TEST-V)))
(check-expect (move (make-ball (make-point WIDTH TEST-Y)
                               (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED (- 0 TEST-V)) TEST-X)
                                     (+ (* SPEED TEST-V) TEST-Y))
                         (make-point (- 0 TEST-V) TEST-V)))
(check-expect (move (make-ball (make-point TEST-X HEIGHT)
                               (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED TEST-V) TEST-X)
                                     (+ (* SPEED (- 0 TEST-V)) TEST-Y))
                         (make-point TEST-V (- 0 TEST-V))))
(define (move b)
  (cond (or (<= (point-x (ball-pos b)))
            (>= (point-x (ball-pos b)))
            (bounce "left/right" b))
        (or (<= (point-y (ball-pos b)))
            (>= (point-y (ball-pos b)))
            (bounce "up/down" b))
        (else (forward b))))

;;Ball -> Ball
;;move ball forwards with respect to velocity
(check-expect (forward (make-ball (make-point TEST-X TEST-Y)
                                  (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED TEST-V) TEST-X)
                                     (+ (* SPEED TEST-V) TEST-Y))
                         (make-point TEST-V TEST-V)))
(define (forward b)
  (make-ball (make-point (+ (* SPEED (point-x (ball-vel b)))
                            (point-x (ball-pos b)))
                         (+ (* SPEED (point-y (ball-vel b)))
                            (point-y (ball-pos b))))))

;;Ball String -> Ball
;;bounce ball off horizontal or vertical wall
(check-expect (bounce "left/right" (make-ball (make-point WIDTH TEST-Y)
                                              (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED (- 0 TEST-V)) TEST-X)
                                     (+ (* SPEED TEST-V) TEST-Y))
                         (make-point (- 0 TEST-V) TEST-V)))
(check-expect (bounce "up/down" (make-ball (make-point TEST-X HEIGHT)
                                           (make-point TEST-V TEST-V)))
              (make-ball (make-point (+ (* SPEED TEST-V) TEST-X)
                                     (+ (* SPEED (- 0 TEST-V)) TEST-Y))
                         (make-point TEST-V (- 0 TEST-V))))
(define (bounce s b)
  (cond ((string=? "up/down" s)
         (make-ball (make-point
                     (- (point-x (ball-pos b))
                        (* BALL-SPEED (point-x (ball-vel b))))
                     (+ (point-y (ball-pos b))
                        (* BALL-SPEED (point-x (ball-vel b)))))
                    (make-point
                     (point-x (ball-vel b))
                     (- 0 (point-y (ball-vel b)))))
        ((string=? "left/right" s)
         (make-ball (make-point
                     (+ (point-x (ball-pos b))
                        (* BALL-SPEED (point-x (ball-vel b))))
                     (+ (point-y (ball-pos b))
                        (* BALL-SPEED (point-x (ball-vel b)))))
                    (make-point
                     (- 0 (point-x (ball-vel b)))
                     (point-y (ball-vel b))))))))

;;ListOfBall -> Image
;;render lines between closest points
(check-expect (render empty) MTS)
(check-expect (render (cons (make-ball
                             (make-point TEST-X TEST-Y)
                             (make-point TEST-V TEST-V)) empty))
              MTS)
(check-expect (render (list (make-ball
                             (make-point TEST-X TEST-Y)
                             (make-point TEST-V TEST-V))
                            (make-ball
                             (make-point (+ 10 TEST-X) (+ 10 TEST-Y))
                             (make-point TEST-V TEST-V))))
              (add-line MTS TEST-X TEST-Y (+ 10 TEST-X) (+ 10 TEST-Y) "black"))
(check-expect (render (list (make-ball
                             (make-point TEST-X TEST-Y)
                             (make-point TEST-V TEST-V))
                            (make-ball
                             (make-point (+ 10 TEST-X) (+ 10 TEST-Y))
                             (make-point TEST-V TEST-V))
                            (make-ball
                             (make-point (+ 20 TEST-X)
                                         (+ 20 TEST-Y))
                             (make-point TEST-V TEST-V))))
              (add-line
               (add-line MTS TEST-X TEST-Y (+ 10 TEST-X) (+ 10 TEST-Y) "black")
               (+ 10 TEST-X) (+ 10 TEST-X) (+ 20 TEST-X) (+ 20 TEST-X) "black"))
;;!!!
(define (render lob)
  (cond ((empty? lob) MTS)
        (else
         (add-line (render (rest lob))
                   (fn-for-ball (first lob))))))

;;MouseEvent ListOfBall -> ListOfBall
;;make additional ball with random velocity and mouse position
(define (handle-mouse lob me x y)
  (cond ((mouse=? "mouse-down" me)
         (cons (make-ball
               (make-point x y)
               (make-point (round-five (cos (+ x y)))
                           (round-five (sin (+ x y)))))
              b))
        (else b)))

;;Number -> Number
;;truncate inexact number to five decimal places
(define [round-five n]
  (/ (round (* n (expt 10 5))) (expt 10 5)))

;;================
;;Run:
