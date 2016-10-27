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

;;================
;;Data definitions:

(define-struct point [x y])
;;Point is (make-point Number Number)
#;
(define [fn-for-point p]
  (... (point-x p)
       (point-y p)))

(define-struct ball [pos vel])
;;Ball is (make-ball (Point Point))
#;
(define [fn-for-ball b]
  (... (fn-for-point (ball-pos b))
       (fn-for-point (ball-vel b))))

;;ListOfBall is one of:
;;- empty
;;- (cons Ball ListOfBall)
#;
(define [fn-for-lob lob]
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-ball (first lob))
              (fn-for-lob (rest lob)))]))

;;================
;;Functions:



;;================
;;Run:
