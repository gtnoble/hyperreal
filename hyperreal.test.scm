#!/usr/bin/env -S csi -s
(import srfi-78)

(load "hyperreal.so")

(define real-hyper (make-nonstandard (lambda (i) 69)))
(check (nonstandard? real-hyper) => #t)
(define one-hyper (make-nonstandard (lambda (i) 1)))
(check (nonstandard? one-hyper) => #t)

(define (first-element x)
  (hyper-ref x 0))
(define (second-element x)
  (hyper-ref x 1))

(check (first-element 69) => 69)
(check (second-element real-hyper) => 69)

(check (first-element index-hyper) => 1)
(check (second-element index-hyper) => 2)

(check (add-hyper 1 2) => 3)
(check (add-hyper 1 2 3 4) => 10)
(check (add-hyper #(1 2) #(3 4)) => #(4 6))
(define hyper-sum (add-hyper real-hyper real-hyper))
(check (nonstandard? hyper-sum) => #t)
(check (first-element hyper-sum) => (+ 69 69))

(check (reciprocal-hyper 2) => (/ 1 2))
(define hyper-reciprocal (reciprocal-hyper real-hyper))
(check (nonstandard? hyper-reciprocal) => #t)
(check (first-element hyper-reciprocal) => (/ 1 69))

(define (near? x y #!optional (tol 0.001))
  (if (< (abs (- x y)) tol)
      #t
      #f))

(check (near? (aitken-extrapolate (lambda (i) (nonstandard-ref infinitesimal i)) 
                                   10) 
              0) => #t)
(check (near? (standard-part infinitesimal) 
              0) => #t)

(define (limited-standard-part x) 
  (standard-part x 10))

(define (linear-function x)
  x)
(define linear-derivative (differentiate linear-function))
(check (standard-part
         (linear-derivative 1)) => 1)
(check (standard-part
         (linear-derivative 2)) => 1)
(check (standard-part (linear-derivative one-hyper)) => 1)
(check (standard-part (linear-derivative real-hyper)) => 1)
(check (limited-standard-part (integrate linear-function 0 1)) => (/ 1 2))

(define (parabolic-function x)
  (expt-hyper x 2))
(check (parabolic-function 2) => 4)
(define parabolic-derivative (differentiate parabolic-function))
(check (near? (standard-part (parabolic-derivative 1)) 2) => #t)
(check (near? (standard-part (parabolic-derivative one-hyper)) 2) => #t)
(check (near? (limited-standard-part 
                (integrate parabolic-function 0 1)) 
              (/ 1 3)) => #t)
(check (near? (limited-standard-part 
                (integrate parabolic-function 0 one-hyper)) 
              (/ 1 3)) => #t)

(define target-value 2.5)


(define decay-constant -1.0)
(define exponential-initial-value 1.0)

(define (exponential-decay time)
  (define (update-state current-time current-state differential)
    (let* ((y (hyper-vector-ref current-state 0))
             (y-prime (hyper-vector-ref current-state 1))
             (dy (differential y-prime))
             (next-y (add-hyper y dy))
             (next-y-prime (multiply-hyper y decay-constant))) 
        (make-hyper-vector next-y next-y-prime)))
  (ode-solve  
    update-state
    0
    time
    (make-hyper-vector exponential-initial-value 
                       (multiply-hyper exponential-initial-value 
                                       decay-constant))))
(check 
  (near? 
    (vector-ref (standard-part (exponential-decay 10) 
                               8 extrapolate?: #f) 
                0) 
    0) 
  => #t)

(define cannonball-initial-position #(0 0))
(define muzzle-speed 10)
(define pi (acos -1))
(define firing-angle (/ pi 4))
(define cannonball-initial-velocity 
  (vector (* muzzle-speed 
             (cos firing-angle))
          (* muzzle-speed
             (sin firing-angle))))
(define cannonball-mass 1)
(define drag-coefficient 1)

(define (cannonball-trajectory final-time)
  (let ((calculate-acceleration 
         (lambda (velocity)
           (let* ((drag-force (multiply-hyper 
                                -1
                                drag-coefficient
                                (hyper-vector-norm-squared velocity)
                                (normalize-hyper-vector velocity)))
                  (drag-acceleration (divide-hyper 
                                       drag-force
                                       cannonball-mass))
                  (gravitational-acceleration (make-hyper-vector 0 -9.8)))
             (add-hyper drag-force gravitational-acceleration))))) 
    (ode-solve (lambda (current-time current-state differential)
                 (let ((current-position (hyper-vector-ref current-state 0))
                       (current-velocity (hyper-vector-ref current-state 1))
                       (current-acceleration (hyper-vector-ref current-state 2)))
                   (let ((next-position 
                           (add-hyper current-position 
                                      (differential current-velocity)))
                         (next-velocity 
                           (add-hyper current-velocity
                                      (differential current-acceleration)))
                         (next-acceleration 
                           (calculate-acceleration current-velocity)))
                     (make-hyper-vector next-position next-velocity next-acceleration))))
               0
               final-time
               (make-hyper-vector cannonball-initial-position 
                       cannonball-initial-velocity 
                       (calculate-acceleration cannonball-initial-velocity)))))

(print (standard-part (cannonball-trajectory 0.5) 6 extrapolate?: #t))

(check (near? (standard-part (gradient-descent parabolic-function 0.01 1) 8) 0) => #t)
