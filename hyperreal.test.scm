#!/usr/bin/env -S csi -s
(import srfi-78)

(load "hyperreal.scm")

(define (real-hyperreal i)
  69)
(define (one-hyperreal i)
  1)

(define (first-element x)
  (hyper-ref x 0))
(define (second-element x)
  (hyper-ref x 1))

(check (first-element 69) => 69)
(check (second-element real-hyperreal) => 69)

(check (first-element (index-hyperreal)) => 1)
(check (second-element (index-hyperreal)) => 2)

(check (add-hyperreal 1 2) => 3)
(define hyperreal-sum (add-hyperreal real-hyperreal real-hyperreal))
(check (procedure? hyperreal-sum) => #t)
(check (first-element hyperreal-sum) => (+ 69 69))

(check (reciprocal-hyperreal 2) => (/ 1 2))
(define hyperreal-reciprocal (reciprocal-hyperreal real-hyperreal))
(check (procedure? hyperreal-reciprocal) => #t)
(check (first-element hyperreal-reciprocal) => (/ 1 69))

(define (near? x y #!optional (tol 0.001))
  (if (< (abs (- x y)) tol)
      #t
      #f))

(check (near? ((aitken-extrapolate infinitesimal) 10) 0) => #t)
(check (near? (standard-part infinitesimal) 
              0) => #t)

(define (limited-standard-part x) 
  (standard-part x 10))

(define (linear-function x)
  x)
(define linear-derivative (differentiate linear-function))
(check (standard-part
         (linear-derivative 1)) => 1)
(check (standard-part (linear-derivative one-hyperreal)) => 1)
(check (standard-part (linear-derivative real-hyperreal)) => 1)
(check (limited-standard-part (integrate linear-function 0 1)) => (/ 1 2))

(define (parabolic-function x)
  (expt-hyperreal x 2))
(define parabolic-derivative (differentiate parabolic-function))
(check (standard-part (parabolic-derivative 1)) => 2)
(check (standard-part (parabolic-derivative one-hyperreal)) => 2)
(check (near? (limited-standard-part 
                (integrate parabolic-function 0 1)) 
              (/ 1 3)) => #t)
(check (near? (limited-standard-part 
                (integrate parabolic-function 0 one-hyperreal)) 
              (/ 1 3)) => #t)

(define decay-constant -1)

(define (exponential-decay t)
  (memoize 
    (if-hyperreal (lte-hyperreal t 0)
                  1
                  (ode-integrate exponential-decay 
                                 (lambda (t y) (mul-hyperreal y decay-constant))
                                 t))))

(check (near? (standard-part (exponential-decay 10) 3 extrapolate?: #t) 0) 
              => #t)

(define cannonball-initial-position #(0 0))
(define (cannonball-trajectory t)
  (if-hyperreal (lte-hyperreal t 0)
                cannonball-initial-position
                (ode-integrate cannonball-trajectory
                               cannonball-velocity
                               t)))

(define muzzle-speed 10)
(define pi (acos -1))
(define firing-angle (/ pi 4))
(define cannonball-initial-velocity 
  (vector (* muzzle-speed 
             (cos firing-angle))
          (* muzzle-speed
             (sin firing-angle))))
(define cannonball-mass 1)

(define (cannonball-velocity t y)
  (if-hyperreal (lte-hyperreal t 0)
                cannonball-initial-velocity
                (ode-integrate cannonball-velocity
                               cannonball-acceleration
                               t)))

(define drag-coefficient 1)
(define (cannonball-acceleration t velocity)
  (let* ((cannonball-speed (vector-norm-hyperreal y))
        (drag-force (mul-scalar-vector-hyperreal 
                      (mul-hyperreal -1
                                      drag-coefficient
                                      (square-hyperreal (vector-norm-hyperreal velocity)))
                      (normalize-vector-hyperreal velocity)))
        (drag-acceleration (mul-scalar-vector-hyperreal 
                             (hyperreal-reciprocal cannonball-mass) 
                             drag-force))
        (gravitational-acceleration (vector-hyperreal 0 -9.8)))
    ()
    ))
