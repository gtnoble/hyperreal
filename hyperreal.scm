(import srfi-1 srfi-133)

;; Access the nth element of a hyperreal number
(define (hyper-ref x n)
  (if (procedure? x) (x n) x))

(define (memoize func)
  (let ((previous-x '())
        (previous-y '()))
    (lambda (x) (if (equal? previous-x x) 
                    previous-y
                    (begin
                      (set! previous-x x)
                      (set! previous-y (func x))
                      previous-y)))))

(define (hyper-operation-2 operator)
  (lambda (x y) 
      (if (or (procedure? x) (procedure? y)) 
          (memoize 
            (lambda (n) (operator (hyper-ref x n) (hyper-ref y n))))   
          (operator x y))))

(define (hyper-operation-chained pairwise-operator)
  (let ((pairwise-operation (hyper-operation-2 pairwise-operator))) 
   (letrec ((chained-operation
              (lambda (first-arg second-arg . remaining-args)
                (let ((accumulated-result (pairwise-operation first-arg second-arg)))
                 (if (null? remaining-args) 
                     accumulated-result
                     (apply chained-operation accumulated-result remaining-args))))))
     chained-operation)))

;; Create a hyperreal number representing a sequence like 1/n
(define (hyper-operation-1 operator)
  (lambda (x) 
      (if (procedure? x) 
          (memoize
            (lambda (n) (operator (hyper-ref x n))))
          (operator x))))

(define-syntax hyper-if 
  (syntax-rules ()
    ((_ condition on-true on-false) 
     (let ((bound-condition condition)) 
      (check-boolean-or-hyperboolean bound-condition)
      (memoize 
        (lambda (i) (if (hyper-ref bound-condition i) 
                        (hyper-ref on-true i)
                        (hyper-ref on-false i))))))))

(define (index-hyperreal)
  (lambda (x) (+ x 1)))

(define (vector-operation-1 operator)
  (lambda (input-vector)
    (vector-map operator input-vector)))

(define (vector-fill reference-vector filler)
  (if (not (vector? filler))
      (make-vector 
        (vector-length reference-vector) 
        filler)
      filler))

(define (vector-or-scalar-operation-2 scalar-operator)
  (lambda (x y)
    (if (or (vector? x) (vector? y))
        (let ((vector-1 (vector-fill x y))
              (vector-2 (vector-fill y x))) 
          (vector-map
            (lambda (element-1 element-2) 
              (scalar-operator element-1 element-2))
            vector-1
            vector-2))
        (scalar-operator x y))))

(define (vector-or-scalar-operation-1 scalar-operator)
  (lambda (x) (if (vector? x)
                  (vector-map scalar-operator x)
                  (scalar-operator x))))

(define (scalar-vector-operation operation)
  (lambda (input-vector scalar)
    (vector-map
      (lambda (element) (operation element))
      input-vector)))

(define (scalar-or-vector-operation-chained scalar-operator)
  (hyper-operation-chained 
                        (vector-or-scalar-operation-2 scalar-operator)))

;; Addition of two hyperreal numbers
(define add-hyperreal (scalar-or-vector-operation-chained +))
(define sub-hyperreal (scalar-or-vector-operation-chained -))
(define mul-hyperreal (scalar-or-vector-operation-chained *))
(define div-hyperreal (scalar-or-vector-operation-chained /))

(define (hyper-scalar-or-vector-operation-2 scalar-operator)
  (hyper-operation-2
    (vector-or-scalar-operation-2 scalar-operator)))

(define expt-hyperreal (hyper-scalar-or-vector-operation-2 expt))

(define lte-hyperreal (hyper-operation-2 <=))
(define gte-hyperreal (hyper-operation-2 >=))
(define gt-hyperreal (hyper-operation-2 >))

(define (hyper-scalar-or-vector-operation-1 scalar-operator)
  (hyper-operation-1 
    (vector-or-scalar-operation-1 
      scalar-operator)))

(define reciprocal-hyperreal 
  (hyper-scalar-or-vector-operation-1
    (lambda (x) (/ 1 x))))
(define exp-hyperreal (hyper-scalar-or-vector-operation-1 exp))
(define neg-hyperreal (hyper-scalar-or-vector-operation-1 -))
(define sqrt-hyperreal (hyper-scalar-or-vector-operation-1 sqrt))
(define square-hyperreal (hyper-scalar-or-vector-operation-1 (lambda (x) (expt x 2))))

(define hyper-vector-ref (hyper-operation-2 vector-ref))

(define sum (hyper-operation-1 
              (lambda (x) (if (vector? x)
                              (vector-fold add-hyperreal 0 x)
                              x))))

(define (vector-norm-squared vect)
  (sum (square-hyperreal vect)))

(define (vector-norm vect)
  (sqrt-hyperreal (vector-norm-squared vect)))

(define (normalize-hyper-vector hyper-vector)
  (mul-hyperreal 
    (div-hyperreal hyper-vector 
                   (hyper-vector-norm hyper-vector))))

(define (check-function func)
  (if (not (procedure? func)) 
      (error "Not a function" func)))

(define (aitken-extrapolate func)
  (check-function func)
  (lambda (n) 
    (let* ((xn (func n))
           (xn+1 (func (+ n 1)))
           (xn+2 (func (+ n 2)))
           (delta-xn (- xn+1 xn))
           (delta-squared-xn (+ xn (- (* 2 xn+1)) xn+2))
           )
      (if (= 0 delta-squared-xn) 
          xn
          (- xn (/ (expt delta-xn 2) delta-squared-xn))))))

(define (standard-part h #!optional (n 1000) #!key (extrapolate? #t))
  (if (procedure? h) 
      (if extrapolate? 
          ((aitken-extrapolate (lambda (n) (hyper-ref h n))) 
           n)
          (hyper-ref h n))
      h))

;; Create an infinitesimal hyperreal number representing the sequence 1/n
(define infinitesimal
  (div-hyperreal 1 (expt-hyperreal 2 (index-hyperreal))))

(define negative-infinitesimal
  (neg-hyperreal infinitesimal))

;; Create an infinitesimal hyperreal number representing a smaller infinitesimal 1/n^2
(define infinitesimal-squared
  (reciprocal-hyperreal (expt-hyperreal (index-hyperreal) 2)))

(define (differentiate f #!key (order 1) (direction 'forward))
  (check-function f)
  (let ((dx (if (equal? direction 'forward) 
                infinitesimal
                (neg-hyperreal infinitesimal))))
    (hyper-if (lte-hyperreal order 0) 
                  f
                  (differentiate 
                    (lambda (x) 
                      (div-hyperreal 
                        (sub-hyperreal 
                          (f (add-hyperreal dx x)) 
                          (f x)) 
                        dx))
                    order: (- order 1)))))

(define (integrate f a b)
  (check-function f)
  (letrec ((delta-x infinitesimal)
           (do-integration 
             (lambda (x running-sum) 
               (hyper-if (gte-hyperreal x b) 
                             running-sum
                             (do-integration 
                               (add-hyperreal delta-x x) 
                               (add-hyperreal (mul-hyperreal delta-x (f x)) 
                                              running-sum))))))
    (do-integration a 0)))

(define (ode-integrate f df/dt t)
  (check-function f)
  (check-function df/dt)
  (let* ((dt negative-infinitesimal)
         (previous-t (add-hyperreal t dt))
         (previous-y (f previous-t))
         (dy (mul-hyperreal 
               (df/dt t previous-y) 
               dt)))
    (sub-hyperreal previous-y dy)))

(define (print-hyperreal h #!optional (n 10))
  (print (map (lambda (i) (hyper-ref h i)) (iota n))))
