(import srfi-1 srfi-132 srfi-133 srfi-69 binary-search)

(define-record hyper ultraproduct)

(define (is-hyper? x)
  (or #t (vector? x) (boolean? x) (number? x) (procedure? x) (hyper? x)))


;; Access the nth element of a hyper number
(define (hyper-ref x i)
  (assert (is-hyper? x))
  (if (hyper? x)
      ((hyper-ultraproduct x) i)
      x))


(define (memoize func #!key (store-all #f))
  (assert (procedure? func))
  (if store-all 
      (let ((lookup-table (make-hash-table)))
       (lambda (#!rest args)
         (if (hash-table-exists? lookup-table args)
             (hash-table-ref lookup-table args)
             (let ((y (apply func args))) 
              (hash-table-set! lookup-table x y)
              y)))) 
      (let ((previous-args '())
            (previous-y '()))
        (lambda (#!rest args) 
          (if (equal? previous-args args) 
              previous-y
              (begin
                (set! previous-args args)
                (set! previous-y (apply func args))
                previous-y))))))

(define (functionize x)
  (if (procedure? x)
      x
      (lambda (#!rest args) x)))

(define (function-operation-2 operator)
  (lambda (f1 f2)
    (lambda (#!rest args) (operator (apply f1 args) (apply f2 args)))))

(define (scalar-operation-2 operator)
  (lambda (x y)
    (assert (or (number? x) (procedure? x)))
    (assert (or (number? y) (procedure? y)))
    (if (or (procedure? x) (procedure? y))
        (let ((f1 (functionize x))
              (f2 (functionize y)))
          (lambda (#!rest args) (operator (apply f1 args) (apply f2 args))))
        (operator x y))))

(define (vector-operation-2 scalar-operator)
  (lambda (x y)
    (assert (and (vector? x) (vector? y)))
    (vector-map
      scalar-operator
      x
      y)))

(define (operation-2 operator #!key (convert-scalar-to-vector? #f))
  (let* ((scalar-operation (scalar-operation-2 operator))
         (vector-operation (vector-operation-2 scalar-operation))
         (fill-vector (lambda (reference-vector scalar)
                        (make-vector (vector-length reference-vector) 
                                     scalar))))
    (lambda (x y)
      (cond 
        ((and (vector? x) 
              (vector? y)) 
         (vector-operation x y))
        ((and convert-scalar-to-vector? 
              (or (vector? x) (vector? y))) 
         (if (vector? x) 
             (vector-operation x (fill-vector x y))
             (vector-operation (fill-vector y x) y)))
        (#t (scalar-operation x y))))))

(define (hyper-operation-2 number-operator 
                           #!key (convert-scalar-to-vector? #f))
  (let ((operator (operation-2 number-operator 
                               convert-scalar-to-vector?: convert-scalar-to-vector?))) 
   (lambda (x y) 
    (if (or (hyper? x) 
            (hyper? y)) 
        (make-hyper 
          (memoize 
            (lambda (i) 
              (assert (integer? i))
              (operator (hyper-ref x i) 
                        (hyper-ref y i)))))
        (operator x y)))))

(define (operation-chained pairwise-operation)
  (assert (procedure? pairwise-operation))
  (letrec ((chained-operation
             (lambda (first-arg second-arg . remaining-args)
               (let ((accumulated-result (pairwise-operation first-arg second-arg)))
                (if (null? remaining-args) 
                    accumulated-result
                    (apply chained-operation accumulated-result remaining-args))))))
    chained-operation))

(define (chained-hyper-operation operator 
                                 #!key (convert-scalar-to-vector? #f))
  (operation-chained 
    (hyper-operation-2 operator 
                       convert-scalar-to-vector?: convert-scalar-to-vector?)))

(define add-hyper (chained-hyper-operation +))
(define subtract-hyper (chained-hyper-operation -))

(define multiply-hyper (chained-hyper-operation * convert-scalar-to-vector?: #t))
(define divide-hyper (chained-hyper-operation / convert-scalar-to-vector?: #t))

(define (vector-operation-1 scalar-operator)
  (lambda (x) 
    (assert (vector? x))
    (vector-map scalar-operator x)))

(define (operation-1 operator)
  (let* ((scalar-operation 
           (lambda (x)
             (if (procedure? x)
                 (lambda (#!rest args) (operator (apply x args)))
                 (operator x))))
         (vector-operation (vector-operation-1 scalar-operation)))
    (lambda (x)
      (if (vector? x)
          (vector-operation x)
          (scalar-operation x)))))

(define (hyper-operation-1 operator)
  (let ((operation (operation-1 operator))) 
   (lambda (x) 
     (if (hyper? x) 
         (make-hyper 
           (memoize
             (lambda (i) 
               (operation (hyper-ref x i)))))
         (operation x)))))

(define-syntax hyper-if 
  (syntax-rules ()
    ((_ condition on-true on-false) 
     (let ((bound-condition condition)) 
      (if (hyper? bound-condition) 
          (make-hyper 
        (memoize 
          (lambda (i)  
            (if (procedure? bound-condition) 
                (lambda (#!rest args) 
                  (if (apply (hyper-ref bound-condition i) args)
                      (apply (functionize (hyper-ref on-true i)) 
                             args)
                      (apply (functionize (hyper-ref on-false i)) 
                             args)))
                (if (hyper-ref bound-condition i)
                    (hyper-ref on-true i)
                    (hyper-ref on-false i))))))
          (if bound-condition on-true on-false))))))

(define (index-hyper)
  (make-hyper (lambda (i) (+ i 1))))

(define expt-hyper (hyper-operation-2 expt))

(define lte-hyper (hyper-operation-2 <=))
(define gte-hyper (hyper-operation-2 >=))
(define gt-hyper (hyper-operation-2 >))
(define eq-hyper (hyper-operation-2 =))

(define (reciprocal-hyper x) 
  (divide-hyper 1 x))
(define exp-hyper (hyper-operation-1 exp))
(define negate-hyper (hyper-operation-1 -))
(define sqrt-hyper (hyper-operation-1 sqrt))
(define square-hyper (hyper-operation-1 (lambda (x) (expt x 2))))

(define (sum-vector-elements vect) 
  (assert (vector? vect))
  (vector-fold add-hyper 0 x))

(define (vector-dot vector-1 vector-2)
  (assert (vector? vector-1))
  (assert (vector? vector-2))
  (sum-vector-elements 
    (hadamard-multiply-vector vector-1 vector-2)))

(define (hyper-vector-norm-squared vect)
  (assert (vector? vect))
  (vector-dot vect vect))

(define (hyper-vector-norm vect)
  (assert (vector? vect))
  (sqrt-hyper (hyper-vector-norm-squared vect)))

(define (normalize-hyper-vector hyper-vector)
  (assert (vector? vect))
  (divide-hyper hyper-vector 
                 (hyper-vector-norm hyper-vector)))

(define (aitken-extrapolate func)
  (assert (procedure? func))
  (lambda (n) 
    (let* ((xn (func n))
           (xn+1 (func (add-hyper n 1)))
           (xn+2 (func (add-hyper n 2)))
           (delta-xn (subtract-hyper xn+1 xn))
           (delta-squared-xn (add-hyper xn 
                                        (negate-hyper (multiply-hyper 2 xn+1)) 
                                        xn+2)))
      (hyper-if (eq-hyper 0 delta-squared-xn) 
                xn
                (subtract-hyper xn 
                                (divide-hyper (expt-hyper delta-xn 2) 
                                              delta-squared-xn))))))

(define (standard-part h #!optional (n 1000) #!key (extrapolate? #t))
  (assert (integer? n))
  (if (hyper? h) 
      (if extrapolate? 
          ((aitken-extrapolate (lambda (n) (hyper-ref h n))) 
           n)
          (hyper-ref h n))
      h))

;; Create an infinitesimal hyper number representing the sequence 1/n
(define infinitesimal
  (divide-hyper 1 (expt-hyper 2 (index-hyper))))

(define negative-infinitesimal
  (negate-hyper infinitesimal))

;; Create an infinitesimal hyper number representing a smaller infinitesimal 1/n^2
(define infinitesimal-squared
  (reciprocal-hyper (expt-hyper (index-hyper) 2)))

(define (integrate f a b #!key (step-size infinitesimal))
  (hyper-map (lambda (f a b step-size) 
               (assert (procedure? f))
               (letrec ((delta-x step-size)
                        (do-integration 
                          (lambda (x running-sum) 
                            (hyper-if (gte-hyper x b) 
                                      running-sum
                                      (do-integration 
                                        (add-hyper delta-x x) 
                                        (add-hyper (multiply-hyper delta-x (f x)) 
                                                   running-sum))))))
                 (do-integration a 0)))
             f
             a
             b
             step-size))

(define (hyper-map func #!rest hyper-vals)
  (make-hyper 
    (lambda (i)
      (apply func 
             (map 
               (lambda (hyper) 
                 (hyper-ref hyper i)) 
               hyper-vals)))))

(define (hyper-eval hyperfunction #!rest args)
  (apply hyper-map (lambda (function-slice #!rest arg-slices) 
                     (apply function-slice arg-slices)) 
         hyperfunction args))

(define (differentiate f #!key (direction 'forward) (step-size infinitesimal))
  (assert (is-hyper? f))
  (assert (is-hyper? step-size))
  (let ((dx (if (equal? direction 'forward) 
                step-size
                (negate-hyper step-size)))) 
    (hyper-map (lambda (fi dx) 
                 (lambda (x) 
                   (divide-hyper 
                     (subtract-hyper 
                       (fi (add-hyper dx x)) 
                       (fi x)) 
                     dx)))
               f dx)))

(define-record point x y)

(define (make-interpolant-1d coords)
  (assert (vector? coords))
  (let* ((comparison-order (lambda (x1 x2)
                             (- x2 x1)))
         (compare-x-values (lambda (point-1 point-2) 
                             (assert (point? point-1))
                             (assert (point? point-2))
                             (<= 0 (comparison-order (point-x point-1) 
                                                     (point-x point-2)))))
         (ordered-coords (vector-sort compare-x-values coords)))
    (lambda (xi)
      (let* ((before-x (binary-search ordered-coords 
                                      (lambda (candidate) 
                                        (assert (point? candidate))
                                        (comparison-order (point-x candidate) xi))))
             (after-x (add-hyper before-x 1))
             (before-point (vector-ref ordered-coords before-x))
             (after-point (vector-ref ordered-coords after-x)))
        (assert (point? before-point))
        (assert (point? after-point))
        (let* ((delta-x (subtract-hyper (point-x after-point) 
                                        (point-x before-point)))
               (delta-y (subtract-hyper (point-y after-point) 
                                        (point-y before-point)))
               (slope (divide-hyper delta-y delta-x)))
          (add-hyper (point-y before-point) 
                     (multiply-hyper (subtract-hyper xi 
                                                     (point-x before-point))
                                     slope)))))))

(define (ode-solve update-function 
                   final-time 
                   initial-values 
                   #!key (step-size infinitesimal))
  (hyper-map 
    (lambda (update-function final-time initial-values step-size)
      (assert (procedure? update-function))
      (assert (point? initial-values))
      (letrec ((update-ode-state 
                 (lambda (points)
                   (let* ((current-point (car points))
                          (current-time (point-x current-point))
                          (current-state (point-y current-point))) 
                     (hyper-if (gte-hyper current-time final-time) 
                               points
                               (let ((next-state 
                                       (update-function current-time 
                                                        current-state 
                                                        step-size))
                                     (next-time
                                       (add-hyper current-time step-size)))
                                 (update-ode-state 
                                   (cons (make-point next-time next-state) 
                                         points))))))))
        (let ((ode-solution-points 
                (update-ode-state (list initial-values))))
          (make-interpolant-1d (list->vector ode-solution-points)))))
    update-function final-time initial-values step-size))

(define (print-hyper h #!optional (n 10))
  (print (map (lambda (i) (hyper-ref h i)) (iota n))))
