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
              (hash-table-set! lookup-table args y)
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

(define (make-2-operation operator #!key (convert-scalar-to-vector? #f))
  (define (fill-vector reference-vector scalar)
    (make-vector (vector-length reference-vector) 
                 scalar))
  (define (2-operation x y) 
    (define (scalar-operation x y)
      (assert (or (number? x) (procedure? x)))
      (assert (or (number? y) (procedure? y)))
      (if (or (procedure? x) (procedure? y))
          (let ((f1 (functionize x))
                (f2 (functionize y)))
            (lambda (#!rest args) (operator (apply f1 args) (apply f2 args))))
          (operator x y)))
    (cond 
      ((and (vector? x) 
            (vector? y)) 
       (vector-map 2-operation x y))
      ((and convert-scalar-to-vector? 
            (or (vector? x) (vector? y))) 
       (if (vector? x) 
           (2-operation x (fill-vector x y))
           (2-operation (fill-vector y x) y)))
      (#t (scalar-operation x y))))
  2-operation)

(define (hyper-2-operation 2-operation)
  (lambda (x y) 
    (if (or (hyper? x) 
            (hyper? y)) 
        (make-hyper 
          (memoize 
            (lambda (i) 
              (assert (integer? i))
              (2-operation (hyper-ref x i) 
               (hyper-ref y i)))))
        (2-operation x y))))

(define (operation-chained 2-operation)
  (assert (procedure? 2-operation))
  (define (chained-operation first-arg second-arg . remaining-args)
               (let ((accumulated-result (2-operation first-arg second-arg)))
                (if (null? remaining-args) 
                    accumulated-result
                    (apply chained-operation accumulated-result remaining-args))))
  chained-operation)

(define (chained-hyper-operation operator #!key (convert-scalar-to-vector? #f))
  (operation-chained 
    (hyper-2-operation 
      (make-2-operation operator 
                        convert-scalar-to-vector?: convert-scalar-to-vector?))))

(define (make-hyper-2-operation operator)
  (hyper-2-operation
    (make-2-operation operator)))

(define add-hyper (chained-hyper-operation +))
(define subtract-hyper (chained-hyper-operation -))

(define multiply-hyper (chained-hyper-operation * convert-scalar-to-vector?: #t))
(define divide-hyper (chained-hyper-operation / convert-scalar-to-vector?: #t))

(define (operation-1 operator)
  (define (operation x) 
    (define (scalar-operation x) 
      (if (procedure? x)
          (lambda (#!rest args) (operator (apply x args)))
          (operator x)))
    (if (vector? x)
        (vector-map operation x)
        (scalar-operation x)))
  operation)

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

(define (hyper-map func #!rest hyper-vals)
  (make-hyper 
    (memoize 
      (lambda (i)
        (assert (integer? i))
        (apply func 
               (map 
                 (lambda (hyper) 
                   (hyper-ref hyper i)) 
                 hyper-vals))))))

(define-syntax hyper-define
  (syntax-rules ()
    ((_ (name) form1 ...)
     (define (name)
       (hyper-map (lambda () form1 ...))))
    ((_ (name arg1 ...) form1 ...)
     (define (name arg1 ...)
       (hyper-map (lambda (arg1 ...) form1 ...)
                  arg1 ...)))))

(hyper-define (hyper-vector-ref hyper-vector index)
              (vector-ref hyper-vector index))

(define (make-hyper-vector #!rest components)
  (apply hyper-map vector components))

(define (index-hyper)
  (make-hyper (lambda (i) (+ i 1))))

(define expt-hyper (make-hyper-2-operation expt))

(define lte-hyper (make-hyper-2-operation <=))
(define gte-hyper (make-hyper-2-operation >=))
(define gt-hyper (make-hyper-2-operation >))
(define eq-hyper (make-hyper-2-operation =))

(define (reciprocal-hyper x) 
  (divide-hyper 1 x))
(define exp-hyper (hyper-operation-1 exp))
(define negate-hyper (hyper-operation-1 -))
(define sqrt-hyper (hyper-operation-1 sqrt))
(define square-hyper (hyper-operation-1 (lambda (x) (expt x 2))))

(hyper-define (sum-vector-elements vect) 
  (assert (vector? vect))
  (vector-fold add-hyper 0 vect))

(define (vector-dot vector-1 vector-2)
  (sum-vector-elements 
    (multiply-hyper vector-1 vector-2)))

(define (hyper-vector-norm-squared vect)
  (vector-dot vect vect))

(define (hyper-vector-norm vect)
  (sqrt-hyper (hyper-vector-norm-squared vect)))

(define (normalize-hyper-vector hyper-vector)
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
      (hyper-if (eq-hyper (multiply-hyper 0 delta-squared-xn) 
                          delta-squared-xn) 
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

(define infinity (expt-hyper 2 (index-hyper)))

;; Create an infinitesimal hyper number representing a smaller infinitesimal 1/n^2
(define infinitesimal-squared
  (reciprocal-hyper (expt-hyper (index-hyper) 2)))

(define (hyper-eval hyperfunction #!rest args)
  (apply hyper-map (lambda (function-slice #!rest arg-slices) 
                     (apply function-slice arg-slices)) 
         hyperfunction args))

(define (integrate f a b #!key (step-size infinitesimal))
  (define (do-integration x running-sum) 
    (hyper-if (gte-hyper x b) 
              running-sum
              (do-integration 
                (add-hyper step-size x) 
                (add-hyper (multiply-hyper step-size (hyper-eval f x)) 
                           running-sum))))
  (assert (procedure? f))
  (do-integration a 0))


(define (differentiate f #!key (step-size infinitesimal))
  (assert (is-hyper? f))
  (assert (is-hyper? step-size))
  (lambda (x) 
    (divide-hyper 
      (subtract-hyper 
        (hyper-eval f (add-hyper step-size x)) 
        (hyper-eval f x)) 
      step-size)))

(define (ode-solve update-function 
                   initial-time
                   final-time 
                   initial-state 
                   #!key (step-size infinitesimal))
  (define (differential derivative) 
    (multiply-hyper derivative step-size)) 
  (define (update-ode-state current-time current-state)
    (hyper-if (gte-hyper current-time final-time) 
              current-state
              (let ((next-state 
                      (update-function current-time 
                                       current-state 
                                       differential))
                    (next-time
                      (add-hyper current-time step-size)))
                (update-ode-state next-time next-state))))
  (assert (procedure? update-function))
  (assert (>= final-time initial-time))
  (update-ode-state initial-time initial-state))

(define (gradient-descent f gain initial-location 
                          #!key (step-size infinitesimal) (num-steps infinity))
  (let ((gradient (differentiate f step-size: step-size)))
   (define (do-descent current-location current-step-count)
     (hyper-if (gte-hyper current-step-count num-steps)
               current-location
               (do-descent (subtract-hyper current-location 
                                           (multiply-hyper (gradient current-location) 
                                                           gain))
                           (add-hyper current-step-count 1))))
   (do-descent initial-location 0)))

(define (print-hyper h #!optional (n 10))
  (print (map (lambda (i) (hyper-ref h i)) (iota n))))
