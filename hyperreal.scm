(import srfi-1 srfi-133 srfi-69)

(: make-nonstandard (ultrafilter-type --> nonstandard-type))
(define (make-nonstandard f)
  (assert (procedure? f))
  (cons 'hyper f))

(: nonstandard? (* --> boolean : nonstandard-type))
(define (nonstandard? x)
  (and (pair? x) (eq? (car x) 'hyper)))

(define-specialization (nonstandard? (x (not nonstandard-type))) #f)

(: nonstandard-ref (nonstandard-type fixnum --> base-type))
(define (nonstandard-ref x i)
  (assert (nonstandard? x))
  ((cdr x) i))

;; Access the nth element of a hyper number
(: hyper-ref (hyper-type fixnum --> base-type))
(define (hyper-ref x i)
  (assert (integer? i))
  (if (nonstandard? x)
      (nonstandard-ref x i)
      x))

(define-specialization (hyper-ref (x (not nonstandard-type)) (i fixnum)) x)

(: memoize-hyper (hyper-type --> hyper-type))
(define (memoize-hyper h)
  (if (nonstandard? h)
      (let ((previous-i -1)
            (previous-y '()))
        (make-nonstandard 
          (lambda (i) 
            (assert (>= i 0))
            (if (= previous-i i) 
                previous-y
                (begin
                  (set! previous-i i)
                  (set! previous-y (hyper-ref h i))
                  previous-y)))))
      h))

(: make-2-operation (elemental-operator-type #!optional boolean --> 2-operator-type))
(define (make-2-operation operator #!optional (convert-scalar-to-vector? #f))
  (define (fill-vector reference-vector scalar)
    (make-vector (vector-length reference-vector) 
                 scalar))
  (define (2-operation x y) 
    (define (scalar-operation x y)
      (operator x y))
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

(: hyper-2-operation (2-operator-type -> hyper-2-operator-type))
(define (hyper-2-operation 2-operation)
  (lambda (x y) 
    (if (or (nonstandard? x) 
            (nonstandard? y)) 
        (make-nonstandard 
          (lambda (i) 
            (assert (integer? i))
            (2-operation (hyper-ref x i) 
             (hyper-ref y i))))
        (2-operation x y))))

(: operation-chained (hyper-2-operator-type -> chained-operator-type))
(define (operation-chained 2-operation)
  (assert (procedure? 2-operation))
  (define (chained-operation first-arg second-arg . remaining-args)
    (let ((accumulated-result (2-operation first-arg second-arg)))
     (if (null? remaining-args) 
         accumulated-result
         (apply chained-operation accumulated-result remaining-args))))
  chained-operation)

(: chained-hyper-operation (elemental-operator-type #!optional boolean --> chained-operator-type))
(define (chained-hyper-operation operator #!optional (convert-scalar-to-vector? #f))
  (operation-chained 
    (hyper-2-operation 
      (make-2-operation operator 
                        convert-scalar-to-vector?))))

(define (make-hyper-2-operation operator)
  (hyper-2-operation
    (make-2-operation operator)))

(: add-hyper hyper-arithmetic-operator)
(define add-hyper (chained-hyper-operation +))
(: subtract-hyper hyper-arithmetic-operator)
(define subtract-hyper (chained-hyper-operation -))

(: multiply-hyper hyper-arithmetic-operator)
(define multiply-hyper (chained-hyper-operation * #t))
(: divide-hyper hyper-arithmetic-operator)
(define divide-hyper (chained-hyper-operation / #t))

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
     (if (nonstandard? x) 
         (make-nonstandard 
           (lambda (i) 
             (operation (hyper-ref x i))))
         (operation x)))))

(define-syntax hyper-if 
  (syntax-rules ()
    ((_ condition on-true on-false) 
     (let ((bound-condition condition)) 
      (if (nonstandard? bound-condition) 
          (make-nonstandard 
            (lambda (i)  
              (if (hyper-ref bound-condition i)
                  (hyper-ref on-true i)
                  (hyper-ref on-false i))))
          (if bound-condition on-true on-false))))))

(: hyper-map ((#!rest base-type -> base-type) #!rest hyper-type --> nonstandard-type))
(define (hyper-map func #!rest hyper-vals)
  (make-nonstandard 
    (lambda (i)
      (assert (integer? i))
      (apply func 
             (map 
               (lambda (hyper) 
                 (hyper-ref hyper i)) 
               hyper-vals)))))

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


(define index-hyper
  (make-nonstandard (lambda (i) (+ i 1))))

(: expt-hyper hyper-arithmetic-2-operator)
(define expt-hyper (make-hyper-2-operation expt))

(: lte-hyper hyper-comparison-operator)
(define lte-hyper (make-hyper-2-operation <=))
(: gte-hyper hyper-comparison-operator)
(define gte-hyper (make-hyper-2-operation >=))
(: gt-hyper hyper-comparison-operator)
(define gt-hyper (make-hyper-2-operation >))
(: eq-hyper hyper-comparison-operator)
(define eq-hyper (make-hyper-2-operation =))

(define (reciprocal-hyper x) 
  (divide-hyper 1 x))
(define exp-hyper (hyper-operation-1 exp))
(define negate-hyper (hyper-operation-1 -))
(define sqrt-hyper (hyper-operation-1 sqrt))
(define square-hyper (hyper-operation-1 (lambda (x) (expt x 2))))

(: sum-vector-elements hyper-vector-reduce-operator)
(hyper-define (sum-vector-elements vect) 
              (assert (vector? vect))
              (vector-fold add-hyper 0 vect))

(: vector-dot hyper-vector-reduce-2-operator)
(define (vector-dot vector-1 vector-2)
  (sum-vector-elements 
    (multiply-hyper vector-1 vector-2)))

(: hyper-vector-norm-squared hyper-vector-reduce-operator)
(define (hyper-vector-norm-squared vect)
  (vector-dot vect vect))

(: hyper-vector-norm hyper-vector-reduce-operator)
(define (hyper-vector-norm vect)
  (sqrt-hyper (hyper-vector-norm-squared vect)))

(: normalize-hyper-vector (hypervector --> hypervector))
(define (normalize-hyper-vector hyper-vector)
  (divide-hyper hyper-vector 
                (hyper-vector-norm hyper-vector)))

(: aitken-extrapolate ((integer -> realfield) integer -> realfield))
(define (aitken-extrapolate func n)
  (assert (procedure? func))
  (assert (integer? n))
  (let* ((xn (func n))
         (xn+1 (func (+ n 1)))
         (xn+2 (func (+ n 2)))
         (delta-xn (subtract-hyper xn+1 xn))
         (delta-squared-xn (add-hyper xn 
                                      (negate-hyper (multiply-hyper 2 xn+1)) 
                                      xn+2)))
    (hyper-if (eq-hyper (multiply-hyper 0 delta-squared-xn) 
                        delta-squared-xn) 
              xn
              (subtract-hyper xn 
                              (divide-hyper (expt-hyper delta-xn 2) 
                                            delta-squared-xn)))))

(define (standard-part h #!optional (n 1000) #!key (extrapolate? #t))
  (assert (integer? n))
  (if (nonstandard? h) 
      (if extrapolate? 
          (aitken-extrapolate (lambda (n) (hyper-ref h n)) n) 
          (hyper-ref h n))
      h))

;; Create an infinitesimal hyper number representing the sequence 1/n
(define infinitesimal
  (divide-hyper 1 (expt-hyper 2 index-hyper)))

(define negative-infinitesimal
  (negate-hyper infinitesimal))

(define infinity (expt-hyper 2 index-hyper))

;; Create an infinitesimal hyper number representing a smaller infinitesimal 1/n^2
(define infinitesimal-squared
  (reciprocal-hyper (expt-hyper index-hyper 2)))

(: integrate ((hyperfield -> hyperfield) hyperfield hyperfield #!optional hyperfield -> hyperfield) )
(define (integrate f a b #!optional (step-size infinitesimal))
  (define (do-integration x running-sum) 
    (hyper-if (gte-hyper x b) 
              running-sum
              (do-integration 
                (memoize-hyper (add-hyper step-size x)) 
                (memoize-hyper (add-hyper 
                                 (multiply-hyper step-size (f x)) 
                                 running-sum)))))
  (assert (procedure? f))
  (do-integration a 0))

(: differentiate ((hyperfield -> hyperfield) #!optional hyperfield --> (hyperfield -> hyperfield)))
(define (differentiate f #!optional (step-size infinitesimal))
  (lambda (x) 
    (divide-hyper 
      (subtract-hyper 
        (f (add-hyper step-size x)) 
        (f x)) 
      step-size)))

(: ode-solve ((number-type base-type (number-type -> number-type) -> base-type) 
              hyperreal 
              hyperreal 
              base-type 
              #!optional hyperreal 
              --> 
              base-type))
(define (ode-solve update-function 
                   initial-time
                   final-time 
                   initial-state 
                   #!optional (step-size infinitesimal))
  (define (differential derivative) 
    (multiply-hyper derivative step-size)) 
  (define (update-ode-state current-time current-state)
    (hyper-if (gte-hyper current-time final-time) 
              current-state
              (let ((next-state 
                      (memoize-hyper
                        (update-function current-time 
                                         current-state 
                                         differential)))
                      (next-time
                        (memoize-hyper
                          (add-hyper current-time step-size))))
                (update-ode-state next-time next-state))))
  (assert (procedure? update-function))
  (update-ode-state (memoize-hyper initial-time) (memoize-hyper initial-state)))

(: gradient-descent ((hyperfield -> hyperfield) 
                     hyperfield 
                     hyperfield 
                     #!optional hyperfield hyperfield 
                     -> hyperfield))
(define (gradient-descent f gain initial-location 
                          #!optional (step-size infinitesimal) (min-step-change infinitesimal))
  (let ((gradient (differentiate f step-size)))
   (define (do-descent current-location current-step-count)
     (hyper-if (gte-hyper current-step-count num-steps)
               current-location
               (do-descent (memoize-hyper
                             (subtract-hyper current-location 
                                             (multiply-hyper (gradient current-location) 
                                                             gain)))
                           (memoize-hyper
                             (add-hyper current-step-count 1)))))
   (do-descent initial-location 0)))

