(import srfi-1)

(define (check-real-or-hyperreal value)
  (if (not (or (procedure? value) 
               (number? value)))
      (error "Not a real or hyperreal number" value)))

(define (check-boolean-or-hyperboolean value)
  (if (not (or (procedure? value)
               (boolean? value)))
      (error "Not a boolean or hyperboolean value" value)))

;; Access the nth element of a hyperreal number
(define (hyperreal-ref x n)
  (cond ((procedure? x) (x n)) 
        ((number? x) x)
        (#t (error "Not a real or hyperreal number" x))))

(define (hyperboolean-ref x n)
  (cond ((procedure? x) (x n))
        ((boolean? x) x)
        (#t (error "Not a boolean or hyperboolean value" x))))

(define (memoize func)
  (let ((previous-x '())
        (previous-y '()))
    (lambda (x) (if (equal? previous-x x) 
                    previous-y
                    (begin
                      (set! previous-x x)
                      (set! previous-y (func x))
                      previous-y)))))

(define (operation-2-hyperreal operator)
  (lambda (x y) 
    (check-real-or-hyperreal x)
    (check-real-or-hyperreal y)
    (if (and (number? x) (number? y)) 
        (operator x y)
        (memoize 
          (lambda (n) (operator (hyperreal-ref x n) (hyperreal-ref y n)))))))

(define (operation-chained-hyperreal pairwise-operator)
  (let ((pairwise-operation (operation-2-hyperreal pairwise-operator))) 
   (letrec ((chained-operation
              (lambda (first-arg second-arg . remaining-args)
                (let ((accumulated-result (pairwise-operation first-arg second-arg)))
                 (if (null? remaining-args) 
                     accumulated-result
                     (apply chained-operation accumulated-result remaining-args))))))
     chained-operation)))

;; Create a hyperreal number representing a sequence like 1/n
(define (operation-1-hyperreal operator)
  (lambda (x) 
    (check-real-or-hyperreal x)
    (if (number? x) 
        (operator x)
        (memoize
          (lambda (n) (operator (hyperreal-ref x n)))))))

(define-syntax if-hyperreal 
  (syntax-rules ()
    ((_ condition on-true on-false) 
     (let ((bound-condition condition)) 
      (check-boolean-or-hyperboolean bound-condition)
      (memoize 
        (lambda (i) (if (hyperboolean-ref bound-condition i) 
                        (hyperreal-ref on-true i)
                        (hyperreal-ref on-false i))))))))

(define (index-hyperreal)
  (lambda (x) (+ x 1)))


;; Addition of two hyperreal numbers
(define add-hyperreal (operation-chained-hyperreal +))
(define sub-hyperreal (operation-chained-hyperreal -))
;; Multiplication of two hyperreal numbers
(define mul-hyperreal (operation-chained-hyperreal *))
(define div-hyperreal (operation-chained-hyperreal /))
(define expt-hyperreal (operation-2-hyperreal expt))
(define lte-hyperreal (operation-2-hyperreal <=))
(define gte-hyperreal (operation-2-hyperreal >=))

(define reciprocal-hyperreal (operation-1-hyperreal (lambda (x) (/ 1 x))))
(define exp-hyperreal (operation-1-hyperreal exp))
(define neg-hyperreal (operation-1-hyperreal -))
(define sqrt-hyperreal (operation-1-hyperreal sqrt))
(define square-hyperreal (operation-1-hyperreal (lambda (x) (expt x 2))))

(define-record vector-hyperreal length elements)

(define (check-vector-hyperreal hyperreal-vector)
  (if (not (vector-hyperreal? hyperreal-vector))
      (error "Not a hyperreal vector" hyperreal-vector)))

(define (vector-hyperreal . elements)
  (let ((vector-len (length elements))
        (vector-elements (if (procedure? (car elements)) 
                             (car elements)
                             (let ((elements (list->vector elements)))
                              (lambda (i) (vector-ref elements i)))))) 
    (make-vector-hyperreal 
      vector-len 
      vector-elements)))

(define (vector-hyperreal-ref hyperreal-vector i)
  (check-vector-hyperreal hyperreal-vector)
  ((vector-hyperreal-elements hyperreal-vector) i))

(define (vector-hyperreal-map hyperreal-operator hyperreal-vector)
  (check-vector-hyperreal hyperreal-vector)
  (vector-hyperreal (vector-hyperreal-length hyperreal-vector) 
                    (lambda (i) 
                      (hyperreal-operator (vector-hyperreal-ref hyperreal-vector i)))))

(define (vector-hyperreal-elementwise-operation hyperreal-operator)
  (lambda (vector-1 vector-2) 
    (check-vector-hyperreal vector-1)
    (check-vector-hyperreal vector-2)
    (lambda (i) 
      (hyperreal-operator (vector-hyperreal-ref vector-1 i)
                          (vector-hyperreal-ref vector-2 i)))))

(define add-vector-hyperreal 
  (vector-hyperreal-elementwise-operation add-hyperreal))

(define (vector-norm-squared-hyperreal hyperreal-vector)
  (check-vector-hyperreal hyperreal-vector)
  (letrec ((vector-len (vector-hyperreal-length hyperreal-vector))
           (do-norm 
             (lambda (square-sum i)
               (if-hyperreal (gte-hyperreal i vector-len)
                             square-sum
                             (do-norm 
                               (let ((vector-component (vector-hyperreal-ref hyperreal-vector i))) 
                                (add-hyperreal square-sum 
                                               (square-hyperreal 
                                                 vector-component)))
                               (add-hyperreal 1 i))))))
    (do-norm 0 0)))

(define (vector-norm-hyperreal hyperreal-vector)
  (check-vector-hyperreal hyperreal-vector)
  (sqrt-hyperreal (vector-norm-squared-hyperreal hyperreal-vector)))

(define (mul-scalar-vector-hyperreal hyperreal-scalar hyperreal-vector)
  (check-real-or-hyperreal hyperreal-scalar)
  (check-vector-hyperreal hyperreal-vector)
  (vector-hyperreal-map hyperreal-vector 
                        (lambda (hyperreal-component) 
                          (mul-hyperreal hyperreal-scalar hyperreal-component))))

(define (normalize-hyperreal-vector hyperreal-vector)
  (check-vector-hyperreal hyperreal-vector)
  (mul-scalar-vector-hyperreal 
    (reciprocal-hyperreal (vector-norm-hyperreal hyperreal-vector)) 
    hyperreal-vector))

(define (check-function func)
  (if (not (procedure? func)) (error "Not a function" func)))

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
  (check-real-or-hyperreal h)
  (if (number? h) 
      h 
      (if extrapolate? 
          ((aitken-extrapolate (lambda (n) (hyperreal-ref h n))) 
           n)
          (hyperreal-ref h n))))

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
  (check-real-or-hyperreal order)
  (let ((dx (if (equal? direction 'forward) 
                infinitesimal
                (neg-hyperreal infinitesimal))))
    (if-hyperreal (lte-hyperreal order 0) 
                  f
                  (differentiate 
                    (lambda (x) 
                      (check-real-or-hyperreal x)
                      (div-hyperreal 
                        (sub-hyperreal 
                          (f (add-hyperreal dx x)) 
                          (f x)) 
                        dx))
                    order: (- order 1)))))

(define (integrate f a b)
  (check-function f)
  (check-real-or-hyperreal a)
  (check-real-or-hyperreal b)
  (letrec ((delta-x infinitesimal)
           (do-integration 
             (lambda (x running-sum) 
               (if-hyperreal (gte-hyperreal x b) 
                             running-sum
                             (do-integration 
                               (add-hyperreal delta-x x) 
                               (add-hyperreal (mul-hyperreal delta-x (f x)) 
                                              running-sum))))))
    (do-integration a 0)))

(define (ode-integrate f df/dt t)
  (check-function f)
  (check-function df/dt)
  (check-real-or-hyperreal t)
  (let* ((dt negative-infinitesimal)
         (previous-t (add-hyperreal t dt))
         (previous-y (f previous-t))
         (dy (mul-hyperreal 
               (df/dt t previous-y) 
               dt)))
    (sub-hyperreal previous-y dy)))

(define (print-hyperreal h #!optional (n 10))
  (check-real-or-hyperreal h)
  (print (map (lambda (i) (hyperreal-ref h i)) (iota n))))
