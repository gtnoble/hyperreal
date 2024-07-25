(import srfi-1)

;; Access the nth element of a hyperreal number
(define (hyperreal-ref x n)
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

(define (operation-2-hyperreal operator)
  (lambda (x y) 
    (if (and (number? x) (number? y)) 
        (operator x y)
          (memoize 
            (lambda (n) (operator (hyperreal-ref x n) (hyperreal-ref y n)))))))

;; Create a hyperreal number representing a sequence like 1/n
(define (operation-1-hyperreal operator)
  (lambda (x) 
    (if (number? x) 
        (operator x)
         (memoize
           (lambda (n) (operator (hyperreal-ref x n)))))))  ;; iota generates a list of integers

(define-syntax switch-hyperreal 
  (syntax-rules ()
    ((_ condition on-true on-false) 
     (memoize
       (lambda (i) (if (hyperreal-ref condition i) 
                     (hyperreal-ref on-true i)
                     (hyperreal-ref on-false i)))))))

(define (index-hyperreal)
  (lambda (x) (+ x 1)))


;; Addition of two hyperreal numbers
(define add-hyperreal (operation-2-hyperreal +))
(define sub-hyperreal (operation-2-hyperreal -))
;; Multiplication of two hyperreal numbers
(define mul-hyperreal (operation-2-hyperreal *))
(define div-hyperreal (operation-2-hyperreal /))
(define expt-hyperreal (operation-2-hyperreal expt))
(define lte-hyperreal (operation-2-hyperreal <=))
(define gte-hyperreal (operation-2-hyperreal >=))

(define reciprocal-hyperreal (operation-1-hyperreal (lambda (x) (/ 1 x))))
(define exp-hyperreal (operation-1-hyperreal exp))
(define neg-hyperreal (operation-1-hyperreal -))

(define (aitken-extrapolate func)
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
  (let ((delta-x (if (equal? direction 'forward) 
                     infinitesimal
                     (neg-hyperreal infinitesimal))))
    (if (= order 0) 
        f
        (differentiate 
          (lambda (x) 
            (div-hyperreal 
              (sub-hyperreal 
                (f (add-hyperreal delta-x x)) 
                (f x)) 
              delta-x))
          order: (- order 1)))))

(define (integrate f a b)
  (letrec ((delta-x infinitesimal)
           (do-integration 
             (lambda (x running-sum) 
               (switch-hyperreal (gte-hyperreal x b) 
                                 running-sum
                                 (do-integration 
                                   (add-hyperreal delta-x x) 
                                   (add-hyperreal (mul-hyperreal delta-x (f x)) 
                                                  running-sum))))))
    (do-integration a 0)))

(define (print-hyperreal h #!optional (n 10))
  (print (map h (iota n))))
