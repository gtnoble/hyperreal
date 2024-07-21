(import srfi-1)

;; Define a hyperreal number as a sequence of real numbers
(define (reals-seq->hyperreal seq)
  (let ((vect (if (list? seq) 
                  (list->vector seq) 
                  seq))) 
   (lambda (n) (if (n > (- (vector-length vect) 1)) 
                   0 
                   (vector-ref vect n)))))

(define (hyperreal x)
  (cond ((or (list? x) (vector? x)) (reals-seq->hyperreal x))
        (#t x)))

;; Access the nth element of a hyperreal number
(define (hyperreal-ref x n)
  (if (number? x) x (x n)))

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
    (let ((h1 (hyperreal x))
          (h2 (hyperreal y))) 
      (memoize 
        (lambda (n) (operator (hyperreal-ref h1 n) (hyperreal-ref h2 n)))))))

;; Create a hyperreal number representing a sequence like 1/n
(define (operation-1-hyperreal operator)
    (lambda (x) 
    (let ((h (hyperreal x))) 
     (memoize
       (lambda (n) (operator (hyperreal-ref h n)))))))  ;; iota generates a list of integers

(define (index-hyperreal)
  (lambda (x) (+ x 1)))


;; Addition of two hyperreal numbers
(define add-hyperreal (operation-2-hyperreal +))

(define sub-hyperreal (operation-2-hyperreal -))

;; Multiplication of two hyperreal numbers
(define mul-hyperreal (operation-2-hyperreal *))

(define div-hyperreal (operation-2-hyperreal /))

(define expt-hyperreal (operation-2-hyperreal expt))

(define reciprocal-hyperreal (operation-1-hyperreal (lambda (x) (/ 1 x))))

(define (aitken-extrapolate func)
  (lambda (n) 
    (let* ((xn (func n))
          (xn+1 (func (+ n 1)))
          (xn+2 (func (+ n 2)))
          (delta-xn (- xn+1 xn))
          (delta-squared-xn (+ xn (- (* 2 xn+1)) xn+2))
          )
      (- xn (/ (expt delta-xn 2) delta-squared-xn)))))

(define (standard-part h #!optional (n 1000) #!key (extrapolate? #t))
  (if (number? h) 
      h 
      (if extrapolate? 
          ((aitken-extrapolate (lambda (n) (hyperreal-ref h n))) 
           n)
          (hyperreal-ref h n))))

(define (hyperreal-comparison comparator)
  (lambda (x y #!optional (n 1000) #!key (extrapolate? #t)) 
    (comparator (standard-part x n extrapolate?: extrapolate?) (standard-part y n extrapolate?: extrapolate?))))

;; Comparison of two hyperreal numbers (equality)
(define equal-hyperreal? (hyperreal-comparison = ))

(define lt-hyperreal? (hyperreal-comparison < ))
(define gt-hyperreal? (hyperreal-comparison > ))
(define gte-hyperreal? (hyperreal-comparison >= ))


;; Create an infinitesimal hyperreal number representing the sequence 1/n
(define infinitesimal
  (div-hyperreal 1 (expt-hyperreal 2 (index-hyperreal))))

;; Create an infinitesimal hyperreal number representing a smaller infinitesimal 1/n^2
(define infinitesimal-squared
  (reciprocal-hyperreal (expt-hyperreal (index-hyperreal) 2)))

(define (differentiate f)
  (lambda (r) (standard-part 
                (div-hyperreal 
                  (sub-hyperreal 
                    (f (add-hyperreal infinitesimal r)) 
                    (f r)) 
                  infinitesimal))))

(define (integrate f a b #!optional (n 100))
  ((aitken-extrapolate 
     (lambda (n) 
       (letrec ((do-integration 
                  (lambda (x sum) 
                    (print (standard-part x n extrapolate?: #f))
                    ;(print (standard-part b n extrapolate?: #f))
                    (if (gte-hyperreal? x b n extrapolate?: #f) 
                        sum
                        (do-integration 
                          (add-hyperreal infinitesimal x) 
                          (add-hyperreal (mul-hyperreal infinitesimal 
                                                        (f x))
                                         sum))))))
         (standard-part (do-integration a 0) n extrapolate?: #f)))) 
   n))

;; Example usage:
(let* ((h1 1)
       (h2 infinitesimal)
       (h3 infinitesimal-squared)
       (h4 (add-hyperreal h1 h2))
       (h5 (mul-hyperreal h1 h2)))
  (print "First 10 elements of 1 + 1/n:")
  (print (map (lambda (n) (hyperreal-ref h4 n)) (iota 10)))
  (print "First 10 elements of 1 * 1/n:")
  (print (map (lambda (n) (hyperreal-ref h5 n)) (iota 10)))
  (print "First 10 elements of 1/n^2:")
  (print (map (lambda (n) (hyperreal-ref h3 n)) (iota 10)))
  (print "h3 must be less than h2")
  (print (lt-hyperreal? h3 h2)))
  

