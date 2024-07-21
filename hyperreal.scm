(import srfi-1)

;; Define a hyperreal number as a sequence of real numbers
(define (reals-seq->hyperreal seq)
  (let ((vect (if (list? seq) 
                  (list->vector seq) 
                  seq))) 
   (lambda (n) (if (n > (- (vector-length vect) 1)) 
                   0 
                   (vector-ref vect n)))))

(define (real->hyperreal number)
  (lambda (n) number))

(define (hyperreal x)
  (cond ((or (list? x) (vector? x)) (reals-seq->hyperreal x))
        ((number? x) (real->hyperreal x))
        (#t x)))


;; Access the nth element of a hyperreal number
(define (hyperreal-ref x n)
  ((hyperreal x) n))

(define (operation-2-hyperreal operator)
  (lambda (x y) 
    (let ((h1 (hyperreal x))
          (h2 (hyperreal y))) 
      (lambda (n) (operator (hyperreal-ref h1 n) (hyperreal-ref h2 n))))))

;; Create a hyperreal number representing a sequence like 1/n
(define (operation-1-hyperreal operator)
  (lambda (x) 
    (let ((h (hyperreal x))) 
     (lambda (n) (operator (hyperreal-ref h n))))))  ;; iota generates a list of integers

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

(define (hyperreal-comparison comparator)
  (lambda (x y #!optional (n 1000) (tol (/ 9 10))) 
    (letrec ((count-successful-comparisons 
               (lambda (x y i sum) 
                 (if (< i 0) 
                     sum
                     (count-successful-comparisons
                       x
                       y
                       (- i 1)
                       (+ sum 
                          (if (comparator (hyperreal-ref x i) 
                                          (hyperreal-ref y i)) 
                              1 
                              0)))))))
      (> (/ (count-successful-comparisons x y n 0) n) 
         tol))))

;; Comparison of two hyperreal numbers (equality)
(define equal-hyperreal? (hyperreal-comparison = ))

(define lt-hyperreal? (hyperreal-comparison < ))
(define gt-hyperreal? (hyperreal-comparison > ))
(define gte-hyperreal? (hyperreal-comparison >= ))

(define (standard-part h #!optional (n 1000))
  (hyperreal-ref h n))

;; Create an infinitesimal hyperreal number representing the sequence 1/n
(define infinitesimal
  (reciprocal-hyperreal (index-hyperreal)))

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
  (letrec ((do-integration 
             (lambda (x sum) 
               (print (standard-part x))
               (if (gte-hyperreal? x b n) 
                   sum
                   (do-integration 
                     (add-hyperreal infinitesimal x) 
                     (add-hyperreal (mul-hyperreal infinitesimal 
                                                   (f x))
                                    sum))))))
    (standard-part (do-integration a 0) n)))

;; Example usage:
(let* ((h1 (real->hyperreal 1))
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
  

