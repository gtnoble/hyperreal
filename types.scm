(define-syntax make-nonstandard-type
  (syntax-rules ()
    ((_ base-type)
     (pair symbol (fixnum -> base-type)))))

(define-syntax define-hyper-type
  (syntax-rules ()
    ((_ type-name base-type)
     (define-type type-name (or (pair symbol (fixnum -> base-type))
                                base-type)))))
(define-syntax define-nonstandard-type
  (syntax-rules ()
    ((_ type-name base-type)
     (define-type type-name (pair symbol (fixnum -> base-type))))))

(define-type number-type (or float fixnum cplxnum))
(define-type vector-type (vector-of number-type))

(define-hyper-type hyperreal number-type)
(define-hyper-type hypervector vector-type)
(define-hyper-type hyperboolean boolean)
(define-type hyperfield (or hyperreal hypervector))
(define-type realfield (or number-type vector-type))

(define-type hyper-type (or hyperfield hyperboolean))
(define-type base-type (or boolean number-type vector-type))
(define-nonstandard-type nonstandard-type base-type)
(define-type ultrafilter-type (fixnum -> base-type))

(define-type elemental-operator-type (or (number-type number-type -> boolean) 
                                         (number-type number-type -> number-type)))
(define-type vector-operator-type (or (vector-type number-type -> vector-type) 
                                      (vector-type vector-type -> vector-type)
                                      (number-type vector-type -> vector-type)))
(define-type 2-operator-type (or elemental-operator-type vector-operator-type))
(define-type hyper-2-operator-type (hyper-type hyper-type --> hyper-type))
(define-type chained-operator-type (#!rest hyper-type --> hyper-type))

(define-type hyper-vector-2-operator (hypervector hypervector --> hypervector))
(define-type hyper-vector-reduce-2-operator (hypervector hypervector --> hyperreal))
(define-type hyper-vector-reduce-operator (hypervector --> hyperreal))

(define-type hyper-arithmetic-operator (#!rest hyperfield --> hyperfield))
(define-type hyper-arithmetic-2-operator (hyperfield hyperfield --> hyperfield))
(define-type hyper-comparison-operator 
             (hyperreal hyperreal --> hyperboolean))

