(import srfi-133 srfi-1)

(load "hyperreal.so")

(define-type matrix (vector (vector hyperreal hyperreal)
                            (vector hyperreal hyperreal)))
(define-type network (hyperreal --> matrix))
(define-type impedance (hyperreal --> hyperreal))
(define-type admittance (hyperreal --> hyperreal))

(: index-matrix (matrix --> (fixnum fixnum -> hyperreal)))
(define (index-matrix matrix)
  (lambda (row column) 
    (matrix-ref matrix (- row 1) (- column 1))))

(: matrix-multiply (matrix matrix --> matrix))
(define (matrix-multiply a b)
  (define (dot-product v1 v2)
    (vector-fold add-hyper 0 (vector-map multiply-hyper v1 v2)))

  (define (get-column m j)
    (vector-map (lambda (row) (vector-ref row j)) m))

  (define (multiply-row row m)
    (vector-map (lambda (j) (dot-product row 
                                         (get-column m j))) 
                #(0 1)))

  (vector-map (lambda (row) (multiply-row row b)) a))

(: matrix-ref (matrix fixnum fixnum --> hyperreal))
(define (matrix-ref matrix row column)
  (vector-ref (vector-ref matrix row) 
              column))

(: matrix-determinant (matrix --> hyperreal))
(define (matrix-determinant matrix)
  (let ((m (index-matrix matrix))) 
   (subtract-hyper (multiply-hyper (m 1 1) (m 2 2))
                   (multiply-hyper (m 1 2) (m 2 1)))))

(: scale-matrix (matrix number --> matrix))
(define (scale-matrix matrix scalar)
  (matrix-multiply matrix (vector (vector scalar 0)
                                  (vector 0 scalar))))

(: make-two-port-network (number number number number --> matrix))
(define (make-two-port-network a b c d)
  (vector (vector a b) 
          (vector c d)))

(: inductor-impedance (hyperreal --> impedance))
(define (inductor-impedance inductance)
  (lambda (complex-frequency) 
    (multiply-hyper complex-frequency inductance)))

(: capacitor-impedance (hyperreal --> impedance))
(define (capacitor-impedance capacitance)
  (lambda (complex-frequency) 
    (divide-hyper 1 (multiply-hyper complex-frequency capacitance))))

(: resistor-impedance (hyperreal --> impedance))
(define (resistor-impedance resistance)
  (lambda (complex-frequency) resistance))

(: series-network (impedance --> network))
(define (series-network z)
  (lambda (complex-frequency) 
    (make-two-port-network 1 (z complex-frequency)
                           0 1)))

(: impedance->admittance (impedance --> admittance))
(define (impedance->admittance z)
  (lambda (complex-frequency) 
    (divide-hyper 1 (z complex-frequency))))

(: shunt-admittance (admittance --> network))
(define (shunt-admittance y)
  (lambda (complex-frequency) 
    (make-two-port-network 1 0
                           (y complex-frequency) 1)))
(: shunt-network (impedance --> network))
(define (shunt-network z)
  (shunt-admittance 
    (impedance->admittance z)))

(: cascade-network (network network --> network))
(define (cascade-network network-1 network-2)
  (lambda (complex-frequency) 
    (matrix-multiply (network-1 complex-frequency) 
                     (network-2 complex-frequency))))

(: impedance-parameters (matrix --> matrix))
(define (impedance-parameters abcd-matrix)
  (let ((abcd (index-matrix abcd-matrix)))
   (scale-matrix (vector (vector (abcd 1 1) (matrix-determinant abcd-matrix))
                         (vector 1 (abcd 2 2)))
                 (divide-hyper 1 (abcd 2 1)))))

(: network-abcd-matrix (network number --> matrix))
(define (network-abcd-matrix network complex-frequency)
  (network complex-frequency))

(: input-impedance (network impedance --> impedance))
(define (input-impedance network load-impedance)
  (lambda (complex-frequency)
    (let* ((impedance-matrix 
             (impedance-parameters 
               (network-abcd-matrix network 
                                    complex-frequency)))
           (z (index-matrix impedance-matrix)))
      (subtract-hyper (z 1 1) 
                      (divide-hyper (multiply-hyper (z 1 2) (z 2 1))
                                    (add-hyper (z 2 2) (load-impedance complex-frequency)))))))

(: output-impedance (network impedance --> impedance))
(define (output-impedance network source-impedance)
  (lambda (complex-frequency)
    (let* ((impedance-matrix 
             (impedance-parameters 
               (network-abcd-matrix network 
                                    complex-frequency)))
           (z (index-matrix impedance-matrix)))
      (subtract-hyper (z 2 2) 
                      (divide-hyper (multiply-hyper (z 1 2) (z 2 1))
                                    (add-hyper (z 1 1) (source-impedance complex frequency)))))))
