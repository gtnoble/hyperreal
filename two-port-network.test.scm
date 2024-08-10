(import srfi-78)

(load "two-port-network.so")

(define resistor-value 10)
(define series-resistor (series-network 
                          (resistor-impedance resistor-value)))
(check (matrix-ref 
         (series-resistor 0) 
         0 1) => resistor-value)

(define shunt-resistor (shunt-network 
                         (resistor-impedance resistor-value)))
(check (matrix-ref 
         (impedance-parameters 
           (network-abcd-matrix shunt-resistor 10))
                   0 0) => resistor-value)

