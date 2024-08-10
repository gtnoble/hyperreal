(import (chicken process))
(import (chicken io))
(import srfi-13)

(define (start-spice netlist-file)
  (let-values (((process-output-port process-input-port pid) 
                (process "ngspice" (list "-p" netlist-file))))
              (let* ((execute-command 
                      (lambda (#!rest args)
                        (write-line (string-join args " ") process-input-port)))
                    (exit-spice
                      (lambda ()
                        (execute-command "quit")
                        (process-wait pid)
                        (close-input-port process-output-port)
                        (close-output-port process-output-port))))
                (values execute-command exit-spice))))

(define (execute-spice-command process-input-port #!rest args)
  (write-line (string-join args " ") process-input-port))

(define (alter-device-value process-input-port device-name new-value)
  (execute-spice-command process-input-port "alter" device-name new-value))

(define (write-raw-data process-input-port variable-name output-filename)
  (execute-spice-command process-input-port "wrdata" output-filename variable-name))

(define (run-analysis process-input-port)
  (execute-spice-command process-input-port "run"))

(define (quit-ngspice process-input-port)
  (execute-spice-command process-input-port "quit"))

(let-values (((execute-command exit-spice) (start-spice "test-circuit.spice")))
            (execute-command "unset askquit")
            (execute-command "run")
            (execute-command "wrdata" "output.dat" "output")
            (exit-spice))
