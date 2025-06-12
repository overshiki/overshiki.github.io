#lang racket 

(define (mkdir dir)
  (if (not (directory-exists? dir))
    (system (string-append "mkdir -p " dir))
    '()
  )
)

(mkdir "__site/assets/chp")
(system "cp chp_table.png __site/assets/chp/")

(mkdir "__site/assets/cat")
(system "cp cat-qubits-imgs/* __site/assets/cat/")