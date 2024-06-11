#lang racket 

(system "git clone https://github.com/overshiki/overshiki.github.io.git")
(system "rm -rf __site")
(system "mv overshiki.github.io __site")