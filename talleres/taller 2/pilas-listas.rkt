#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;Empty-stack:
(define empty-stack '())

;push:

(define push
  (lambda (element stack)
    (cons element stack)))

;pop:

(define pop
  (lambda (stack)
    (cons (cdr stack) empty)))

;top:

(define top
  (lambda (stack)
    (car stack)))

;empty-stack?

(define empty-stack?
  (lambda (stack)
    (if (null? stack) #t #f)))

