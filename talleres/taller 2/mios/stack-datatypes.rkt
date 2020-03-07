#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Constructor
(define-datatype stack stack?
  (empty-stack)
  (extended-stack (head number?)
         (body stack?)))

;push:
(define push
  (lambda (e s)
    (cases stack s
      (empty-stack () (extended-stack e s))
      (extended-stack (head body) (extended-stack e s))
      )
    ))


