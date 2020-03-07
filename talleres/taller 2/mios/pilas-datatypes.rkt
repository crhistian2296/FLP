#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;empty-stack
(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (extended-stack (head body) #f))))

;Implementación con Datatype
(define-datatype stack stack?
  (empty-stack)
  (extended-stack (head number?)
         (body stack?)))

;push:
(define push
  (lambda (e s)
    (cases stack s
      (empty-stack () (extended-stack e s))
      (extended-stack (head body) (extended-stack e s)))))

;Funcion auxiliar de pop
(define get-body
  (lambda (s)
    (cases stack s
      (empty-stack () (empty-stack))
      (extended-stack (head body)
          (if
            (empty-stack?  body)
            (empty-stack)(push (top body) (get-body body)))))))
;pop:
(define pop
  (lambda (s)
    (cases stack s
    (empty-stack () eopl:error "You can't pop an empty-stack")
    (extended-stack (head body) (get-body s)))))

;top:
(define top
  (lambda (s)
    (cases stack s
    (empty-stack () eopl:error "You can't top an empty-stack")
    (extended-stack (head body) head))))










