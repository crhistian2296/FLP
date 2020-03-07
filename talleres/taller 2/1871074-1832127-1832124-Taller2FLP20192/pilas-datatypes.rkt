#lang eopl
;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Implementación stack con Datatype
; Gramática:
; <stack> :: = () | (int <stack>)

;empty-stack?
;Definición del predicado de la pila vacía 
(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (extended-stack (head body) #f))))

;stack:
;Implementación de pila con datatype 
(define-datatype stack stack?
  (empty-stack)
  (extended-stack (head number?)
         (body stack?)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Función auxiliar:

;get-body:
;Función auxiliar que se encarga de extraer el primer elemento y retorna el resto de la pila 
(define get-body
  (lambda (s)
    (cases stack s
      (empty-stack () (empty-stack))
      (extended-stack (head body)
          (if
            (empty-stack?  body)
            (empty-stack)(push (top body) (get-body body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Codigo cliente:
;push:
;Inserta un elemento en una pila 
(define push
  (lambda (e s)
    (cases stack s
      (empty-stack () (extended-stack e s))
      (extended-stack (head body) (extended-stack e s)))))


;pop:
;Función que retira el elemento superior de la pila 
(define pop
  (lambda (s)
    (cases stack s
    (empty-stack () eopl:error "You can't pop an empty-stack")
    (extended-stack (head body) (get-body s)))))

;top:
;Devuelve el elemento superior de la pila 
(define top
  (lambda (s)
    (cases stack s
    (empty-stack () eopl:error "You can't top an empty-stack")
    (extended-stack (head body) head))))










