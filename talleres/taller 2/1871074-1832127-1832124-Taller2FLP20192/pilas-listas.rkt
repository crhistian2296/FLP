#lang eopl
;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Implementación de stack con listas
;Gramática
;<stack> :: = 'empty-stack | (int <stack>)

;Constructores:

;stack
;Definición de Stack 
(define stack
  (lambda (l)
    (cons 'stack l)))

;Definición de la pila vacía
;empty-stack:
(define empty-stack
  (lambda ()
    'empty-stack))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Predicados:

;empty-stack?
;Predicado de la pila vacía 
(define empty-stack?
  (lambda (l)
    (if (eq? (car l) empty-stack) #t #f)))

;stack?
;Predicado de una pila 
(define stack?
  (lambda (l)
    (if (eq? (car l) stack) #t #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Extractores:

;restStack:
;Extrae el resto de la pila que recibe
(define restStack
  (lambda (l)
    (cdr l)))

;withoutFirst:
;Retorna la pila sin el primer elemento de la que recibió
(define withoutFirst
  (lambda (l)
    (cddr l)))

;firstStack:
;Retorna el primer elemento de la lista 
(define firstStack
  (lambda (l)
    (cadr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Codigo cliente:

;push:
;Función que inserta un elemento en una pila
(define push
  (lambda (element l)
    (cons 'stack (cons element (restStack l)))))

;pop:
;Función encargada de retirar el elemento superior de la pila 
(define pop
  (lambda (l)
    (cons 'stack (withoutFirst l))))

;top:
;Esta se encarga de devolver el elemento superior de la pila 
(define top
  (lambda (l)
    (firstStack l)))


     