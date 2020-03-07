#lang eopl
;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Gramática:
;<Stack> ::= () | (Int <Stack>) 

;Constructores:

;Empty-stack
;Retorna un lambda que representa la pila vacia
(define empty-stack
  (lambda ()  (lambda (signal) '() ))) 


;Stack
;Retorna un lambda que representa una nueva pila, donde a partir de señales permite extraer sus valores 
(define stack
  (lambda (s)
   (lambda (signal)
     (cond
     [(eq? signal 1) 'stack]
     [(eq? signal 2) (cdr s)]
     [(eq? signal 3) (car s)]
     [(eq? signal 4) s]
     ) )))

;Funcion auxiliar Insert
;Retorna un lambda que inserta un elemento e en una pila s
(define insert (lambda (e s) (cons e s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;extractor:

;Stack-elements
;Retorna los elementos de la pila
(define  stack-elements 
  (lambda (proc) (proc 4)))

;First-element
;Retorna el primer elemento de la pila
(define first-element
  (lambda (proc) (proc 3)))

;Without-first
;Retorna la pila sin el primer elemento 
(define without-first
  (lambda (proc) (proc 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Codigo cliente:

;Pop
; Retira el elemento superior de la pila
(define pop (lambda (s) (without-first s)))

;Top
; Devuelve el elemento superior de la pila
(define top (lambda (s) (first-element s)))

;Push
;Insertar elemento en una pila
(define push (lambda (e s) (insert e (stack-elements s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Predicados:

;Stack?
;Determina si el elemento ingresado es una pila
(define stack? (lambda (s) (eq? (s 1) 'stack)))

;Empty-stack?
;Determina si el elmento ingresado es una pila vacia
(define empty-stack? (lambda (s) (if (null? s) #t #f)))
