#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;Hacer validacion de implementacion.

;Constructor:

(define empty-stack (lambda ()  (lambda (signal) '() ))) 

(define stack
  (lambda (s)
   (lambda (signal)
     (cond
     [(eq? signal 1) 'stack]
     [(eq? signal 2) (cdr s)]
     [(eq? signal 3) (car s)]
     [(eq? signal 4) s]
     ) )))

(define insert (lambda (e s) (cons e s)))

;extractor:

(define  stack-elements 
  (lambda (proc) (proc 4)))

(define first-element
  (lambda (proc) (proc 3)))

(define without-first
  (lambda (proc) (proc 2)))

;Codigo cliente:
(define pop (lambda (s) (without-first s)))

(define top (lambda (s) (first-element s)))

(define push (lambda (e s) (insert e (stack-elements s))))


;Predicado:
(define stack? (lambda (s) (eq? (s 1) 'stack)))

(define empty-stack? (lambda (s) (if (null? s) #t #f)))