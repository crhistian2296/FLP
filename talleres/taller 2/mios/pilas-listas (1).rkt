#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Contructor:

;Empty-stack:
(define empty-stack (lambda () '(empty-stack)))

;stack:
(define stack
  (lambda (l)
    (cons 'stack l)))

;Extractores:

(define rest-stack (lambda (l) (cdr l)))

(define without-first (lambda (l) (cddr l)))

(define first-stack (lambda (l) (cadr l)))

;;;;;;;;;;;Revisar cons , para ver si es posible quitar cualquier indicio de que se implementa con listas
;;;;Preguntar a juan marcos si es necesario ahre.
;Codigo cliente:
;push:
(define push
  (lambda (element l)
    (cons 'stack (cons element (rest-stack l)))))


;pop:
(define pop
  (lambda (l)
    (cons 'stack (without-first l))))

;top:

(define top
  (lambda (l)
    (first-stack l)))

;Predicados:
;empty-stack?

(define empty-stack?
  (lambda (l)
    (if (eq? (car l) 'empty-stack) #t #f)))

;stack?
(define stack?
  (lambda (l)
    (if (eq? (car l) 'stack) #t #f)))

