#lang eopl
;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
;Taller N° 3
;Especificacion Lexica
(define lexica
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (texto (letter (arbno (or letter digit))) string)
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación gramática

(define gramatica
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion ("\""texto"\"")texto-lit)
    (expresion (identificador)var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")" ) primapp-un-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    (expresion ("declarar" "(" identificador "=" expresion (arbno ";" identificador "="  expresion )")" "{" expresion "}")variableLocal-exp)
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("length") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)))

;Construcciones Automaticas
(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

(define interpretador
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (evaluar-programa pgm))
                        (sllgen:make-stream-parser lexica gramatica)))


;;Definicion del tipo de dato ambiente 
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;Ambiente-vacio
;Funcion que crea un ambiente vacio
(define ambiente-vacio  
  (lambda ()
    (empty-env-record)))


;Agregar-variables
;Funcion que crea un ambiente extendido 
(define agregar-variables
  (lambda (syms vals env)
    (extended-env-record syms vals env)))


;Ambiente-inicial
(define ambiente-inicial
  (lambda () (agregar-variables '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (ambiente-vacio))))

;Evaluar-programa
(define evaluar-programa
  (lambda (el-programa)
    (cases programa el-programa
      (un-programa (una-expresion) (evaluar-expresion una-expresion (ambiente-inicial))))))

;Evaluar-expresion
(define evaluar-expresion
  (lambda (la-expresion el-ambiente)
    (cases expresion la-expresion
      (numero-lit (numero) numero) 
      (texto-lit (texto) texto)
      (primapp-bin-exp (primer-expresion operacioncita segunda-expresion)
               (evaluar-ope-bin (evaluar-expresion primer-expresion el-ambiente)
                                    operacioncita
                                    (evaluar-expresion segunda-expresion el-ambiente)))
      (primapp-un-exp (operacioncita expresion) (evaluar-ope-un operacioncita (evaluar-expresion expresion el-ambiente) ))
      (condicional-exp (test-exp true-exp false-exp) (if (valor-verdad? (evaluar-expresion test-exp el-ambiente))
                                                         (evaluar-expresion true-exp el-ambiente)
                                                         (evaluar-expresion false-exp el-ambiente)) )
      (variableLocal-exp (ids exps cuerpo) )
      (var-exp (id) (buscar-variable el-ambiente id))
      )))

;Evaluar operacion
(define evaluar-ope-bin
  (lambda (f-val ope s-val)
    (cases primitiva-binaria ope
      (primitiva-suma () (+ f-val s-val)) 
      (primitiva-resta () (- f-val s-val))
      (primitiva-multi () (* f-val s-val))
      (primitiva-div () (/ f-val s-val)) 
      (primitiva-concat () (string-append f-val s-val)))))


;Evaluar operacion unaria
(define evaluar-ope-un
  (lambda (ope val)
    (cases primitiva-unaria ope
      (primitiva-longitud () (string-length val))
      (primitiva-add1 () (+ val 1) )
      (primitiva-sub1 () (- val 1)))))



;List-find-position
;
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym)))))))



;Valor-verdad?
;Predicado que simula los booleanos

(define valor-verdad?
  (lambda (n)
    (if (= n 0) #f #t)))



#|
(define gramatica
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion ("\""texto"\"")texto-lit)
    (expresion (identificador)var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (primitiva-unaria (expresion)primapp-un-exp)
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("-") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("length") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)))
|#

