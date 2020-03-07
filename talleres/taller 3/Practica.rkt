#lang eopl
;;;;;;Especificacion Lexica
(define lexica
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (texto (letter (arbno (or letter digit))) string)
    (identificador ("@" letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

;;;;;;;Especificacion Gramatica
(define gramatica
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion (primitiva "["(separated-list expresion ";")"]") primapp-exp)
    (primitiva ("+") suma)
    (primitiva ("-") resta)
    (primitiva ("*") multiplicacion)
    (primitiva ("/") div)
    (primitiva ("concat") concat)
    (primitiva ("length") length)
    (primitiva ("add1") add1)
    (primitiva ("sub1") sub1)
    ))

;;;;;;Construcciones Automaticas
(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

(define interpretador
  (sllgen:make-rep-loop "&&> "
                        (lambda (pgm) (eval-programa pgm))
                        (sllgen:make-stream-parser lexica gramatica)))

;;Evaluaciones
;(extend-env ('@x '@y '@z) (5 3 1) empty-env)

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                  (eval-expresion body (extend-env (list '@x '@y '@z) (list 5 3 "hola") empty-env))))))

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim exp-list)
                   (let ((args (eval-exp exp-list env)))
                     (eval-primitiva prim args))))))
(define eval-exp
  (lambda (exp-list env)
    (map (lambda (x) (eval-expresion x env)) exp-list)))

(define eval-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (suma () (recorrer + args))
      (resta () (recorrer - args))
      (multiplicacion () (recorrer * args))
      (div () (recorrer / args))
      (add1 () (+ (car args) 1))
      (sub1 () (- (car args) 1))
      (concat () (recorrer string-append args))
      (length () (string-length (car args))))))


(define recorrer
  (lambda (prim lis)
    (cond
      [(null? lis) 0]
      [(null? (cdr lis)) (car lis)]
      [else (prim (car lis) (recorrer prim (cdr lis)))])))


;;Ambientes representados como listas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-environ)
  (extend-environ (syms (list-of symbol?))
                  (vals (list-of scheme-value?))
                  (environment environment?)))
(define scheme-value? (lambda (x) #t))

;empty-env
(define empty-env
  (empty-environ))

;extend-env
(define extend-env
  (lambda (vars vals env)
    (extend-environ vars vals env)))


;apply-env
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-environ ()
                     (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-environ (syms vals environment)
                      (let ((pos (list-find-position search-var syms)))
                        (if (number? pos)
                            (list-ref vals pos)
                            (apply-env environment search-var)))))))


;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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
