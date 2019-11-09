#lang eopl

;;;;;;;;
;Interfaz: Definición de tipo de dato lambda con define-datatype
;<Lc-exp> ::= <var-exp> (<identifier>)
;         ::= <lambda-exp> (lambda (<identifier>) <Lc-exp>)
;         ::= <app-exp> (<Lc-exp> <Lc-exp>)

(define-datatype lc-exp lc-exp?
   (var-exp (id symbol?) )
   (lambda-exp (bound-var symbol?) (body lc-exp?) )
   (app-exp (rator lc-exp?) (rand lc-exp?) ) )


(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (id)
               (eqv? id search-var))
      (lambda-exp (bound-var body)
                  (and (not ( eqv? search-var bound-var) )
                       (occurs-free? search-var body ) ) )
      (app-exp (rator rand )
               (or(occurs-free? search-var rator)
                  (occurs-free? search-var rand ) ) ) ) ) )

(define e
  (lambda-exp 'x (var-exp 'y)))

(occurs-free? 'x  e)
(occurs-free? 'y  e)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tipo de dato s-list

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list (first s-exp?) (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp (sym symbol?))
  (s-list-s-exp (slst s-list?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tipo de dato binary tree
(define-datatype bintree bintree?
   (leaf-node (datum number?) )
   (interior-node (key symbol?)
                  (left bintree?)
                  (right bintree?) ) )
;Definir un procedimiento que permita encontrar la suma de los
;enteros en las hojas de un  arbol

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (+ (leaf-sum left) (leaf-sum right ) ) ) ) ) )