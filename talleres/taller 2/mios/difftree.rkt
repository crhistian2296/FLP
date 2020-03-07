#lang eopl
;INTEGRANTES
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO 

;Difftree

;zero
;Función que define el cero en esta gramática
(define zero '(diff (one)(one)))

;diffTree->nat
;Función auxiliar que retorna el resultado de operar el árbol 

(define diffTree->int
  (lambda (l)
    (cond
      [(eq?  (car l) 'one) 1]
      [else (- (diffTree->int (cadr l))(diffTree->int (caddr l)))])))

;is-zero?
;Función que verifica si el valor que retorna la función auxiliar es equivalente a cero 

(define is-zero?
  (lambda (treeValue)
    (if (eq? (diffTree->int treeValue)0)#t #f)))

(define int->diffTree
  (lambda (num)
    (letrec 
        (
         (negativeTree
          (lambda (n)
            (if (eq? n 0) zero
                (list 'diff (negativeTree (+ n 1)) '(one))
                )))
          (positiveTree
           (lambda (n)
             (list 'diff zero (negativeTree (- 0 n)))
             ))
          (makeTree
           (lambda (n)
               (if (< n 0) (negativeTree n) (positiveTree n )
               )))         
         )
      (makeTree num)
      )))

;Successor

(define successor
  (lambda (l)
    (int->diffTree (+ 1 (diffTree->int l)))
    ))

;Predecessor

(define predecessor
  (lambda (l)
    (int->diffTree (- (diffTree->int l) 1))
    ))
          
;diff-tree-plus


(define diff-tree-plus
  (lambda (tree1 tree2)
    (int->diffTree (+ (diffTree->int tree1) (diffTree->int tree2)))
    ))






















