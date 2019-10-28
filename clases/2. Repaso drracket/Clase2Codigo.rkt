#lang eopl
;Definición de un lambda
(lambda (a b)
    (+ a b))

;evaluación de un lambda
((lambda (a b)
    (+ a b)) 3 4)


;definición de la variable cuadrado asociada a un lambda
(define cuadrado
  (lambda (x)
    (* x x))
  )
;invocación
(cuadrado 9)

;Definición de una función rara que recibe una función f
;y la evalua tres veces dentro de una suma
(define funcion-rara
  (lambda (f x y z)
    (+ (f x) (f y) (f z))))

(funcion-rara cuadrado 2 3 4)

;Retornando una función
(define suma-recargada
  (lambda (a b)
    (lambda (x y)
      (+ a b x y))))

(suma-recargada 4 5) ;retorna u procedimiento
((suma-recargada 4 5) 8 7) ;evalúa el procedimiento



(define comparacion1
(lambda (a b)
       (if (> (* a b) 0)
           (* a b)
           (+ a b)
           )
       )
  )

(define comparacion2
(lambda (a b)
  (if
   (or
    (and (> a 0) (> b 0))
    (and (< a 0) (< b 0))
   )
   (* a b)
   (+ a b)
   )
  )
)
