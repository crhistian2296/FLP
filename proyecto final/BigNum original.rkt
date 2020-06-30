#lang eopl


(define x8 8)
(define x16 16)
(define x32 32)
(define base x16)

(define zero '())
(define is-zero? (lambda (n) (null? n)))

(define sucesor
  (lambda (num)
    (cond
      [(is-zero? num) (list 1)]
      [(= (car num) (- base 1))(cons 0 (sucesor (cdr num)))]
      [else (cons (+ 1 (car num)) (cdr num))]
      )))

(define predecesor
  (lambda (num)
         (cond
           [(is-zero? num) (eopl:error "solo se admiten numeros naturales")]
           [(equal? num '(1)) empty]
           [(= (car num) 0) (cons 15 (predecesor (cdr num)))]
           [else (cons (- (car num) 1) (cdr num))]
           )))

(define two  (sucesor (sucesor zero)))
(define three (sucesor (sucesor (sucesor zero))))
(define four (sucesor (sucesor (sucesor (sucesor zero)))))
(define five (sucesor (sucesor (sucesor (sucesor (sucesor zero))))))
(define seven (sucesor  (sucesor  (sucesor  (sucesor  (sucesor  (sucesor  (sucesor zero))))))))
(define ten  (sucesor (sucesor (sucesor (sucesor (sucesor (sucesor (sucesor (sucesor (sucesor (sucesor zero)))))))))))

(define sumar
  (lambda (a b)
    (cond
      [(is-zero? a) b]
      [else (sucesor (sumar (predecesor a) b))])))

(define restar
  (lambda (a b)
    (cond
      [(is-zero? b) a]
      [(is-zero? a) (eopl:error "solo se contemplan numeros naturales")]
      [else (predecesor (restar a (predecesor b)))]
      )))

(define multiplicar
  (lambda (a b)
    (cond
      [(is-zero? b) zero]
      [else (sumar a (multiplicar a (predecesor b)))])))

(define thirty (multiplicar three ten))

(define potencia
  (lambda (a b)
    (cond
      [(is-zero? b) '(1)]
      [else (multiplicar a (potencia a (predecesor b)))])))

(define factorial
  (lambda (n)
    (cond
      [(is-zero? n) '(1)]
      [else (multiplicar n (factorial (predecesor n)))])))