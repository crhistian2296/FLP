 #lang eopl
; 1) REPRESENTACIÓN BIGNUM 32


(define zero '())
(define is-zero? null?)

(define successor
  (lambda (lista)
    (cond
      [(is-zero? lista)'(1)]
      [(< (car lista) 31) (cons (+ 1 (car lista))(cdr lista))]
      [else (cons 0 (successor (cdr lista)))])))

(define predecessor
  (lambda (lista)
    (cond
      [(equal? '(1) lista) '()]
      [(> (car lista) 0) (cons (- (car lista) 1)(cdr lista))]
      [(is-zero? lista) (eopl:error "0 does not have a predecessor")]
      [else (cons 31 (predecessor (cdr lista)))])))


(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

;;PRUEBAS:
;;(suma '(4) '(3)) -> '(7)
;;(suma '(2 3)  '(3 3)) -> '(5 6)

(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

;;PRUEBAS:
;;(resta '(4 4 5) '(3 3 4)) -> '(1 1 1)
;;(resta '(10 9 5 7) '(3 5 5 6)) -> '(7 4 0 1) 

#|(define multiplicacion
  (lambda (x y)
    (if |#


(define multiplicacion
  (lambda (x y)
    (if (is-zero? y) zero
        (suma (multiplicacion x (predecessor y)) x))
    ))

;;PRUEBAS:
;;(multiplicacion '(10) '(2)) -> '(20)

(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))


(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

(define (bignum->nat n)
  (if (null? n) 0 (+ (car n) (* 32 (bignum->nat (cdr n))))))