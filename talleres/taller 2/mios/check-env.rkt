#lang eopl

;INTEGRANTES:
;1871074 - MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127 - MELISSA GONZÁLEZ NEBRIJO
;1832124 - CRHISTIAN ALEXANDER GARCÍA URBANO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define empty-env (lambda () (list 'empty-env)))

(define extend-env (lambda (var val env) (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env) (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env) (let
                                       ((saved-var (cadr env))
                                        (saved-val (caddr env))
                                        (saved-env (cadddr env)))
                                      (if (eqv? search-var saved-var)
                                          saved-val (apply-env saved-env search-var))))
      (else (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var) (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env) (eopl:error 'apply-env "Bad environment: ~s" env)))


(define extend-env*
  (lambda (vars vals env)
    (list 'extend-env* vars vals env)))

(define check-env (lambda (e n)
  (letrec ((env-levels (lambda (e)
                         (if (eq? (car e) 'empty-env) 0 (+ 1 (env-levels (cadddr e))))))

           (values (lambda (L1 L2)
                     (if (and (null? L1) (null? L2))
                         '() (cons (list (car L1) (car L2)) (values (cdr L1) (cdr L2)))
                         )))
           (return-vars (lambda (e n i)
                          (cond
                            [(> n i) eopl:error  "check-env: Not possible to search depth on environment"]
                            [(= n 0) '()]
                            [(< n i) (return-vars (cadddr e) n (- i 1))]
                            [(and (= n i)(eq? (car e) 'extend-env)) (list (list (cadr e) (caddr e)))]
                            [(and (= n i)(eq? (car e) 'extend-env*)) (values (cadr e) (caddr e))]
                            )))) (return-vars e n (env-levels e))
    )))

      


(define e
(extend-env 'y 8
(extend-env* '(x z w) '(1 4 5)
(extend-env 'a 7
(empty-env)))))







