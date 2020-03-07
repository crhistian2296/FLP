#lang eopl

;Notación de prefijo polaca


;Definición data-type:

(define-datatype prefix-exp prefix-exp?
  (const-exp (num integer?))
  (diff-exp (operand1 prefix-exp?) (operand2 prefix-exp?)))



;Parse
(define parse
  (lambda (pl)
    (cond
      [(and (number? (car pl)) (null? (cdr pl))) (list (list 'const-exp (car pl))(list 'const-exp (cdr pl)))]
      [(and (eq? '- (car pl))
            (number? (cadr pl))
            (number? (caddr pl))) (list 'diff-exp (list 'const-exp (cadr pl)) (list 'const-exp (caddr pl)))]
      [else (list 'diff-exp (parse (cdr pl)) )]
      
      )))







;Unparse
;(define unparse
; (lambda (pL)
;   (cases prefix-exp pL
;     (const-exp (num) )
;     (diff-exp (operand1 operand2)))))



