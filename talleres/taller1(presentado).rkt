#lang eopl
;;MELISSA GONZÁLEZ NEBRIJO - 1832127
;;MICHELLE GONZÁLEZ HERNANDEZ - 1871074
;;CRHISTIAN ALEXANDER GARCÍA URBANO - 1832124
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N°1
;;invert:
;;propósito:
;;L --> L: Recibe una lista que contiene listas de 2 elementos "x" ,y "y" retorna una lista similar con los elementos
;; en orden invertido
;;<lista>::= ()
;;       ::= (<lista (<valor-uno-de-scheme>)(<valor-dos-de-scheme>)>) 

(define invert (lambda (L)
                (letrec
                 (
                   [invertPair (lambda (L) (cons (cons (cadr L) (cons (car L)empty)) empty))]; Recibe una lista de dos elementos y los
                                                                                             ; pone en orden invertido.
                   [invert-aux (lambda (L)(if (null? L)
                     empty
                    (append (invertPair (car L)) (invert (cdr L)))))];Funcion recursiva que llama a invertPair con cada uno de los elementos
                                                                     ;de la lista.
                  )
                  (invert-aux L))))

;;Pruebas:
;;(invert '((a 1) (a 2) (1 b) (2 b)))
;;(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
;;(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))
;;(invert '((2 3)(s e)("racket" "FLP")))
;;(invert '(("Taller" "uno")(4 3)(a b)(o p)))
;;(invert '(("dos" "tres" )(7 8)("help"  "send")(c b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N°2
;;down:
;;Propósito:
;; L -> L': Procedimiento que retorna cada elemento de la lista asociado a un nivel más de paréntesis
;;comparado con su estado original.
;; <lista>::= ()
;;        ::=(<lista>)


(define down
  (lambda (L)
    (if (eq? L '())'()
        (cons(list (car L))(down (cdr L))))))


;;pruebas:
;;(down '(1 2 3))
;;(down '((una) (buena) (idea)))
;;(down '(un (objeto (mas)) complicado))
;;(down '(("dos")(300)((2))))
;;(down '((envia)((un))(((abrazo)))))
;;(down '((100)((200))(((300)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 3
;;list-set:
;;propósito:
;; lista numero elemento -> lista: Procedimiento que retorna una lista similar a la que recibe (L),ingresando en la posición n un
;;elemento x
;;<lista> ::= ()
;;        ::= (<valor-de-scheme> <lista>)

(define (list-set l n x)
    (letrec(
            (listaAuxA (lambda (listaAux nAux xAux contador)
                         ;; funcion auxiliar que recibe una lista, un numero, una simbolo y un contador, este contador
                         ;; guarda la posicion para comparar con el numero de posicion dado y al coincidir ambos valores
                         ;; se sustituye el elemento en la lista por el simbolo dado
                   (cond
                     [(null? listaAux) empty]
                     [(equal? nAux contador)
                          (cons xAux (listaAuxA (cdr listaAux) nAux xAux (+ 1 contador)))]
                     [else (cons (car listaAux) (listaAuxA (cdr listaAux) nAux xAux (+ 1 contador)))])
                   )));
      (listaAuxA l n x 0)))


;;Pruebas:
;;(list-set '(a b c d) 2 '(1 2))
;;(list-set '(a b c d) 3 ’(1 5 10))
;;(list-set '(j k l m) 3 'g)
;;(list-set '(a c d e) 1 'b)
;;(list-set '(1 2 2 4) 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N°4
;;filter-in:
;;propósito:
;;P x L - L: Recibe un predicado y una lista, se encarga de retornar la nueva lista que se obtenga por medio del filtrado
;;con el predicado P
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)


(define filter-in (lambda (P L)
                    (cond
                      [(null? L) '()]
                      [(equal? (P (car L)) #t) (cons (car L)(filter-in P (cdr L)))]
                      [else (filter-in P (cdr L))])))


;;Pruebas:
;;(filter-in number? '(a 2 (1 3) b 7))
;;(filter-in symbol? '(a (b c) 17 foo))
;;(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
;;(filter-in number? '("pc" (3 4)300))
;;(filter-in symbol? '(120 200 (3 4) 300))
;;(filter-in null? '(120 200 (3 4) 300))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 5
;;list-index
;;propósito:
;;P, L -> number: Este procedimiento se encarga de retornar la posición del primer elemento que satisface el
;;predicado recibido. 
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)

(define list-index
  (lambda (P L)
    (cond
      [(null? L)#f]
      [else
       (letrec
           (
            (funcion-aux (lambda (P L acc);; Esta función recibe, un predicado, una lista y un acumulador, en este
                                          ;; ultimo es donde almacena la posición del primer elemento que cumpla
                                          ;; con el predicado. 
                           (cond
                            [(null? L)#f]
                            [(P (car L))acc]
                            [else (funcion-aux P (cdr L) (+ acc 1))])
                           )
                         )
            )
         (funcion-aux P L 0))])))

;;Pruebas:

;;(list-index number? '(a 2 (1 3) b 7))
;;(list-index symbol? '(a (b c) 17 foo))
;;(list-index symbol? '(1 2 (a b) 3))
;;(list-index number? '(a b c (2 3)))
;;(list-index symbol? '(a (b c) (2 3)))
;;(list-index symbol? '(2 (b c) (2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 6
;;swapper:
;; elemento elemento lista -> lista: La función retorna una lista similar a L, solo que cada ocurrencia anterior de E1 es reemplazada
;; por E2 y cada ocurrencia anterior de E2 es reemplazada por E1.
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)

(define (swapper E1 E2 l)
  (cond
    [(null? l) empty]
    [(equal? E1 (car l)) (cons E2 (swapper E1 E2 (cdr l)))]
    [(equal? E2 (car l)) (cons E1 (swapper E1 E2 (cdr l)))]
    [else (cons (car l) (swapper E1 E2 (cdr l)))]))

;;pruebas:

;;(swapper 'a 'd '(a b c d))
;;(swapper 'a 'd '(a d () c d))
;;(swapper 'x 'y '(y y x y x y x x y))
;;(swapper 'l 'h '(h k l m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N°7
;;cartesian-product
;;propósito:
;;lista lista -> lista : La función retorna una lista de tuplas que representen el producto cartesiano entre L1 y L2.}
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)


(define cartesian-product (lambda (L1 L2)
                          (letrec
                           ( [cartesian-product (lambda (L1 L2)
                             (cond
                              [(null? L1) '()]
                              [(null? L2) '()]
                              [else (cons (cons (car L1) (cons (car L2) empty)) (cartesian-product L1 (cdr L2)))]
                              )) ] ;Se encarga de hacer el producto cartesiano del primer elemento de la lista 1 con todos los
                                   ;elementos de la segunda. 

                             [aux (lambda (n L) (if (= n 0) empty (append (cartesian-product L L2) (aux (- n 1) (cdr L)))))]
                             ;Llama a la funcion de cartesian-product para aplicarse  sucesivamente con un nuevo
                             ;primer elemento de la lista 1 al llamarla recursivamente con el resto.
                                                                                  
                            )
                             (aux (length L1) L1)
                              )
                            )
                          )

;;Pruebas:
;; (cartesian-product '(a b c) '(x y))
;; (cartesian-product '(p q r) '(5 6 7))
;; (cartesian-product '(1 2 3) '(c b d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 8
;;mapping:
;;Funcion lista lista -> lista: La funcion retorna una lista de pares (a,b) siendo a elemento de L1 y b elemento de L2
;; cumplindose la propiedad que al aplicar la funcion unaria F con el argumento a arroja el numero b.
;;;;<lista>::= ()
;;         ::= (<valor-de-scheme> <lista>)

(define mapping
  (lambda (F L1 L2)
    (cond
      [(and (null? L1)(null? L2))'()]
      [(eq? (F(car L1))(car L2))(cons (list (car L1)(car L2))(mapping F (cdr L1 )(cdr L2)))]
      [else (mapping F (cdr L1 )(cdr L2))])))

;;Pruebas:

;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;; (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
;; (mapping (lambda (d) (+ d 2)) (list 1 2 3) (list 2 4 6))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N°9
;;inversions:
;;Propósito: 
;;Lista -> numero:  Devuelve el número de intercambios necesarios para organizar la lista de entrada de forma ascendente
;;<lista>::= ()
;;         ::= (<valor-de-scheme> <lista>)

(define inversions (lambda (ListaA)
                     (letrec [(nIntercambios (lambda (ListaB) ; devuelve cuantos cambios hizo
                                               (cond [(equal? (length ListaB) 0) 0]
                                                     [(equal? (length ListaB) 1) 0]
                                                     [else (if (> (car ListaB) (cadr ListaB))
                                                               (+ 1 (nIntercambios (cddr ListaB)))
                                                               (+ 0(nIntercambios (cdr ListaB))))])))
                              (organizar (lambda (listaC) ; Retorna la lista ya organizada
                                          (cond [(equal? (length listaC) 0) listaC]
                                                [(equal? (length listaC) 1) listaC]
                                                [else (if (> (car listaC) (cadr listaC))
                                                          (cons (cadr listaC) (cons (car listaC) (organizar (cddr listaC))))
                                                          (cons (car listaC)(organizar (cdr listaC))))])))
                              ];;fin de las asignaciones
                       (if (equal? (nIntercambios ListaA) 0) 0
                           (+ (nIntercambios ListaA) (inversions (organizar ListaA)))))
                     ))

;;Pruebas:
;;(inversions '(2 3 8 6 1))
;;(inversions '(1 2 3 4))
;;(inversions '(3 2 1))
;;(inversions '(5 6 9 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 10
;;up:
;;Propósito:
;;Lista -> lista: Remueve un par de paréntesis a cada elemento del nivel más alto de la lista.
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)


(define up (lambda (L)
             (cond
               [(null? L) '()]
               [(list? (car L)) (append (cons (caar L) (cdr (car L))) (up (cdr L)))]
               [else (cons (car L) (up (cdr L)))]
               )
             ))
;;pruebas:
;;(up '((1 2) (3 4)))
;;(up '((x (y)) z))
;;(up '(((2))(c)(("hello"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 11
;;zip:
;;Propósito: 
;; Función, lista, lista -> lista de números: Este procedimiento retorna la lista donde la posición n-ésima corresponde al resultado
;; de aplicar la función F sobre los elementos en la posición n-ésima en L1 Y L2. 
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)

(define zip
  (lambda (F L1 L2)
    (cond
      [(and (null? L1)(null? L2))'()]
      [else (cons (F (car L1)(car L2))(zip F (cdr L1)(cdr L2)))])))


;; pruebas:
;;(zip + '(1 4) '(6 2))
;;(zip * '(11 5 6) '(10 9 8))
;;(zip * '(2 2 4)'(2 2 4))
;;(zip - '(2 3)'(3 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 12
;;filter-acum:
;;Propósito:
;; numero numero funcion numero predicado -> numero: El procedimiento filter-acum aplica lafuncion F a todos los elementos
;; que están en el intervalo [a, b] y que a su vez todos estos elementos cumplen con el predicado de la funcion filter,
;; el resultado se conserva en acum y se retorna este. 


(define filter-acum (lambda (a b F acum filter)
                      (cond [ (= a b) (if (filter b) (F acum b) acum)]
                            [else (if (filter a) (F a (filter-acum (+ a 1) b F acum filter) ) (F acum (filter-acum (+ a 1) b  F acum filter)) )])))
;;pruebas:
;;(filter-acum 1 10 + 0 odd?)
;;(filter-acum 1 10 + 0 even?)
;;(filter-acum 2 20 + 0 null?)
;;(filter-acum 2 20 + 0 symbol?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 13
;;operate:
;;propósito:
;;lista lista -> numero: La función retorna el resultado de aplicar sucesivamente las operaciones en lrators a los valores en lrands.
;;
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)

(define operate (lambda (lrators lrands)
                  (letrec
                      ([invert-Lst (lambda (L) (if (null? L) '() (append (invert-Lst (cdr L)) (list (car L)))))]; Retorna una lista con
                                                                                                                ;todos los elementos en orden invetidos
                                                                                                                ;Esto facilitara el llamada recursivo
                                                                                                                ;en la funcion execute
                       [link (lambda (L1 L2) (append L1 L2))]; Retorna una lista de sale de la union de la 1 con la 2.
                       [tail (lambda (L) (if (null? (cdr L)) (car L) (tail (cdr L))))]; Retorna el ultimo elemento de una lista
                       [body (lambda (L) (if (null? (cdr L)) '() (cons (car L) (body (cdr L)))))] ;Elimina el ultimo elemento
                                                                                                  ;de una lista
                       [execute (lambda (L) (if (null? (cdr L)) (car L) 
                                                        ((car L) (execute (cdr (body L))) (tail L))
                                                        ))] ; Hace la operacion respectiva segun la lista de operadores tomando: un operador,
                                                            ; el llamado recursivo a la funcion
                                                            ; y la cola de la lista. Construyendo asi la expresion a ejecutar.
                       )
                     
                   (execute (link (invert-Lst lrators) lrands)))))

;;pruebas:
;;(operate (list + * + - *) '(1 2 8 4 11 6))
;;(operate (list *) '(4 5))
;;(operate (list + -)'(10 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 14
;;path:
;;Propósito:
;; numero, árbol binario -> lista: recibe un n el cual busca en el arbol binario y retorna la ruta a tomar con
;; cadenas de left y right.
;;<arbol-binario> := (arbol-vacıo) empty
;;                := (nodo) numero <arbol-binario> <arbol-binario>

;;
(define path
  (lambda (n BST)
    (cond
      [(= n (car BST))'()]
      [(if(< n (car BST))
         (cons 'left (path n (cadr BST)))
         (cons 'rigth (path n (caddr BST))))])))

;;pruebas:

;;(path 17 '(14 (7 () (12 () ()))
;;                (26 (20 (17 () ())
;;                        ())
;;                  (31 () ()))))


;;(path 10 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 15
;;count-odd-and-even:
;;propósito:
;;arbol -> lista: Este procedimiento retorna una lista con el conteo de la cantidad de numeros pares e impares que se encuentran en un árbol 
;;<arbol-binario> := (arbol-vacıo) empty
;;                := (nodo) numero <arbol-binario> <arbol-binario>
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)
           
(define count-odd-and-even
  (lambda (arbol)
    (letrec
        (
         (count-predicado
            (lambda (arbol predicado) ;;Revisa si el primer elemento del arbol cumple con el predicado que se reciba, hace la recursión
                                      ;;en este caso sumando uno, si no cumple con esto hace el llamado recursivo
                                      ;;en ambos lados del arbol mientra suma. 
              (cond
                [(null? arbol)0] 
                [(predicado (car arbol))(+ 1 (count-predicado (cadr arbol) predicado)(count-predicado (caddr arbol) predicado))]
                [else (+ (count-predicado (cadr arbol) predicado)(count-predicado (caddr arbol) predicado))])
              )
            )
         )
    (list (count-predicado arbol even?) (count-predicado arbol odd?)))))


;;(count-odd-and-even '(14 (7 () (12 () ()))
;;(26 (20 (17 () ())
;;())
;;(31 () ()))))

                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 16
;;simpson-rule:
;;propósito:
;;calcula la integral de una función f entre los valores a y b mediante la regla de Simpson


(define simpson-rule (lambda (f a b n)
                       (letrec
                           ( [h (/ (- b a ) n)];Definicion de h en base a la regla de simpson y los valores ingresados
                             [yk (lambda (f k) (f (+ a (* k h))) )];Recibe la funcion f y la aplica con el indice k que va de 0 a n. 
                             [sumatoria (lambda (f k n)
                                          (if (= k 0) (* (/ h 3) (+ (yk f k) (sumatoria f (+ k 1) n) ))
                                              (if (= k n) (yk f n)
                                                  (if (odd? k) (+ (* 4 (yk f k)) (sumatoria f (+ k 1) n) ) (+ (* 2 (yk f k)) (sumatoria f (+ k 1) n) ))
                                                  )

                                          ))]; Construye la expresion de sumatoria de la regla de simpson
                            )
                         (sumatoria f 0 n))))

;;pruebas:
;;(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
;;(simpson-rule (lambda (x) x) 1 5 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 17
;;prod-scalar-matrix:
;;propósito:
;; mat x vec -> lista: recibe una matriz y un vector y realiza la multiplicación de estos.
;; <lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)


(define prod-scalar-matriz
  (lambda (mat vec)
    (cond
      [(null? mat)'()]
      [else
       (letrec
           (
            (funcion-aux          ;;Lo que hace esta funcion auxiliar es recibir dos vectores y hacer la multipliación entre estos. 
             (lambda (vec1 vec2)
               (cond
                 [(null? vec1)'()]
                 [else (cons (* (car vec1)(car vec2))(funcion-aux (cdr vec1)(cdr vec2)))])
               )
             )
            )
         (cons (funcion-aux (car mat) vec)(prod-scalar-matriz (cdr mat)vec)))])))
               

;;pruebas:
;;(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;;(prod-scalar-matriz '((2 2)(3 3)) '(2 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PUNTO N° 18
;;pascal:
;;Propósito:
;;numero -> lista: retorna la fila N del triangulo de Pascal
;;<lista>::= ()
;;       ::= (<valor-de-scheme> <lista>)

(define pascal (lambda (n)
               (letrec
                (
                 [pascal-aux (lambda (n)
                               (cond
                                 [(= n 1) 1]
                                 [else (sum-lists (down (list 0 (pascal (- n 1)))) (down (list (pascal (- n 1)) 0))) ]
                                 ))]; Obtiene el nivel n del triangulo de pascal apartir de una suma entre los elementos del nivel n-1

                 [sum-lists (lambda (L1 L2)
                               (if (and (null? L1) (null? L2)) '()
                                   (append (list (+ (car L1) (car L2))) (sum-lists (cdr L1) (cdr L2)))))]; Suma los elementos de dos listas
                                                                                                         ;de igual tamaño sumando los dos elementos
                                                                                                          ;que coinciden en indices.

                 [down (lambda (L) (cond
                           [(null? L)'()]
                           [(if (list? (car L))
                                  (append  (down (car L)) (down (cdr L)) )
                                  (append (list (car L)) (down (cdr L)))
                                 )]))];Elimina todos los parentesis internos de la lista , pasando asi de una anidada a una lista de solo numeros
                                      ;
                 )
                  (pascal-aux n))))


;;pruebas:
;;(pascal 5)
;;(pascal 1)
;;(pascal 11)
;;(pascal 15)


               
            