; NONES Todos los problemas nones.
; Implementar el predicado repetidos? que reciba 5 args enteros
; y que determine si alguno de llos se repite. 
(define (repetidos? a b c d e)
  (if (or (equal? a b) 
          (equal? a c) 
          (equal? a d) 
          (equal? a e) 
          (equal? b c) 
          (equal? b d) 
          (equal? b e) 
          (equal? c d) 
          (equal? c e) 
          (equal? d e)) 
      #t
      #f)
)


; En matemáticas el valor de pi se puede calcular a partir
; de la fórmula de la sumatoria infinita de Euler: 
; 2 veces (sumatoria de (2^i * i!^2)/(2i + 1)!
; 2 veces (sumatoria de (2*iexpAnt * (ifactAnt*i)^2)/(denomAnt*(2*i + 1))
(define (pie n)
  (* 2 (+ 1 (pi-aux n 1 1 1 1)))
)
; calculates euler infinite sum - 1
(define (pi-aux n i iexpAnt ifactAnt denomAnt)
  (if (not (equal? n i))
      (+ (calc-pi-term i iexpAnt ifactAnt denomAnt)
         (pi-aux n (+ 1 i) (* 2 iexpAnt) (* ifactAnt i) (* denomAnt (* 2 i) (+ 1 (* 2 i)))))
      0
  )
)
; calculates the term in the euler infinite sum         
(define (calc-pi-term i iexp ifact denom)
  (* 2 iexp (* (* ifact i ) (* ifact i)) 
     (/ 1.0 (* denom (* 2 i) (+ 1 (* 2 i)))) )
)

; Implementar la función recursiva repetidos que determine 
; la cantidad de repetidos en dos listas. 
(define (repetidos l1 l2)
  (repetidos-tail l1 l2 0)
  ;(if (null? l1)
   ;   0
   ;   (+ (repetidos-aux (car l1) l2) (repetidos (cdr l1) l2) )
  ;)
)
; Tail recursive repetidos
(define (repetidos-tail l1 l2 c)
  (if (null? l1)
      c
     (repetidos-tail (cdr l1) l2 (+ (repetidos-aux (car l1) l2) c))
  )
)
; Función auxiliar que encuentra si se repite un elemento en una lista
(define (repetidos-aux elem lista)
  (cond [(null? lista)0]
        [(equal? elem (car lista))1]
        [else (repetidos-aux elem (cdr lista))])
)


; PARES Todos los problemas pares
; RESPUESTA #2
; Funcion que calcula el numero de elementos distintos entre los 5 parametros recibidos
(define (distintos a b c d e) (distintos-aux (list a b c d e) 0))
 
; Funcion auxiliar que de manera recursiva cuenta los elementos distintos de una lista
; El valor de n es el acumulador de los elementos distintos
(define (distintos-aux lista n)
  (cond
    [(null? lista) n]
    [(existe? (car lista) (cdr lista)) (distintos-aux (cdr lista) n)]
    [else (distintos-aux (cdr lista) (+ n 1))]
  )
)
 
; Funcion auxiliar que determina si un elemento existe dentro de una lista
(define (existe? elemento lista)
  (cond
    [(null? lista) #f]
    [(equal? elemento (car lista)) #t]
    [else (existe? elemento (cdr lista))]
  )
)
 
; RESPUESTA #4
; Funcion que calcula el n-esimo termino de la serie donde Sn = Sn-1 * Sn-2
; S1 = 1
; S2 = 2
(define (m2last n) (m2last-aux n 1 2))
 
; Funcion auxiliar recursiva que regresa el n-esimo termino de la serie
; Sn1 es donde se guarda el termino n esimo
; Sn2 es el siguiente termino de la serie (Sn+1)
(define (m2last-aux n Sn1 Sn2)
  (cond
    [(equal? n 1) Sn1]
    [else (m2last-aux (- n 1) Sn2 (* Sn1 Sn2))]
  )
)
 
; RESPUESTA #6
; Funcion que convierte un numero en una lista de sus digitos
(define (num->list n) (num->list-aux n '()))
 
; Funcion auxiliar que obtiene los digitos de un numero n y los almacena en una lista
; n es el numero
; lista almacena los valores de los digitos que se han almacenado hasta el momento
(define (num->list-aux n lista)
  (if (zero? n)
      lista
      (num->list-aux (quotient n 10) (append (list (remainder n 10)) lista))
  )
)
 
; RESPUESTA #8
; Funcion que produce una lista sin repeticion que contiene los elementos de listas ordenadas de enteros
(define (mezcla lista1 lista2) (mezcla-aux lista1 lista2 '()))
 
; Funcion auxiliar que recibe las dos listas y acumula los elementos distintos de entre las dos en la lista final
(define (mezcla-aux lista1 lista2 final)
  (cond
    [(and (null? lista1) (null? lista2))
      final]
    [(null? lista1)
      (mezcla-aux lista2 lista1 final)]
    [(or (existe? (car lista1) (cdr lista1)) (existe? (car lista1) lista2)) ; el primer elemento de lista1 no es unico
      (mezcla-aux (cdr lista1) lista2 final)]
    [(or (null? lista2) (< (car lista1) (car lista2))) ; lista2 es nula o su primer elemento es mayor que el primero de lista1
      (mezcla-aux (cdr lista1) lista2 (append final (cons (car lista1) '())))]
    [else ; el primer elemento de lista1 es mayor, asi que no lo podemos insertar; evaluaremos el primer elemento de lista2
      (mezcla-aux lista2 lista1 final)]
  )
)