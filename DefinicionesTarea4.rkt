; José Alberto Esquivel A01139626
; Eduardo Sánchez A01195815

; PROBLEMA 1
(define tokeniza
  (lambda (l)
    (cond [(null? l) '()]
          [(list? (car l)) (append (append (list 'I) (append (tokeniza (car l)) (list 'D)) ) (tokeniza (cdr l)))]
          [(number? (car l)) (append (list 'N) (tokeniza (cdr l)))]
          [else ( append (list 'S) (tokeniza (cdr l)) )]
    )
  )
)

; PROBLEMA 2
(define count
  (lambda (list total)
    (if (null? list)
        total
        (count (cdr list) (+ total 1)))))
 
(define subelementos-aux
  (lambda (matr cont res)
    (cond
      [(null? matr) (cons cont res)]
      [(list? (car matr)) (subelementos-aux (cdr matr) (+ 1 cont) (append res (cons (subelementos-aux (car matr) 0 '()) '())))]
      [else (subelementos-aux (cdr matr) (+ 1 cont) res)]
    )
  )
)
 
(define subelementos
  (lambda (matr)
    (subelementos-aux matr 0 '())))
 
; PROBLEMA 3
; Matrices para el GATO
(define gato1 '((X v X)(v O v)(X v O)))

; estrategia, cada símbolo me regresa una lista de matrices
; tengo dos matrices, una se va vaciando mientras la otra se va llenando, así no pierdo nunca la matriz...
; creo mi matriz nueva, uniendo los renglones de las dos matrices con append
(define sucesores
  (lambda (mat player)
    (sucesores-fila mat '() '() player)
  )
)
; función para recorrer matriz línea por línea sin perder la información de la matriz. 
(define sucesores-fila
  (lambda (matRecorriendo matRecorrida result player)
    (cond
      [(null? matRecorriendo) result]
      [else (sucesores-fila (cdr matRecorriendo) (append matRecorrida (list (car matRecorriendo)))
                            (append result 
                                    (sucesores-columna matRecorrida (car matRecorriendo) '() (cdr matRecorriendo) '() player) )
                            player
            ) 
      ]
    )
  )
)
; función para recorrer matriz columna por columna sin perder la información de la matriz.
(define sucesores-columna
  (lambda (filasAnteriores filaRecorriendo filaRecorrida filasPosteriores result player)
    (cond
      [(null? filaRecorriendo) result]
      [(equal? (car filaRecorriendo) 'v)
       (sucesores-columna filasAnteriores (cdr filaRecorriendo) (append filaRecorrida (list (car filaRecorriendo))) filasPosteriores
                               
                         (append result      
                         (list (append filasAnteriores
                               (append (list (append (append filaRecorrida (list player))
                                             (cdr filaRecorriendo))) 
                                        filasPosteriores)) 
                         ) 
                         )
                         player
       )
      ]
      [else (sucesores-columna filasAnteriores (cdr filaRecorriendo) (append filaRecorrida (list (car filaRecorriendo))) filasPosteriores
                               
                         result player
            )
      ]
    )
  )
)

; PROBLEMA 4
(define renglon?
  (lambda (renglon jugador)
    (if (null? renglon) #t
        (and (equal? (car renglon) jugador) (renglon? (cdr renglon) jugador))
    )
  )
)
 
(define renglones?
  (lambda (tablero jugador)
    (if (null? tablero) #f
        (or (renglon? (car tablero) jugador) (renglones? (cdr tablero) jugador))
    )
  )
)
 
(define columna?
  (lambda (tablero jugador)
    (if (null? tablero) #t
        (and (equal? (caar tablero) jugador) (columna? (cdr tablero) jugador))
    )
  )
)
 
(define sig-columna
  (lambda (tablero)
    (if (or (null? tablero) (null? (cdar tablero))) '()
        (cons (cdar tablero) (sig-columna (cdr tablero)))
    )
  )
)
 
(define columnas?
  (lambda (tablero jugador)
    (if (null? tablero) #f
        (or (columna? tablero jugador) (columnas? (sig-columna tablero) jugador))
    )
  )
)
 
(define jugador?
  (lambda (renglon columna jugador)
    (if (equal? columna 0) (equal? (car renglon) jugador)
        (jugador? (cdr renglon) (- columna 1) jugador)
    )
  )
)
 
(define diagonal?
  (lambda (tablero jugador columna incr)
    (if (null? tablero) #t
        (and (jugador? (car tablero) columna jugador) (diagonal? (cdr tablero) jugador (incr columna 1) incr))
    )
  )
)
 
(define ganador?
  (lambda (tablero jugador)
    (cond
      [(renglones? tablero jugador) #t]
      [(columnas? tablero jugador) #t]
      [(diagonal? tablero jugador 0 +) #t]
      [(diagonal? tablero jugador 2 -) #t]
      [else #f]
    )
  )
)


; PROBLEMA 5
; (eq jg je jp gf gc)
; eq = car
; jg = cadr, je = caddr, jp = cadddr
; gf = caddddr, gc = cadddddr
(define ligaMX '(( America 6 2 1 15 6 )( Atlas 5 3 1 13 8 ) 
                 ( CruzAzul 2 3 4 7 10 )( Guadalajara 2 3 3 6 9 ) 
                 ( Jaguares 3 4 2 13 12 )( Leon 3 0 6 15 16 ) 
                 ( LeonesNegros 1 3 5 3 10 )( Monterrey 5 1 2 10 5 )
                 ( Morelia 0 3 6 8 21 )( Pachuca 4 1 4 11 11 ) 
                 ( Puebla 2 4 3 7 11 )( Queretaro 4 2 3 13 10 ) 
                 ( SantosLaguna 4 3 2 12 9 )( Tijuana 2 5 2 11 9 ) 
                 ( Toluca 5 2 2 11 8 )( UANL 3 4 2 14 11 ) 
                 ( UNAM 3 2 4 12 12 )( Veracruz 1 5 3 5 8 )))
; función que calcula el número de juegos jugados
(define jugados
  (lambda (e)
    (+ (cadr e) (caddr e) (cadddr e))
  )
)
; función que calcula los puntos de un equipo de futbol. 
(define puntos
  (lambda (e)
       (+ (* 3 (cadr e)) (caddr e))
  )
)
; función que obtiene los goles a favor
(define gf
  (lambda (e)
    (car (cddddr e))
  )
)
; función que obtiene los goles en contra
(define gc
  (lambda (e)
    (cadr (cddddr e))
  )
)
; función que obtiene la diferencia de goles de un equipo
(define diferencia
  (lambda (e)
    (- (gf e) (gc e))
  )
)
; función que calcula la información estadística de un equipo
(define info
  (lambda (e lugar)
    (if (null? e) 
        '()
        (list lugar (car e) (jugados e) (diferencia e) (puntos e)))
  )
)
; función que recibe dos equipos y regresa el equipo mayor. 
(define mayorQue?
  (lambda (e1 e2)
    (cond [(null? e2) #t]
          [(null? e1) #f]
          [else 
           (cond 
             [(equal? (puntos e1) (puntos e2))
                (cond [(equal? (diferencia e1) (diferencia e2)) #t]
                      [(> (diferencia e1) (diferencia e2)) #t]
                      [else #f]
                )]
             [(> (puntos e1) (puntos e2)) #t]
             [else #f]
           )]
    )
  )
)

; función auxiliar que ordena lista, usando insertion sort...
(define (ordenar-aux e liga)
    (cond
        [(null? liga) (cons e liga)] 
        [(mayorQue? e (car liga)) (cons e liga)] 
        (else 
            (cons (car liga) (ordenar-aux e (cdr liga))) )))
; función que ordena lista
(define (ordenar liga)
    (cond
        [(null? liga) liga] 
        (else
            (ordenar-aux (car liga) (ordenar (cdr liga))) )))

; función auxiliar para el ordenamiento de la liga que recibe la liga ordenada
(define estadistica-aux
  (lambda (liga lugar)
    (if (null? liga) 
        '()
        (cons (info (car liga) lugar) (estadistica-aux (cdr liga) (+ 1 lugar)))
    )
  )
)
; función que calcula la estadística de toda la liga y la regresa ordenada de 
; mayores puntos a menores puntos. 
(define estadistica
  (lambda (liga)
    (estadistica-aux (ordenar liga) 1)
  )
)

; PROBLEMA 6
(define AB '(8 (5 (2 () ())
                  (7 () ()))
               (9 ()
                  (15 (11 () ())
                      () ))))
 
(define nivel-aux
  (lambda (arbol cont)
    (if (null? arbol) '()
        (list cont (nivel-aux (cadr arbol) (+ cont 1)) (nivel-aux (caddr arbol) (+ cont 1)))
    )
  )
)
 
(define nivel
  (lambda (arbol)
    (nivel-aux arbol 1)))
 

; PROBLEMA 7
(define AB '(8 (5 (2 () ()) 
                  (7 () ())) 
               (9 () 
                  (15 
                   (11 () ()) 
                   () ))))
(define acumulado
  (lambda (tree)
    (cond [(null? tree) '()]
          [else 
           (append (list (acumulado-val tree))
                   (append (list (acumulado (cadr tree)))
                           (list (acumulado (caddr tree))))
           )
          ]
    )
  )
)
; Calcula el valor acumulado de las dos ramas hijos del nodo raiz del arbol que recibe. 
(define acumulado-val
  (lambda (tree)
    (cond [(null? tree) 0]
          [else (+ (car tree)
                   (acumulado-val (cadr tree))
                   (acumulado-val (caddr tree)))
          ]
    )
  )
)

; PROBLEMA 8
(define g
'((A 0 2 0 10 0)
  (B 0 0 9 0 5)
  (C 12 0 0 6 0)
  (D 0 0 0 0 7)
  (E 0 0 3 0 0)))
 
(define costo-ruta
  (lambda (grafo ruta)
    (recorrer grafo ruta 0)))
 
(define recorrer
  (lambda (grafo ruta costo)
    (cond
      [(null? (cdr ruta)) costo]
      [(camino? grafo (car ruta) (cadr ruta)) (recorrer grafo (cdr ruta) (+ costo (camino grafo (car ruta) (cadr ruta))))]
      [else 'no-ruta]
    )
  )
)
 
; Traduce el simbolo nombre de un nodo en su posicion dentro del grafo a partir de 0
(define posicion
  (lambda (grafo s)
    (posicion-aux grafo s 0)))
 
; Busca el nodo dentro del grafo, si no lo encuentra regresa -1
(define posicion-aux
  (lambda (grafo s cont)
    (cond
      [(null? grafo) -1]
      [(equal? (caar grafo) s) cont]
      [else (posicion-aux (cdr grafo) s (+ cont 1))]
    )
  )
)
 
; Determina si existe un camino desde el simbolo de origen al de destino en un grafo
(define camino?
  (lambda (grafo origen destino)
    (and (> (posicion grafo origen) -1) (> (posicion grafo destino) -1)
         (> (camino-aux grafo (posicion grafo origen) (posicion grafo destino)) 0))))
 
; Regresa el valor dentro del grafo para el nodo en la posicion dada por renglon, hacia el nodo en la posicion dada por columna
(define camino-aux
  (lambda (grafo renglon columna)
    (if (equal? renglon 0) (busca (cdar grafo) columna)
        (camino-aux (cdr grafo) (- renglon 1) columna)
    )
  )
)
 
; Obtiene el valor de la lista dado por la posicion en pos
(define busca
  (lambda (lista pos)
    (if (equal? pos 0) (car lista)
        (busca (cdr lista) (- pos 1))
    )
  )
)
 
; Obtiene el costo de recorrer el grafo desde el nodo origen hasta el nodo destino
(define camino
  (lambda (grafo origen destino)
    (camino-aux grafo (posicion grafo origen) (posicion grafo destino))))

; PROBLEMA 9
