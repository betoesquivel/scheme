; José Alberto Esquivel A01139626
; Eduardo Sánchez A01195815

; PROBLEMA 1
; Implementar la función recursiva tokeniza reciba una lista posiblemente imbricada y la convierta en una lista plana que 
; sustituya cada número por el símbolo N, cada símbolo por el símbolo S y cada sublista por una secuencia de símbolos 
; I <elementos> D, donde <elementos> es una secuencia de elementos que depende del contenido de la sublista. 
(define tokeniza
  (lambda (l)
    (cond [(null? l) '()]
          [(list? (car l)) (append (append (list 'I) (append (tokeniza (car l)) (list 'D)) ) (tokeniza (cdr l)))]
          [(number? (car l)) (append (list 'N) (tokeniza (cdr l)))]
          [else ( append (list 'S) (tokeniza (cdr l)) )]
    )
  )
)

; PROBLEMA 3
; Implementar la función recursiva sucesores que reciba una matriz de símbolos que representen un tablero del juego del 
; gato y la marca de un jugador, y regrese una lista de nuevos tableros que indiquen las jugadas válidas del jugador. 

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
; PROBLEMA 5
; Implementar la función recursiva estadistica que a partir de la base de datos de equipos de futbol regrese una 
; lista con los nombres de los equipos, su # de juegos jugados, la diferencia de goles y el # de puntos de cada uno, 
; ordenada por # de puntos y en caso de empate en puntos, por diferencia de goles. Recordar que el número de puntos 
; se calcula como 3 puntos por cada juego ganado y 1 por cada juego empatado. 
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
  (lambda (e)
    (if (null? e) 
        '()
        (list (car e) (jugados e) (diferencia e) (puntos e)))
  )
)
; función que recibe dos equipos y regresa el equipo mayor. 
(define mayorDe2
  (lambda (e1 e2)
    (cond [(null? e2) e1]
          [(null? e1) e2]
          [else 
           (cond 
             [(equal? (puntos e1) (puntos e2))
                (cond [(equal? (diferencia e1) (diferencia e2)) e1]
                      [(> (diferencia e1) (diferencia e2)) e1]
                      [else e2]
                )]
             [(> (puntos e1) (puntos e2)) e1]
             [else e2]
           )]
    )
  )
)
; función que regresa el mayor de una lista de equipos
(define mayor
  (lambda (lista)
    (cond [(null? lista) '()]
          [else
           (mayorDe2 (car lista) (mayor (cdr lista)))]
    )
  )
)
; función que calcula la estadística de toda la liga y la regresa ordenada de 
; mayores puntos a menores puntos. 
(define estadistica
  (lambda (liga)
    (cond [(null? liga) '()] 

; PROBLEMA 7
; Implementar la función recursiva acumulado que a partir de un árbol regrese el mismo árbol, pero donde el valor
; de cada nodo represente la suma de todos los nodos del subárbol del cual ese nodo es la raíz. 
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