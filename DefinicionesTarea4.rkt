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
  (lambda (mat)
    (sucesores-fila mat '() '())
  )
)
; función para recorrer matriz línea por línea sin perder la información de la matriz. 
(define sucesores-fila
  (lambda (matRecorriendo matRecorrida result)
    (cond
      [(null? matRecorriendo) result]
      [else (sucesores-fila (cdr matRecorriendo) (append matRecorrida (list (car matRecorriendo)))
                            (append result 
                                    (sucesores-columna matRecorrida (car matRecorriendo) '() (cdr matRecorriendo) '()) ) 
            ) 
      ]
    )
  )
)
; función para recorrer matriz columna por columna sin perder la información de la matriz.
(define sucesores-columna
  (lambda (filasAnteriores filaRecorriendo filaRecorrida filasPosteriores result)
    (cond
      [(null? filaRecorriendo) result]
      [(equal? (car filaRecorriendo) 'v)
       (sucesores-columna filasAnteriores (cdr filaRecorriendo) (append filaRecorrida (list (car filaRecorriendo))) filasPosteriores
                               
                         (append result      
                         (list (append filasAnteriores
                               (append (list (append filaRecorrida filaRecorriendo)) 
                                        filasPosteriores)) 
                         ) 
                         )
       )
      ]
      [else (sucesores-columna filasAnteriores (cdr filaRecorriendo) (append filaRecorrida (list (car filaRecorriendo))) filasPosteriores
                               
                         result
            )
      ]
    )
  )
)