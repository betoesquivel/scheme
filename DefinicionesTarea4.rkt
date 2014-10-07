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
(define sucesores
  (lambda (mat)
    (cond [(null? mat) '()]
          [(