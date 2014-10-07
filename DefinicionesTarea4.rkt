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