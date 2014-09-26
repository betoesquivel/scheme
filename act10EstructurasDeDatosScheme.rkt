
; Programar la función recursiva add-ren en Scheme que agregue un renglón a una matriz. 
; Asumir que se le pasa un renglón adecuado.
(define (add-ren mat ren)
  (if (null? mat)
      (cons ren '())
      (append mat (cons ren '())))
)

; Programar la función recursiva add-col en Scheme que agregue una columna a una matriz. 
; Asumir que se le pasa una columna adecuada.
(define (add-col mat col)
  (cond [(null? col) '()]
        [(not (list? col)) (list (append mat (cons col '()) '()))]
        [(null? mat)
         (append (add-col mat (car col)) (add-col mat (cdr col)))]
        [else
         (append (add-col (car mat) (car col)) (add-col (cdr mat) (cdr col)))]
        )
)

; Programar la función recursiva hojas en Scheme que obtenga una lista 
; con los valores de los nodos hoja de un árbol binario.
(define (hojas arbol)
  (if (not (null? arbol))
      (hojas-aux arbol '())
      '()))

; Helper function to get leaf nodes...
(define (hojas-aux arbol)
  (cond 
    [(and (null? (cadr arbol)) (null? (caddr arbol)) (not (null? arbol)))
     (cons (car arbol) '())]
    [else
     (append (hojas (cadr arbol)) (hojas (caddr arbol))) ]
  )
)
      