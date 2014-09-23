; Calcula la profundidad de una lista
(
 define (profundidad lista)
  (
   cond ((null? lista) 
         0); caso base
        ((atom? (car lista)) 
         (profundidad (cdr lista)))
        (else 
         (max (+ 1 (profundidad (car lista)))
                   (profundidad (cdr lista))))
  )
)

; Checks if a is an atom or not
(
 define (atom? a)
  (not (pair? a))
)

; Constructs simetrical pattern with N nesting levels. 
(define (simetrico n) (simetrico-aux n '()))

; Helper function for the simetrico function.
(define (simetrico-aux n lista)
        (if (zero? n)
                lista
        (simetrico-aux (- n 1) (list '< lista '> ))
))     

; Deletes one element x from list lista.
(define (elimina x lista)
        (cond
                ((null? lista) lista)
                ((list? (car lista)) (cons (elimina x (car lista)) (elimina x (cdr lista))))
                ((equal? (car lista) x) (elimina x (cdr lista)))
                (else (cons (car lista) (elimina x (cdr lista))))
        )
)