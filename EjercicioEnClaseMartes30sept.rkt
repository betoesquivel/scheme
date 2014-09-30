; testing procedure...
(define (suma a b) (+ a b))

;1. Programar la función recursiva nprocs en Scheme 
;que cuente el número de procedimientos NO primitivos 
;dentro de una lista posiblemente imbricada.
(define nprocs 
  (lambda (list)
    (nprocs-aux list 0)
  )
)
;tail helper func for first problem
(define nprocs-aux 
  (lambda (l s)
    (cond 
      [(null? l) s]
      [(primitive? (car l)) 
       (nprocs-aux (cdr l) s)]
      [else (nprocs-aux (cdr l) (+ s 1))]
    )
  )
)


                 