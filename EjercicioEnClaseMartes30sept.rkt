; testing procedure...
(define (suma a b) (+ a b))
(define nprocs
    (lambda(lista)
        (if (list? lista) 
            (nprocs-aux lista) 
            "ERROR"
        )
    )
)

(define nprocs-aux
    (lambda(lista)
        (cond
            [(null? lista) 0]
            [(not (list? lista)) 
             (if (and (procedure? lista) 
                      (not (primitive? lista)))
                 1 
                 0
             )]
            [else (+ (nprocs-aux (car lista)) (nprocs-aux (cdr lista)))]
        )
    )
)
;1. Programar la función recursiva nprocs en Scheme 
;que cuente el número de procedimientos NO primitivos 
;dentro de una lista posiblemente imbricada.
;(define nprocs 
;  (lambda (list)
;    (nprocs-aux list 0)
;  )
;)
;tail helper func for first problem
;(define nprocs-aux 
;  (lambda (l s)
;    (cond 
;      [(null? l) s]
;      [(list? (car l)) (nprocs-aux (cdr l) (+ s (nprocs (car l))))]
;      [(not-primitive? (car l))  
;       (nprocs-aux (cdr l) (+ s 1))]
;      [else (nprocs-aux (cdr l) s)]
;    )
;  )
;)
;primitive checker function
;(define not-primitive?
;  (lambda (elem)
;    (not (primitive? elem))
;  )
;)

;3. Programar la función recursiva mapea en Scheme que aplique 
;una función que reciba como argumento a cada uno de los
;elementos de una lista.
(define mapea 
  (lambda (func l)
    (mapea-aux-tail func l '())
  )
)
;aux mapea función, tail recursion
(define mapea-aux-tail
  (lambda(func oldL newL)
    (if (null? oldL) 
        newL
        (mapea-aux-tail func (cdr oldL) (append newL (list (func (car oldL)))) )
    )
  )
)

                 