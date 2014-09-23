; Contar átomos en una lista
(
 define (cuenta-atomos lista)
  (cond ((null? lista) 0)
  ((not (list? (car lista)))
   (+ 1 (cuenta-atomos(cdr lista))))
  (else
   (+ (cuenta-atomos (car lista))
      (cuenta-atomos (cdr lista)))))
)

; Encontrar elemento mayor de una lista imbricada de números
; Caso base: Lista vacía => ()
; Caso general: Lista con elementos '(())
; Usa función max para sacar el mayor de una lista de solamente números
(
 define (mayor lista)
  (cond ((null? lista)'()); caso base
        ((number? (car lista))
         (max2 (car lista)
               (mayor (cdr lista)))); me regresará un num o una lista vacía
        (else (max2 (mayor (car lista))
                    (mayor (cdr lista))))
  )
)  

; Obtiene el mayor de entre dos listas o números
(
 define (max2 a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else (max a b)))
)


; Generar una lista donde su único elemento n se enecuentre anidado "n" veces.
(
 define (anida lista n)
  (anida-aux lista n 0)
)

; Función auxiliar que anida la lista n veces
(
 define (anida-aux lista n veces-anidadas)
  (cond ((= n veces-anidadas) lista)
  (else 
   (anida-aux (cons lista '()) n (+ 1 veces-anidadas))))
)

; Crea una lista imbricada de N a 0
(
 define (profundiza n)
  (if (zero? n)
      '(0)
  (list n (profundiza (- n 1))))
)

; Aumentar todos los elementos de una lista
(
 define (incrementa lista n)
  (cond ((null? (car lista)) '())
        ((number? (car lista))
         (cons (+ n (car lista)) (incrementa (cdr lista) n)))
        ((list? (car lista)) 
         (cons (incrementa (car lista) n) (incrementa (cdr lista) n)))
  )
)
  