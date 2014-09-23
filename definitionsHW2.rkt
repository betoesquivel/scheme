;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname definitionsHW2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; Cuenta los dígitos de un número entero no negativo.
(define (digitos n) 
   (if (zero? n)
      0
      (+ 1 (digitos (- (/ n 10) (/ (remainder n 10) 10) ) ))
   )
)

; Cuenta los dígitos de un número entero no negativo pero está en 
; variación terminal.
(define (tdigitos n) 
  (tdigitosAux n 0)
)

; Función auxiliar que realiza la contada de números de forma terminal.
(define (tdigitosAux n r)
  (if (zero? n)
      r
      (tdigitosAux (- (/ n 10) (/ (remainder n 10) 10) ) (+ 1 r) )
  )
)