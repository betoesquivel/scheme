(define promedio
  (lambda (mat)
    (/ (apply + (apply append mat)) (* 1.0 (length (apply append mat))) )
  )
)

