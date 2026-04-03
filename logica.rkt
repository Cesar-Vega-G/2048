#lang racket

(provide tablero-inicial
         crear-tablero
         imprimir-tablero
         posiciones-vacias
         insertar-en-posicion)

; crea una fila de ceros
(define (crear-fila columnas)
  (if (= columnas 0)
      '()
      (cons 0 (crear-fila (- columnas 1)))))

; crea el tablero vacío
(define (crear-tablero filas columnas)
  (if (= filas 0)
      '()
      (cons (crear-fila columnas)
            (crear-tablero (- filas 1) columnas))))

; imprime el tablero fila por fila
(define (imprimir-tablero tablero)
  (cond
    [(null? tablero) (newline)]
    [else
     (displayln (car tablero))
     (imprimir-tablero (cdr tablero))]))

; reemplaza un valor dentro de una fila
(define (reemplazar-en-lista lista indice valor)
  (cond
    [(null? lista) '()]
    [(= indice 0) (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (reemplazar-en-lista (cdr lista) (- indice 1) valor))]))

; inserta un valor en una posición del tablero
(define (insertar-en-posicion tablero fila columna valor)
  (cond
    [(null? tablero) '()]
    [(= fila 0)
     (cons (reemplazar-en-lista (car tablero) columna valor)
           (cdr tablero))]
    [else
     (cons (car tablero)
           (insertar-en-posicion (cdr tablero) (- fila 1) columna valor))]))

; obtiene las posiciones vacías del tablero
(define (posiciones-vacias tablero)
  (define (recorrer-filas tablero fila-actual)
    (cond
      [(null? tablero) '()]
      [else
       (append (recorrer-columnas (car tablero) fila-actual 0)
               (recorrer-filas (cdr tablero) (+ fila-actual 1)))]))

  (define (recorrer-columnas fila fila-actual columna-actual)
    (cond
      [(null? fila) '()]
      [(= (car fila) 0)
       (cons (list fila-actual columna-actual)
             (recorrer-columnas (cdr fila) fila-actual (+ columna-actual 1)))]
      [else
       (recorrer-columnas (cdr fila) fila-actual (+ columna-actual 1))]))

  (recorrer-filas tablero 0))

; escoge una posición vacía aleatoria e inserta un 2
(define (insertar-un-2 tablero)
  ((lambda (vacias)
     ((lambda (pos)
        (if (not pos)
            tablero
            (insertar-en-posicion tablero
                                  (car pos)
                                  (cadr pos)
                                  2)))
      (if (null? vacias)
          #f
          (list-ref vacias (random (length vacias))))))
   (posiciones-vacias tablero)))

; crea el tablero y le inserta los dos 2 iniciales
(define (tablero-inicial filas columnas)
  (insertar-un-2
   (insertar-un-2
    (crear-tablero filas columnas))))