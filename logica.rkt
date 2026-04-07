#lang racket
(provide tablero-inicial
         crear-tablero
         imprimir-tablero
         posiciones-vacias
         insertar-en-posicion
         mover-izquierda
         mover-derecha
         mover-arriba
         mover-abajo
         jugada-izquierda
         jugada-derecha
         jugada-arriba
         jugada-abajo
         tableros-iguales?
         insertar-ficha-random
         hay-2048?)

; ============================================================
; CREACIÓN DEL TABLERO
; ============================================================

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

; ============================================================
; IMPRESIÓN
; ============================================================

; imprime el tablero fila por fila
(define (imprimir-tablero tablero)
  (cond
    [(null? tablero) (newline)]
    [else
     (displayln (car tablero))
     (imprimir-tablero (cdr tablero))]))

; ============================================================
; MODIFICAR POSICIONES
; ============================================================

; reemplaza un valor dentro de una lista
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

; ============================================================
; POSICIONES VACÍAS
; ============================================================

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

; ============================================================
; TABLERO INICIAL
; ============================================================

; inserta un 2 en una posición vacía aleatoria
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

; crea el tablero y le inserta dos 2 iniciales
(define (tablero-inicial filas columnas)
  (insertar-un-2
   (insertar-un-2
    (crear-tablero filas columnas))))

; ============================================================
; AUXILIARES DE MOVIMIENTO
; ============================================================

; cuenta cuántos elementos tiene una lista
(define (largo lista)
  (if (null? lista)
      0
      (+ 1 (largo (cdr lista)))))

; quita los ceros de una fila
; '(2 0 2 0) -> '(2 2)
(define (quitar-ceros fila)
  (cond
    [(null? fila) '()]
    [(= (car fila) 0)
     (quitar-ceros (cdr fila))]
    [else
     (cons (car fila)
           (quitar-ceros (cdr fila)))]))

; combina iguales contiguos solo una vez por jugada
; '(2 2 4)   -> '(4 4)
; '(2 2 2 2) -> '(4 4)
(define (combinar-izquierda fila)
  (cond
    [(null? fila) '()]
    [(null? (cdr fila)) fila]
    [(= (car fila) (cadr fila))
     (cons (+ (car fila) (cadr fila))
           (combinar-izquierda (cddr fila)))]
    [else
     (cons (car fila)
           (combinar-izquierda (cdr fila)))]))

; agrega ceros al final hasta completar el tamaño original
(define (rellenar-con-ceros fila cantidad)
  (if (= cantidad 0)
      fila
      (rellenar-con-ceros (append fila '(0)) (- cantidad 1))))

; mueve una fila a la izquierda
; 1. quita ceros
; 2. combina
; 3. rellena con ceros
(define (mover-fila-izquierda fila)
  ((lambda (sin-ceros)
     ((lambda (combinada)
        (rellenar-con-ceros combinada
                            (- (largo fila) (largo combinada))))
      (combinar-izquierda sin-ceros)))
   (quitar-ceros fila)))

; invierte una lista
(define (invertir lista)
  (if (null? lista)
      '()
      (append (invertir (cdr lista))
              (list (car lista)))))

; ============================================================
; MOVIMIENTOS HORIZONTALES
; ============================================================

; mueve todo el tablero a la izquierda
(define (mover-izquierda tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (mover-fila-izquierda (car tablero))
           (mover-izquierda (cdr tablero)))]))

; mueve una fila a la derecha
(define (mover-fila-derecha fila)
  (invertir
   (mover-fila-izquierda
    (invertir fila))))

; mueve todo el tablero a la derecha
(define (mover-derecha tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (mover-fila-derecha (car tablero))
           (mover-derecha (cdr tablero)))]))

; ============================================================
; TRANSPOSICIÓN
; ============================================================

; toma el primer elemento de cada fila
(define (primer-columna tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (caar tablero)
           (primer-columna (cdr tablero)))]))

; toma el resto de cada fila
(define (resto-columnas tablero)
  (cond
    [(null? tablero) '()]
    [else
     (cons (cdar tablero)
           (resto-columnas (cdr tablero)))]))

; revisa si alguna fila ya quedó vacía
(define (hay-fila-vacia? tablero)
  (cond
    [(null? tablero) #f]
    [(null? (car tablero)) #t]
    [else
     (hay-fila-vacia? (cdr tablero))]))

; transpone filas por columnas
(define (transponer tablero)
  (cond
    [(null? tablero) '()]
    [(hay-fila-vacia? tablero) '()]
    [else
     (cons (primer-columna tablero)
           (transponer (resto-columnas tablero)))]))

; ============================================================
; MOVIMIENTOS VERTICALES
; ============================================================

; arriba = transponer -> izquierda -> transponer
(define (mover-arriba tablero)
  (transponer
   (mover-izquierda
    (transponer tablero))))

; abajo = transponer -> derecha -> transponer
(define (mover-abajo tablero)
  (transponer
   (mover-derecha
    (transponer tablero))))

; ============================================================
; COMPARAR TABLEROS
; ============================================================

(define (filas-iguales? fila1 fila2)
  (cond
    [(and (null? fila1) (null? fila2)) #t]
    [(or (null? fila1) (null? fila2)) #f]
    [(= (car fila1) (car fila2))
     (filas-iguales? (cdr fila1) (cdr fila2))]
    [else #f]))

(define (tableros-iguales? tablero1 tablero2)
  (cond
    [(and (null? tablero1) (null? tablero2)) #t]
    [(or (null? tablero1) (null? tablero2)) #f]
    [(filas-iguales? (car tablero1) (car tablero2))
     (tableros-iguales? (cdr tablero1) (cdr tablero2))]
    [else #f]))

; ============================================================
; FICHA NUEVA DESPUÉS DEL MOVIMIENTO
; ============================================================

; inserta UNA sola ficha nueva en un espacio vacío:
; 90% probabilidad de 2
; 10% probabilidad de 4
(define (insertar-ficha-random tablero)
  ((lambda (vacias)
     ((lambda (pos)
        (if (not pos)
            tablero
            (insertar-en-posicion tablero
                                  (car pos)
                                  (cadr pos)
                                  (if (< (random 10) 9) 2 4))))
      (if (null? vacias)
          #f
          (list-ref vacias (random (length vacias))))))
   (posiciones-vacias tablero)))


; ============================================================
; DETECTAR VICTORIA (2048)
; ============================================================

; revisa si una fila contiene 2048
(define (fila-tiene-2048? fila)
  (cond
    [(null? fila) #f]
    [(= (car fila) 2048) #t]
    [else
     (fila-tiene-2048? (cdr fila))]))

; revisa si el tablero contiene 2048
(define (hay-2048? tablero)
  (cond
    [(null? tablero) #f]
    [(fila-tiene-2048? (car tablero)) #t]
    [else
     (hay-2048? (cdr tablero))]))

; ============================================================
; JUGADAS COMPLETAS
; ============================================================

; función auxiliar general:
; 1. mueve
; 2. si el tablero no cambió, devuelve el original
; 3. si cambió, inserta UNA ficha nueva
(define (aplicar-jugada tablero funcion-movimiento)
  ((lambda (nuevo-tablero)
     (if (tableros-iguales? tablero nuevo-tablero)
         tablero
         (insertar-ficha-random nuevo-tablero)))
   (funcion-movimiento tablero)))

(define (jugada-izquierda tablero)
  (aplicar-jugada tablero mover-izquierda))

(define (jugada-derecha tablero)
  (aplicar-jugada tablero mover-derecha))

(define (jugada-arriba tablero)
  (aplicar-jugada tablero mover-arriba))

(define (jugada-abajo tablero)
  (aplicar-jugada tablero mover-abajo))