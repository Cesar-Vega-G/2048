#lang racket

; Se exporta estas funciones para poder usarlas desde otros archivos,

(provide crear-tablero
         imprimir-tablero
         posiciones-vacias
         insertar-en-posicion
         insertar-dos-iniciales
         mover-fila-izquierda
         mover-tablero-izquierda
         mover-fila-derecha
         mover-tablero-derecha
         obtener-columna
         reemplazar-columna
         mover-tablero-arriba
         mover-tablero-abajo)

; ------------------------------------------------------------
; FUNCIONES PARA CREAR EL TABLERO
; ------------------------------------------------------------

; crear-fila:
; recibe un número n y construye una lista con n ceros.

(define (crear-fila n)
  (if (= n 0)
      '()
      (cons 0 (crear-fila (- n 1)))))

; crear-tablero:
; recibe cantidad de filas y columnas.
; Construye una lista de listas llena de ceros.

(define (crear-tablero filas columnas)
  (if (= filas 0)
      '()
      (cons (crear-fila columnas)
            (crear-tablero (- filas 1) columnas))))

; ------------------------------------------------------------
; FUNCIÓN PARA IMPRIMIR EL TABLERO
; ------------------------------------------------------------

; imprimir-tablero:
; recibe el tablero y lo imprime fila por fila.
; Esto sirve para probar la lógica en consola mientras todavía
; no se ha construido la interfaz gráfica.
(define (imprimir-tablero tablero)
  (cond
    [(null? tablero) (newline)]
    [else
     (displayln (car tablero))
     (imprimir-tablero (cdr tablero))]))

; ------------------------------------------------------------
; FUNCIONES AUXILIARES PARA MANIPULAR LISTAS
; ------------------------------------------------------------

; elemento-en:
; devuelve el elemento que está en una posición específica
; dentro de una lista.
; Si la lista se acaba, devuelve #f.

(define (elemento-en lista indice)
  (cond
    [(null? lista) #f]
    [(= indice 0) (car lista)]
    [else (elemento-en (cdr lista) (- indice 1))]))

; reemplazar-en-lista:
; reemplaza el valor que está en una posición de una lista

(define (reemplazar-en-lista lista indice valor)
  (cond
    [(null? lista) '()]
    [(= indice 0) (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (reemplazar-en-lista (cdr lista) (- indice 1) valor))]))
; ------------------------------------------------------------
; FUNCIONES PARA MOVER A LA IZQUIERDA
; ------------------------------------------------------------

; cantidad-elementos:
; cuenta cuántos elementos tiene una lista.
(define (cantidad-elementos lista)
  (if (null? lista)
      0
      (+ 1 (cantidad-elementos (cdr lista)))))

; concatenar:
; une dos listas sin usar funciones prohibidas.
(define (concatenar lista1 lista2)
  (if (null? lista1)
      lista2
      (cons (car lista1)
            (concatenar (cdr lista1) lista2))))

; quitar-ceros-fila:
; elimina todos los 0 de una fila.

(define (quitar-ceros-fila fila)
  (cond
    [(null? fila) '()]
    [(= (car fila) 0)
     (quitar-ceros-fila (cdr fila))]
    [else
     (cons (car fila)
           (quitar-ceros-fila (cdr fila)))]))

; combinar-fila-izquierda:
; asume que la fila ya no tiene ceros.
; combina solo una vez por jugada.

(define (combinar-fila-izquierda fila)
  (cond
    [(null? fila) '()]
    [(null? (cdr fila)) (list (car fila))]
    [(= (car fila) (cadr fila))
     (cons (+ (car fila) (cadr fila))
           (combinar-fila-izquierda (cddr fila)))]
    [else
     (cons (car fila)
           (combinar-fila-izquierda (cdr fila)))]))

; rellenar-con-ceros:
; recibe una fila ya movida/combinada y le agrega ceros
; al final hasta recuperar el tamaño original.

(define (rellenar-con-ceros fila tam-original)
  (concatenar fila
              (crear-fila (- tam-original
                             (cantidad-elementos fila)))))

; mover-fila-izquierda:
; aplica todo el proceso completo a una fila:
; 1) quita ceros
; 2) combina iguales
; 3) rellena con ceros al final

(define (mover-fila-izquierda fila)
  (rellenar-con-ceros
   (combinar-fila-izquierda
    (quitar-ceros-fila fila))
   (cantidad-elementos fila)))

; mover-tablero-izquierda:
; aplica mover-fila-izquierda a cada fila del tablero.
(define (mover-tablero-izquierda tablero)
  (if (null? tablero)
      '()
      (cons (mover-fila-izquierda (car tablero))
            (mover-tablero-izquierda (cdr tablero)))))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER A LA DERECHA
; ------------------------------------------------------------

; invertir-lista:
; devuelve una lista en orden inverso.

(define (invertir-lista lista)
  (invertir-lista-aux lista '()))

; invertir-lista-aux:
; acumulador para invertir la lista.
(define (invertir-lista-aux lista acumulado)
  (if (null? lista)
      acumulado
      (invertir-lista-aux (cdr lista)
                          (cons (car lista) acumulado))))

; mover-fila-derecha:
; invierte la fila, aplica mover-fila-izquierda
; y luego vuelve a invertir.

(define (mover-fila-derecha fila)
  (invertir-lista
   (mover-fila-izquierda
    (invertir-lista fila))))

; mover-tablero-derecha:
; aplica mover-fila-derecha a cada fila del tablero.
(define (mover-tablero-derecha tablero)
  (if (null? tablero)
      '()
      (cons (mover-fila-derecha (car tablero))
            (mover-tablero-derecha (cdr tablero)))))

; ------------------------------------------------------------
; FUNCIONES PARA MANIPULAR COLUMNAS
; ------------------------------------------------------------

; obtener-columna:
; recibe un tablero y un índice de columna.
; devuelve una lista con los elementos de esa columna.

(define (obtener-columna tablero indice-columna)
  (if (null? tablero)
      '()
      (cons (elemento-en (car tablero) indice-columna)
            (obtener-columna (cdr tablero) indice-columna))))

; reemplazar-columna:
; recibe un tablero, un índice de columna y una nueva columna.
; devuelve un nuevo tablero con esa columna sustituida.

(define (reemplazar-columna tablero indice-columna nueva-columna)
  (cond
    [(null? tablero) '()]
    [(null? nueva-columna) '()]
    [else
     (cons (reemplazar-en-lista (car tablero)
                                indice-columna
                                (car nueva-columna))
           (reemplazar-columna (cdr tablero)
                               indice-columna
                               (cdr nueva-columna)))]))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER ARRIBA
; ------------------------------------------------------------

; mover-columnas-arriba:
; recorre todas las columnas del tablero y aplica
; mover-fila-izquierda a cada columna.
(define (mover-columnas-arriba tablero indice-columna total-columnas)
  (if (= indice-columna total-columnas)
      tablero
      (mover-columnas-arriba
       (reemplazar-columna tablero
                           indice-columna
                           (mover-fila-izquierda
                            (obtener-columna tablero indice-columna)))
       (+ indice-columna 1)
       total-columnas)))

; mover-tablero-arriba:
; mueve todas las columnas hacia arriba.
(define (mover-tablero-arriba tablero)
  (if (null? tablero)
      '()
      (mover-columnas-arriba tablero
                             0
                             (cantidad-elementos (car tablero)))))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER ABAJO
; ------------------------------------------------------------

; mover-columnas-abajo:
; recorre todas las columnas del tablero y aplica
; mover-fila-derecha a cada columna.
(define (mover-columnas-abajo tablero indice-columna total-columnas)
  (if (= indice-columna total-columnas)
      tablero
      (mover-columnas-abajo
       (reemplazar-columna tablero
                           indice-columna
                           (mover-fila-derecha
                            (obtener-columna tablero indice-columna)))
       (+ indice-columna 1)
       total-columnas)))

; mover-tablero-abajo:
; mueve todas las columnas hacia abajo.
(define (mover-tablero-abajo tablero)
  (if (null? tablero)
      '()
      (mover-columnas-abajo tablero
                            0
                            (cantidad-elementos (car tablero)))))

; ------------------------------------------------------------
; FUNCIÓN PARA INSERTAR UN VALOR EN EL TABLERO
; ------------------------------------------------------------

; insertar-en-posicion:
; recibe un tablero, una fila, una columna y un valor.
; Devuelve un nuevo tablero con ese valor colocado
; en la posición indicada.

(define (insertar-en-posicion tablero fila columna valor)
  (cond
    [(null? tablero) '()]
    [(= fila 0)
     (cons (reemplazar-en-lista (car tablero) columna valor)
           (cdr tablero))]
    [else
     (cons (car tablero)
           (insertar-en-posicion (cdr tablero) (- fila 1) columna valor))]))

; ------------------------------------------------------------
; FUNCIÓN PARA OBTENER POSICIONES VACÍAS
; ------------------------------------------------------------

; posiciones-vacias:
; recorre todo el tablero y devuelve una lista con las posiciones
; donde hay un 0.
; Cada posición se representa como:
; '(fila columna)

(define (posiciones-vacias tablero)

  ; aux-filas:
  ; recorre el tablero fila por fila.
  ; fila-actual guarda el número de fila que se está revisando.
  (define (aux-filas tablero fila-actual)
    (cond
      [(null? tablero) '()]
      [else
       (append (aux-columnas (car tablero) fila-actual 0)
               (aux-filas (cdr tablero) (+ fila-actual 1)))]))

  ; aux-columnas:
  ; recorre una fila columna por columna.
  ; Si encuentra un 0, agrega esa posición a la lista.
  (define (aux-columnas fila fila-actual columna-actual)
    (cond
      [(null? fila) '()]
      [(= (car fila) 0)
       (cons (list fila-actual columna-actual)
             (aux-columnas (cdr fila) fila-actual (+ columna-actual 1)))]
      [else
       (aux-columnas (cdr fila) fila-actual (+ columna-actual 1))]))

  ; inicia el recorrido desde la fila 0
  (aux-filas tablero 0))

; ------------------------------------------------------------
; FUNCIONES PARA INSERTAR LAS DOS FICHAS INICIALES
; ------------------------------------------------------------

; elemento-random:
; recibe una lista y devuelve uno de sus elementos al azar.
; Si la lista está vacía, devuelve #f.
(define (elemento-random lista)
  (if (null? lista)
      #f
      (list-ref lista (random (length lista)))))

; insertar-un-2-random:
; busca las posiciones vacías del tablero,
; elige una de ellas al azar,
; e inserta un 2 en esa posición.
(define (insertar-un-2-random tablero)
  (insertar-un-2-random-aux tablero
                            (elemento-random
                             (posiciones-vacias tablero))))

; insertar-un-2-random-aux:
(define (insertar-un-2-random-aux tablero pos)
  (if (not pos)
      tablero
      (insertar-en-posicion tablero
                            (car pos)
                            (cadr pos)
                            2)))
; insertar-dos-iniciales:
; inserta un 2 aleatorio y luego otro 2 aleatorio.
; Así se genera el estado inicial del juego.
(define (insertar-dos-iniciales tablero)
  (insertar-un-2-random
   (insertar-un-2-random tablero)))