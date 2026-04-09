#lang racket

; Se exporta estas funciones para poder usarlas desde otros archivos,

(provide crear-tablero
         imprimir-tablero
         posiciones-vacias
         insertar-en-posicion
         insertar-dos-iniciales
         obtener-columna
         reemplazar-columna
         tableros-iguales?
         tablero-cambio?
         valor-nuevo-random
         insertar-ficha-random
         victoria?
         derrota?
         mover-fila-izquierda-con-puntos
         mover-tablero-izquierda-con-puntos
         mover-fila-derecha-con-puntos
         mover-tablero-derecha-con-puntos
         mover-tablero-arriba-con-puntos
         mover-tablero-abajo-con-puntos
         aplicar-movimiento-con-puntos
         aplicar-jugada-con-puntos)

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
; FUNCIONES PARA MOVER CON PUNTUACION
; ------------------------------------------------------------

; combinar-fila-izquierda-con-puntos:
; recibe una fila sin ceros y devuelve:
; 1. fila combinada
; 2. puntos ganados
(define (combinar-fila-izquierda-con-puntos fila)
  (cond
    [(null? fila) (list '() 0)]
    [(null? (cdr fila)) (list (list (car fila)) 0)]
    [(= (car fila) (cadr fila))
     (combinar-fila-izquierda-con-puntos-suma
      (+ (car fila) (cadr fila))
      (combinar-fila-izquierda-con-puntos (cddr fila)))]
    [else
     (combinar-fila-izquierda-con-puntos-no-suma
      (car fila)
      (combinar-fila-izquierda-con-puntos (cdr fila)))]))

(define (combinar-fila-izquierda-con-puntos-suma nuevo-valor resultado-resto)
  (list
   (cons nuevo-valor
         (car resultado-resto))
   (+ nuevo-valor
      (cadr resultado-resto))))

(define (combinar-fila-izquierda-con-puntos-no-suma valor resultado-resto)
  (list
   (cons valor
         (car resultado-resto))
   (cadr resultado-resto)))

; cantidad-elementos:
; cuenta cuántos elementos tiene una lista.
(define (cantidad-elementos lista)
  (if (null? lista)
      0
      (+ 1 (cantidad-elementos (cdr lista)))))

; concatenar:
; une dos listas sin usar map/apply.
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

; rellenar-con-ceros:
; agrega ceros al final hasta recuperar el tamaño original.
(define (rellenar-con-ceros fila tam-original)
  (concatenar fila
              (crear-fila (- tam-original
                             (cantidad-elementos fila)))))

; mover-fila-izquierda-con-puntos:
; devuelve:
; 1. fila final
; 2. puntos ganados
(define (mover-fila-izquierda-con-puntos fila)
  (mover-fila-izquierda-con-puntos-aux
   fila
   (combinar-fila-izquierda-con-puntos
    (quitar-ceros-fila fila))))

(define (mover-fila-izquierda-con-puntos-aux fila resultado-combinacion)
  (list
   (rellenar-con-ceros
    (car resultado-combinacion)
    (cantidad-elementos fila))
   (cadr resultado-combinacion)))

; mover-tablero-izquierda-con-puntos:
; devuelve:
; 1. tablero final
; 2. puntos ganados
(define (mover-tablero-izquierda-con-puntos tablero)
  (cond
    [(null? tablero) (list '() 0)]
    [else
     (mover-tablero-izquierda-con-puntos-aux
      (mover-fila-izquierda-con-puntos (car tablero))
      (mover-tablero-izquierda-con-puntos (cdr tablero)))]))

(define (mover-tablero-izquierda-con-puntos-aux resultado-fila resultado-resto)
  (list
   (cons (car resultado-fila)
         (car resultado-resto))
   (+ (cadr resultado-fila)
      (cadr resultado-resto))))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER A LA DERECHA CON PUNTUACION
; ------------------------------------------------------------

; invertir-lista:
; devuelve una lista en orden inverso.
(define (invertir-lista lista)
  (invertir-lista-aux lista '()))

(define (invertir-lista-aux lista acumulado)
  (if (null? lista)
      acumulado
      (invertir-lista-aux (cdr lista)
                          (cons (car lista) acumulado))))

; mover-fila-derecha-con-puntos:
; devuelve:
; 1. fila final
; 2. puntos ganados
(define (mover-fila-derecha-con-puntos fila)
  (mover-fila-derecha-con-puntos-aux
   (mover-fila-izquierda-con-puntos
    (invertir-lista fila))))

(define (mover-fila-derecha-con-puntos-aux resultado-izquierda)
  (list
   (invertir-lista (car resultado-izquierda))
   (cadr resultado-izquierda)))

; mover-tablero-derecha-con-puntos:
; devuelve:
; 1. tablero final
; 2. puntos ganados
(define (mover-tablero-derecha-con-puntos tablero)
  (cond
    [(null? tablero) (list '() 0)]
    [else
     (mover-tablero-derecha-con-puntos-aux
      (mover-fila-derecha-con-puntos (car tablero))
      (mover-tablero-derecha-con-puntos (cdr tablero)))]))

(define (mover-tablero-derecha-con-puntos-aux resultado-fila resultado-resto)
  (list
   (cons (car resultado-fila)
         (car resultado-resto))
   (+ (cadr resultado-fila)
      (cadr resultado-resto))))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER ARRIBA CON PUNTUACION
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

(define (mover-columnas-arriba-con-puntos tablero indice-columna total-columnas)
  (if (= indice-columna total-columnas)
      (list tablero 0)
      (mover-columnas-arriba-con-puntos-aux
       tablero
       indice-columna
       total-columnas
       (mover-fila-izquierda-con-puntos
        (obtener-columna tablero indice-columna)))))

(define (mover-columnas-arriba-con-puntos-aux tablero indice-columna total-columnas resultado-columna)
  (mover-columnas-arriba-con-puntos-aux-2
   (reemplazar-columna tablero
                       indice-columna
                       (car resultado-columna))
   (+ indice-columna 1)
   total-columnas
   (cadr resultado-columna)))

(define (mover-columnas-arriba-con-puntos-aux-2 tablero-actual siguiente-columna total-columnas puntos-columna)
  (mover-columnas-arriba-con-puntos-aux-3
   puntos-columna
   (mover-columnas-arriba-con-puntos tablero-actual
                                     siguiente-columna
                                     total-columnas)))

(define (mover-columnas-arriba-con-puntos-aux-3 puntos-columna resultado-resto)
  (list
   (car resultado-resto)
   (+ puntos-columna
      (cadr resultado-resto))))

(define (mover-tablero-arriba-con-puntos tablero)
  (if (null? tablero)
      (list '() 0)
      (mover-columnas-arriba-con-puntos tablero
                                        0
                                        (cantidad-elementos (car tablero)))))

; ------------------------------------------------------------
; FUNCIONES PARA MOVER ABAJO CON PUNTUACION
; ------------------------------------------------------------

(define (mover-columnas-abajo-con-puntos tablero indice-columna total-columnas)
  (if (= indice-columna total-columnas)
      (list tablero 0)
      (mover-columnas-abajo-con-puntos-aux
       tablero
       indice-columna
       total-columnas
       (mover-fila-derecha-con-puntos
        (obtener-columna tablero indice-columna)))))

(define (mover-columnas-abajo-con-puntos-aux tablero indice-columna total-columnas resultado-columna)
  (mover-columnas-abajo-con-puntos-aux-2
   (reemplazar-columna tablero
                       indice-columna
                       (car resultado-columna))
   (+ indice-columna 1)
   total-columnas
   (cadr resultado-columna)))

(define (mover-columnas-abajo-con-puntos-aux-2 tablero-actual siguiente-columna total-columnas puntos-columna)
  (mover-columnas-abajo-con-puntos-aux-3
   puntos-columna
   (mover-columnas-abajo-con-puntos tablero-actual
                                    siguiente-columna
                                    total-columnas)))

(define (mover-columnas-abajo-con-puntos-aux-3 puntos-columna resultado-resto)
  (list
   (car resultado-resto)
   (+ puntos-columna
      (cadr resultado-resto))))

(define (mover-tablero-abajo-con-puntos tablero)
  (if (null? tablero)
      (list '() 0)
      (mover-columnas-abajo-con-puntos tablero
                                       0
                                       (cantidad-elementos (car tablero)))))

; ------------------------------------------------------------
; COMPARAR TABLEROS
; ------------------------------------------------------------

; listas-iguales?:
; compara dos listas elemento por elemento.
(define (listas-iguales? lista1 lista2)
  (cond
    [(and (null? lista1) (null? lista2)) #t]
    [(or (null? lista1) (null? lista2)) #f]
    [(equal? (car lista1) (car lista2))
     (listas-iguales? (cdr lista1) (cdr lista2))]
    [else #f]))

; tableros-iguales?:
; compara dos tableros fila por fila.
(define (tableros-iguales? tablero1 tablero2)
  (cond
    [(and (null? tablero1) (null? tablero2)) #t]
    [(or (null? tablero1) (null? tablero2)) #f]
    [(listas-iguales? (car tablero1) (car tablero2))
     (tableros-iguales? (cdr tablero1) (cdr tablero2))]
    [else #f]))

; tablero-cambio?:
; devuelve #t si el tablero cambió después del movimiento.
(define (tablero-cambio? tablero-original tablero-nuevo)
  (not (tableros-iguales? tablero-original tablero-nuevo)))

; ------------------------------------------------------------
; GENERAR NUEVA FICHA
; ------------------------------------------------------------

; valor-nuevo-random:
; devuelve 2 o 4 aleatoriamente.
(define (valor-nuevo-random)
  (if (= (random 2) 0)
      2
      4))

; insertar-ficha-random:
; inserta una ficha nueva (2 o 4) en una posición vacía aleatoria.
; si no hay espacios vacíos, devuelve el tablero igual.
(define (insertar-ficha-random tablero)
  (insertar-ficha-random-aux tablero
                             (elemento-random
                              (posiciones-vacias tablero))))

; insertar-ficha-random-aux:
; auxiliar para evitar let.
(define (insertar-ficha-random-aux tablero posicion)
  (if (not posicion)
      tablero
      (insertar-en-posicion tablero
                            (car posicion)
                            (cadr posicion)
                            (valor-nuevo-random))))

; ------------------------------------------------------------
; APLICAR MOVIMIENTOS Y JUGADAS CON PUNTUACION
; ------------------------------------------------------------

; aplicar-movimiento-con-puntos:
; recibe un tablero y una dirección.
; devuelve:
; 1. tablero movido
; 2. puntos ganados en esa jugada
(define (aplicar-movimiento-con-puntos tablero direccion)
  (cond
    [(equal? direccion 'izquierda)
     (mover-tablero-izquierda-con-puntos tablero)]
    [(equal? direccion 'derecha)
     (mover-tablero-derecha-con-puntos tablero)]
    [(equal? direccion 'arriba)
     (mover-tablero-arriba-con-puntos tablero)]
    [(equal? direccion 'abajo)
     (mover-tablero-abajo-con-puntos tablero)]
    [else
     (list tablero 0)]))

; aplicar-jugada-con-puntos:
; recibe tablero, direccion y puntaje actual.
; devuelve:
; 1. tablero final
; 2. puntaje total actualizado
(define (aplicar-jugada-con-puntos tablero direccion puntaje-actual)
  (aplicar-jugada-con-puntos-aux
   tablero
   puntaje-actual
   (aplicar-movimiento-con-puntos tablero direccion)))

(define (aplicar-jugada-con-puntos-aux tablero puntaje-actual resultado-movimiento)
  (aplicar-jugada-con-puntos-aux-2
   tablero
   puntaje-actual
   (car resultado-movimiento)
   (cadr resultado-movimiento)))

(define (aplicar-jugada-con-puntos-aux-2 tablero-original puntaje-actual tablero-movido puntos-ganados)
  (if (tablero-cambio? tablero-original tablero-movido)
      (list
       (insertar-ficha-random tablero-movido)
       (+ puntaje-actual puntos-ganados))
      (list tablero-original puntaje-actual)))

; ------------------------------------------------------------
; DETECTAR VICTORIA
; ------------------------------------------------------------

; fila-contiene-2048?:
; revisa si una fila tiene una casilla con valor 2048.
(define (fila-contiene-2048? fila)
  (cond
    [(null? fila) #f]
    [(= (car fila) 2048) #t]
    [else
     (fila-contiene-2048? (cdr fila))]))

; victoria?:
; revisa si en alguna fila del tablero existe una casilla 2048.
(define (victoria? tablero)
  (cond
    [(null? tablero) #f]
    [(fila-contiene-2048? (car tablero)) #t]
    [else
     (victoria? (cdr tablero))]))

; ------------------------------------------------------------
; DETECTAR DERROTA
; ------------------------------------------------------------

; sin-espacios-vacios?:
; devuelve #t si el tablero no tiene casillas vacías.
(define (sin-espacios-vacios? tablero)
  (null? (posiciones-vacias tablero)))

; no-se-puede-mover-izquierda?:
; devuelve #t si mover a la izquierda no cambia el tablero.
(define (no-se-puede-mover-izquierda? tablero)
  (tableros-iguales? tablero
                     (car (mover-tablero-izquierda-con-puntos tablero))))

; no-se-puede-mover-derecha?:
; devuelve #t si mover a la derecha no cambia el tablero.
(define (no-se-puede-mover-derecha? tablero)
  (tableros-iguales? tablero
                     (car (mover-tablero-derecha-con-puntos tablero))))

; no-se-puede-mover-arriba?:
; devuelve #t si mover arriba no cambia el tablero.
(define (no-se-puede-mover-arriba? tablero)
  (tableros-iguales? tablero
                     (car (mover-tablero-arriba-con-puntos tablero))))

; no-se-puede-mover-abajo?:
; devuelve #t si mover abajo no cambia el tablero.
(define (no-se-puede-mover-abajo? tablero)
  (tableros-iguales? tablero
                     (car (mover-tablero-abajo-con-puntos tablero))))

; derrota?:
; hay derrota si no quedan espacios vacíos
; y además ningún movimiento cambia el tablero.
(define (derrota? tablero)
  (and (sin-espacios-vacios? tablero)
       (no-se-puede-mover-izquierda? tablero)
       (no-se-puede-mover-derecha? tablero)
       (no-se-puede-mover-arriba? tablero)
       (no-se-puede-mover-abajo? tablero)))
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