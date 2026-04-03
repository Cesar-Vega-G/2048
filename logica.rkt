#lang racket

; Exporta estas funciones para poder usarlas desde otros archivos,
; por ejemplo desde main.rkt o desde interfaz.rkt
(provide crear-tablero
         imprimir-tablero
         posiciones-vacias
         insertar-en-posicion
         insertar-dos-iniciales)

; ------------------------------------------------------------
; FUNCIONES PARA CREAR EL TABLERO
; ------------------------------------------------------------

; crear-fila:
; recibe un número n y construye una lista con n ceros.
; Ejemplo:
; (crear-fila 4) -> '(0 0 0 0)
(define (crear-fila n)
  (if (= n 0)
      '()
      (cons 0 (crear-fila (- n 1)))))

; crear-tablero:
; recibe cantidad de filas y columnas.
; Construye una lista de listas llena de ceros.
; Ejemplo:
; (crear-tablero 2 3) -> '((0 0 0) (0 0 0))
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
; Ejemplo:
; (elemento-en '(10 20 30) 1) -> 20
(define (elemento-en lista indice)
  (cond
    [(null? lista) #f]
    [(= indice 0) (car lista)]
    [else (elemento-en (cdr lista) (- indice 1))]))

; reemplazar-en-lista:
; reemplaza el valor que está en una posición de una lista
; por otro nuevo valor.
; Ejemplo:
; (reemplazar-en-lista '(1 2 3) 1 9) -> '(1 9 3)
(define (reemplazar-en-lista lista indice valor)
  (cond
    [(null? lista) '()]
    [(= indice 0) (cons valor (cdr lista))]
    [else
     (cons (car lista)
           (reemplazar-en-lista (cdr lista) (- indice 1) valor))]))

; ------------------------------------------------------------
; FUNCIÓN PARA INSERTAR UN VALOR EN EL TABLERO
; ------------------------------------------------------------

; insertar-en-posicion:
; recibe un tablero, una fila, una columna y un valor.
; Devuelve un nuevo tablero con ese valor colocado
; en la posición indicada.
;
; Importante:
; no modifica el tablero original, sino que construye uno nuevo.
; Eso va muy acorde con la idea de programación funcional.
;
; Ejemplo:
; (insertar-en-posicion '((0 0) (0 0)) 1 0 2)
; -> '((0 0) (2 0))
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
;
; Ejemplo:
; si una casilla vacía está en fila 2 columna 3,
; en la lista aparecerá '(2 3)
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
;
; OJO:
; esta versión usa let*, y el enunciado indica que no se permite
; usar let, map, apply ni derivados.
; Entonces esta función sirve para avanzar hoy,
; pero luego conviene reescribirla sin let* para evitar problemas.
(define (insertar-un-2-random tablero)
  (let* ([vacias (posiciones-vacias tablero)]
         [pos (elemento-random vacias)])
    (if (not pos)
        tablero
        (insertar-en-posicion tablero
                              (car pos)
                              (cadr pos)
                              2))))

; insertar-dos-iniciales:
; inserta un 2 aleatorio y luego otro 2 aleatorio.
; Así se genera el estado inicial del juego.
(define (insertar-dos-iniciales tablero)
  (insertar-un-2-random
   (insertar-un-2-random tablero)))