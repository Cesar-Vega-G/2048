#lang racket

; Importa las funciones que están en el archivo logica.rkt
(require "logica.rkt")

; Crea un tablero inicial de 4 filas por 4 columnas lleno de ceros
(define tablero-inicial (crear-tablero 4 4))

; Muestra un texto en consola para identificar qué se va a imprimir
(displayln "Tablero vacío:")

; Imprime el tablero vacío fila por fila
(imprimir-tablero tablero-inicial)

; Crea un nuevo tablero a partir del tablero vacío,
; pero ahora con dos fichas iniciales de valor 2 en posiciones aleatorias
(define tablero-con-dos (insertar-dos-iniciales tablero-inicial))

; Muestra un texto en consola
(displayln "Tablero con dos 2 iniciales:")

; Imprime el tablero ya con las dos fichas iniciales
(imprimir-tablero tablero-con-dos)

; Muestra un texto en consola
(displayln "Posiciones vacías:")

; Imprime la lista de posiciones donde todavía hay un 0
(displayln (posiciones-vacias tablero-con-dos))
(displayln "Prueba mover fila a la izquierda:")
(displayln (mover-fila-izquierda '(2 0 2 4)))
(displayln (mover-fila-izquierda '(2 2 2 0)))
(displayln (mover-fila-izquierda '(2 2 2 2)))

(displayln "Prueba mover tablero a la izquierda:")
(imprimir-tablero
 (mover-tablero-izquierda
  '((2 0 2 4)
    (2 2 2 0)
    (0 4 0 4)
    (2 0 0 2))))

(displayln "Prueba mover fila a la derecha:")
(displayln (mover-fila-derecha '(2 0 2 4)))
(displayln (mover-fila-derecha '(2 2 2 0)))
(displayln (mover-fila-derecha '(2 2 2 2)))

(displayln "Prueba mover tablero a la derecha:")
(imprimir-tablero
 (mover-tablero-derecha
  '((2 0 2 4)
    (2 2 2 0)
    (0 4 0 4)
    (2 0 0 2))))

(displayln "Prueba mover tablero arriba:")
(imprimir-tablero
 (mover-tablero-arriba
  '((2 0 2 4)
    (2 2 0 4)
    (0 2 2 0)
    (0 0 2 4))))

(displayln "Prueba mover tablero abajo:")
(imprimir-tablero
 (mover-tablero-abajo
  '((2 0 2 4)
    (2 2 0 4)
    (0 2 2 0)
    (0 0 2 4))))