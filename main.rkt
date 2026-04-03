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