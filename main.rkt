#lang racket

(require "logica.rkt")

(define tablero
  '((2 0 2 2)
    (4 4 0 8)
    (0 0 2 0)
    (2 2 2 2)))

(displayln "Tablero inicial:")
(imprimir-tablero tablero)

(displayln "Jugada arriba:")
(define tablero1 (jugada-arriba tablero))
(imprimir-tablero tablero1)

(displayln "Jugada izquierda:")
(define tablero2 (jugada-izquierda tablero1))
(imprimir-tablero tablero2)

(displayln "Jugada abajo:")
(define tablero3 (jugada-abajo tablero2))
(imprimir-tablero tablero3)

(displayln "Jugada derecha:")
(define tablero4 (jugada-derecha tablero3))
(imprimir-tablero tablero4)