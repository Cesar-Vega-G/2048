#lang racket

(require "logica.rkt")

(define tablero (tablero-inicial 4 4))

(displayln "Tablero inicial:")
(imprimir-tablero tablero)

(displayln "Posiciones vacías:")
(displayln (posiciones-vacias tablero))