#lang racket

; Importa la librería gráfica de Racket
(require racket/gui)

; Importa las funciones de lógica del juego
(require "logica.rkt")

; Exporta esta función para poder llamarla desde otro archivo
(provide iniciar-interfaz)

; iniciar-interfaz:
; por ahora solo muestra un mensaje en consola.
; Más adelante aquí se construirá la ventana,
; el tablero visual y la lectura de flechas del teclado.
(define (iniciar-interfaz)
  (displayln "La interfaz se hará después."))
