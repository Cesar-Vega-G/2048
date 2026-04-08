#lang racket

(require "logica.rkt")
(require "interfaz.rkt")

; ------------------------------------------------------------
; TABLERO INICIAL
; ------------------------------------------------------------

(define tablero-inicial (crear-tablero 4 4))

(displayln "Tablero vacío:")
(imprimir-tablero tablero-inicial)

(define tablero-con-dos (insertar-dos-iniciales tablero-inicial))

(displayln "Tablero con dos 2 iniciales:")
(imprimir-tablero tablero-con-dos)

(displayln "Posiciones vacías:")
(displayln (posiciones-vacias tablero-con-dos))

; ------------------------------------------------------------
; PRUEBAS DE MOVIMIENTO CON PUNTOS
; ------------------------------------------------------------

(displayln "Prueba mover fila a la izquierda con puntos:")
(displayln (mover-fila-izquierda-con-puntos '(2 0 2 4)))
(displayln (mover-fila-izquierda-con-puntos '(2 2 2 0)))
(displayln (mover-fila-izquierda-con-puntos '(2 2 2 2)))

(displayln "Prueba mover tablero a la izquierda con puntos:")
(displayln "Tablero resultante:")
(imprimir-tablero
 (car
  (mover-tablero-izquierda-con-puntos
   '((2 0 2 4)
     (2 2 2 0)
     (0 4 0 4)
     (2 0 0 2)))))
(displayln "Puntos ganados:")
(displayln
 (cadr
  (mover-tablero-izquierda-con-puntos
   '((2 0 2 4)
     (2 2 2 0)
     (0 4 0 4)
     (2 0 0 2)))))

(displayln "Prueba mover fila a la derecha con puntos:")
(displayln (mover-fila-derecha-con-puntos '(2 0 2 4)))
(displayln (mover-fila-derecha-con-puntos '(2 2 2 0)))
(displayln (mover-fila-derecha-con-puntos '(2 2 2 2)))

(displayln "Prueba mover tablero a la derecha con puntos:")
(displayln "Tablero resultante:")
(imprimir-tablero
 (car
  (mover-tablero-derecha-con-puntos
   '((2 0 2 4)
     (2 2 2 0)
     (0 4 0 4)
     (2 0 0 2)))))
(displayln "Puntos ganados:")
(displayln
 (cadr
  (mover-tablero-derecha-con-puntos
   '((2 0 2 4)
     (2 2 2 0)
     (0 4 0 4)
     (2 0 0 2)))))

(displayln "Prueba mover tablero arriba con puntos:")
(displayln "Tablero resultante:")
(imprimir-tablero
 (car
  (mover-tablero-arriba-con-puntos
   '((2 0 2 4)
     (2 2 0 4)
     (0 2 2 0)
     (0 0 2 4)))))
(displayln "Puntos ganados:")
(displayln
 (cadr
  (mover-tablero-arriba-con-puntos
   '((2 0 2 4)
     (2 2 0 4)
     (0 2 2 0)
     (0 0 2 4)))))

(displayln "Prueba mover tablero abajo con puntos:")
(displayln "Tablero resultante:")
(imprimir-tablero
 (car
  (mover-tablero-abajo-con-puntos
   '((2 0 2 4)
     (2 2 0 4)
     (0 2 2 0)
     (0 0 2 4)))))
(displayln "Puntos ganados:")
(displayln
 (cadr
  (mover-tablero-abajo-con-puntos
   '((2 0 2 4)
     (2 2 0 4)
     (0 2 2 0)
     (0 0 2 4)))))

; ------------------------------------------------------------
; PRUEBAS DE COMPARACION
; ------------------------------------------------------------

(displayln "Prueba comparar tableros:")
(displayln
 (tableros-iguales?
  '((2 0)
    (0 2))
  '((4 0)
    (0 2))))

(displayln
 (tablero-cambio?
  '((2 0)
    (0 2))
  '((4 0)
    (0 0))))

; ------------------------------------------------------------
; PRUEBAS DE JUGADA COMPLETA CON PUNTOS
; ------------------------------------------------------------

(displayln "Prueba aplicar movimiento con puntos:")
(displayln
 (aplicar-movimiento-con-puntos
  '((2 0 2 4)
    (2 2 2 0)
    (0 4 0 4)
    (2 0 0 2))
  'izquierda))

(displayln "Prueba aplicar jugada completa con puntos:")
(displayln
 (aplicar-jugada-con-puntos
  '((2 0 2 4)
    (2 2 2 0)
    (0 4 0 4)
    (2 0 0 2))
  'izquierda
  0))

; ------------------------------------------------------------
; PRUEBAS DE VICTORIA
; ------------------------------------------------------------

(displayln "Prueba victoria:")
(displayln
 (victoria?
  '((2 4 8 16)
    (32 64 128 256)
    (512 1024 2048 0)
    (0 0 0 0))))

(displayln
 (victoria?
  '((2 4 8 16)
    (32 64 128 256)
    (512 1024 1024 0)
    (0 0 0 0))))

; ------------------------------------------------------------
; PRUEBAS DE DERROTA
; ------------------------------------------------------------

(displayln "Prueba derrota real:")
(displayln
 (derrota?
  '((2 4 2 4)
    (4 2 4 2)
    (2 4 2 4)
    (4 2 4 2))))

(displayln "Prueba tablero lleno pero sin derrota:")
(displayln
 (derrota?
  '((2 4 2 4)
    (4 2 4 2)
    (2 2 8 4)
    (4 8 4 2))))

(displayln "Prueba tablero con espacios vacíos:")
(displayln
 (derrota?
  '((2 4 2 4)
    (4 0 4 2)
    (2 4 2 4)
    (4 2 4 2))))

(iniciar-interfaz)