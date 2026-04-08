#lang racket

(require racket/gui)
(require "logica.rkt")

(provide iniciar-interfaz)

; --------------------------------------------
; Constantes de dibujo
; --------------------------------------------

(define TAM-CASILLA 80)
(define MARGEN 10)
(define ANCHO 400)
(define ALTO 450)

; --------------------------------------------
; Estado actual del juego
; --------------------------------------------

(define tablero-actual
  (insertar-dos-iniciales (crear-tablero 4 4)))

(define puntaje-actual 0)

; --------------------------------------------
; Funciones auxiliares
; --------------------------------------------

(define (numero-a-texto n)
  (if (= n 0)
      ""
      (number->string n)))

(define (dibujar-celda dc fila col valor)
  (send dc draw-rectangle
        (+ MARGEN (* col TAM-CASILLA))
        (+ 50 (* fila TAM-CASILLA))
        TAM-CASILLA
        TAM-CASILLA)

  (send dc draw-text
        (numero-a-texto valor)
        (+ MARGEN (* col TAM-CASILLA) 30)
        (+ 50 (* fila TAM-CASILLA) 30)))

(define (dibujar-fila dc fila fila-indice col-indice)
  (cond
    [(null? fila) (void)]
    [else
     (dibujar-celda dc fila-indice col-indice (car fila))
     (dibujar-fila dc (cdr fila) fila-indice (+ col-indice 1))]))

(define (dibujar-tablero dc tablero fila-indice)
  (cond
    [(null? tablero) (void)]
    [else
     (dibujar-fila dc (car tablero) fila-indice 0)
     (dibujar-tablero dc (cdr tablero) (+ fila-indice 1))]))

(define (dibujar-pantalla dc)
  (send dc clear)
  (send dc draw-text
        (string-append "Puntaje: " (number->string puntaje-actual))
        10
        10)
  (dibujar-tablero dc tablero-actual 0))

; --------------------------------------------
; Movimiento según tecla
; --------------------------------------------

(define (mover-segun-tecla tecla)
  (cond
    [(equal? tecla 'left)  (actualizar-juego 'izquierda)]
    [(equal? tecla 'right) (actualizar-juego 'derecha)]
    [(equal? tecla 'up)    (actualizar-juego 'arriba)]
    [(equal? tecla 'down)  (actualizar-juego 'abajo)]
    [else (void)]))

(define (actualizar-juego direccion)
  (define resultado
    (aplicar-jugada-con-puntos tablero-actual direccion puntaje-actual))

  ; Esto asume que la función devuelve:
  ; (list nuevo-tablero nuevo-puntaje)
  (set! tablero-actual (car resultado))
  (set! puntaje-actual (cadr resultado)))

; --------------------------------------------
; Interfaz principal
; --------------------------------------------

(define (iniciar-interfaz)
  (define ventana
    (new frame%
         [label "2048"]
         [width ANCHO]
         [height ALTO]))

  (define canvas-juego
    (new
     (class canvas%
       (super-new)

       (define/override (on-char event)
         (mover-segun-tecla (send event get-key-code))
         (send this refresh)
         #t)

       (define/override (on-paint)
         (dibujar-pantalla (send this get-dc))))
     [parent ventana]))

  (send canvas-juego focus)
  (send ventana show #t))
