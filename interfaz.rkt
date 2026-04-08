#lang racket
(require racket/gui)
(require "logica.rkt")

(provide iniciar-interfaz)

; --------------------------------------------
; Colores por valor de ficha
; --------------------------------------------

(define (color-ficha valor)
  (cond
    [(= valor 0)    "lightgray"]
    [(= valor 2)    "lightyellow"]
    [(= valor 4)    "moccasin"]
    [(= valor 8)    "orange"]
    [(= valor 16)   "darkorange"]
    [(= valor 32)   "tomato"]
    [(= valor 64)   "orangered"]
    [(= valor 128)  "gold"]
    [(= valor 256)  "yellow"]
    [(= valor 512)  "khaki"]
    [(= valor 1024) "goldenrod"]
    [(= valor 2048) "darkgoldenrod"]
    [else           "dimgray"]))

(define (color-texto valor)
  (if (< valor 16) "black" "white"))


; --------------------------------------------
; Ventana del juego
; --------------------------------------------

(define (iniciar-juego filas columnas)
  (define TAM-CASILLA 80)
  (define MARGEN 10)
  (define ANCHO (+ (* columnas TAM-CASILLA) (* 2 MARGEN)))
  (define ALTO (+ (* filas TAM-CASILLA) 80))

  ; estado del juego
  (define estado-tablero
    (box (insertar-dos-iniciales (crear-tablero filas columnas))))
  (define estado-puntaje (box 0))
  (define estado-juego (box 'jugando)) ; 'jugando 'victoria 'derrota

  ; dibuja una sola celda
  (define (dibujar-celda dc fila col valor)
  (define x (+ MARGEN (* col TAM-CASILLA)))
  (define y (+ 60 (* fila TAM-CASILLA)))
  
  ; dibuja el fondo de la celda
  (send dc set-brush (color-ficha valor) 'solid)
  (send dc set-pen "white" 2 'solid)
  (send dc draw-rectangle x y TAM-CASILLA TAM-CASILLA)
  
  ; dibuja el número ENCIMA del fondo
  (when (> valor 0)
    (define texto (number->string valor))
    (send dc set-font (make-object font% 22 'default 'normal 'bold))
    (send dc set-text-foreground (color-texto valor))
    ; get-text-extent devuelve el ancho y alto del texto
    (define-values (tw th _ __) (send dc get-text-extent texto))
    ; centra el texto dentro de la celda
    (send dc draw-text texto
          (+ x (round (/ (- TAM-CASILLA tw) 2)))
          (+ y (round (/ (- TAM-CASILLA th) 2))))))

  ; dibuja una fila completa
  (define (dibujar-fila dc fila fila-idx col-idx)
    (cond
      [(null? fila) (void)]
      [else
       (dibujar-celda dc fila-idx col-idx (car fila))
       (dibujar-fila dc (cdr fila) fila-idx (+ col-idx 1))]))

  ; dibuja todo el tablero
  (define (dibujar-tablero dc tablero fila-idx)
    (cond
      [(null? tablero) (void)]
      [else
       (dibujar-fila dc (car tablero) fila-idx 0)
       (dibujar-tablero dc (cdr tablero) (+ fila-idx 1))]))

  ; dibuja la pantalla completa
  (define (dibujar-todo dc)
  (send dc set-background (make-object color% "whitesmoke"))
  (send dc clear)
    ; puntaje
    (send dc set-text-foreground "black")
    (send dc set-font (make-object font% 16 'default 'normal 'bold))
    (send dc draw-text
          (string-append "Puntaje: " (number->string (unbox estado-puntaje)))
          MARGEN 10)
    ; tablero
    (dibujar-tablero dc (unbox estado-tablero) 0)
    ; mensaje de victoria o derrota
    (cond
      [(equal? (unbox estado-juego) 'victoria)
       (send dc set-text-foreground "green")
       (send dc set-font (make-object font% 28 'default 'normal 'bold))
       (send dc draw-text "¡Ganaste! 🎉" MARGEN 20)]
      [(equal? (unbox estado-juego) 'derrota)
       (send dc set-text-foreground "red")
       (send dc set-font (make-object font% 28 'default 'normal 'bold))
       (send dc draw-text "¡Perdiste!" MARGEN 20)]))

  ; procesa una jugada
  (define (procesar-jugada direccion canvas)
    (when (equal? (unbox estado-juego) 'jugando)
      (define resultado
        (aplicar-jugada-con-puntos (unbox estado-tablero)
                                   direccion
                                   (unbox estado-puntaje)))
      (set-box! estado-tablero (car resultado))
      (set-box! estado-puntaje (cadr resultado))
      (cond
        [(victoria? (unbox estado-tablero))
         (set-box! estado-juego 'victoria)]
        [(derrota? (unbox estado-tablero))
         (set-box! estado-juego 'derrota)])
      (send canvas refresh)))

  ; ventana del juego
  (define ventana
    (new frame%
         [label "2048"]
         [width ANCHO]
         [height ALTO]))

  (define canvas
    (new (class canvas%
           (super-new)
           (define/override (on-char evento)
             (define tecla (send evento get-key-code))
             (cond
               [(equal? tecla 'left)  (procesar-jugada 'izquierda this)]
               [(equal? tecla 'right) (procesar-jugada 'derecha this)]
               [(equal? tecla 'up)    (procesar-jugada 'arriba this)]
               [(equal? tecla 'down)  (procesar-jugada 'abajo this)])
             #t)
           (define/override (on-paint)
             (dibujar-todo (send this get-dc))))
         [parent ventana]))

  (send canvas focus)
  (send ventana show #t))

; --------------------------------------------
; Ventana del menú
; --------------------------------------------

(define (iniciar-interfaz)
  (define menu
    (new frame%
         [label "2048 - Seleccionar tamaño"]
         [width 300]
         [height 220]))

  ; instrucciones
  (new message%
       [parent menu]
       [label "Ingrese el tamaño del tablero (entre 4 y 10):"])

  ; campo filas
  (new message% [parent menu] [label "Filas:"])
  (define campo-filas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  ; campo columnas
  (new message% [parent menu] [label "Columnas:"])
  (define campo-columnas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  ; botón jugar
  (new button%
       [parent menu]
       [label "Jugar"]
       [callback
        (lambda (boton evento)
          (define filas (string->number (send campo-filas get-value)))
          (define columnas (string->number (send campo-columnas get-value)))
          (cond
            ; valida que sean números entre 4 y 10
            [(or (not filas) (not columnas)
                 (< filas 4) (> filas 10)
                 (< columnas 4) (> columnas 10))
             (message-box "Error"
                          "Ingrese números válidos entre 4 y 10."
                          menu)]
            [else
             (send menu show #f)  ; cierra el menú
             (iniciar-juego filas columnas)]))])

  (send menu show #t))