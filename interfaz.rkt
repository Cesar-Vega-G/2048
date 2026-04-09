#lang racket
(require racket/gui)
(require "logica.rkt")

(provide iniciar-interfaz)

; ============================================================
; COLORES DE LAS FICHAS
; ============================================================

; color-ficha:
; recibe el valor de una casilla y devuelve el color de fondo
; que le corresponde. Las casillas vacías (0) se muestran en gris.
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

; color-texto:
; devuelve negro para fichas claras y blanco para fichas oscuras.
(define (color-texto valor)
  (if (< valor 16) "black" "white"))

; ============================================================
; PANTALLA DE VICTORIA
; ============================================================

; mostrar-victoria:
; recibe el puntaje final y una función al-menu.
; Muestra una ventana con el resultado y un botón para
; regresar al menú de selección de tamaño.
(define (mostrar-victoria puntaje al-menu)
  (define ventana-victoria
    (new frame%
         [label "¡Ganaste!"]
         [width 300]
         [height 200]))

  (new message%
       [parent ventana-victoria]
       [label "¡Felicidades, lograste el 2048!"])

  (new message%
       [parent ventana-victoria]
       [label (string-append "Puntaje final: " (number->string puntaje))])

  ; regresa al menú de selección de tamaño
  (new button%
       [parent ventana-victoria]
       [label "Menú inicial"]
       [callback
        (lambda (b e)
          (send ventana-victoria show #f)
          (al-menu))])

  (send ventana-victoria show #t))

; ============================================================
; PANTALLA DE DERROTA
; ============================================================

; mostrar-derrota:
; recibe el puntaje final, una función al-menu y una función reintentar.
; Muestra una ventana con el resultado y dos botones:
;   - "Menú inicial": regresa al selector de tamaño
;   - "Reintentar": inicia un juego nuevo con el mismo tamaño
(define (mostrar-derrota puntaje al-menu reintentar)
  (define ventana-derrota
    (new frame%
         [label "¡Perdiste!"]
         [width 300]
         [height 220]))

  (new message%
       [parent ventana-derrota]
       [label "No hay más movimientos posibles."])

  (new message%
       [parent ventana-derrota]
       [label (string-append "Puntaje final: " (number->string puntaje))])

  ; regresa al menú de selección de tamaño
  (new button%
       [parent ventana-derrota]
       [label "Menú inicial"]
       [callback
        (lambda (b e)
          (send ventana-derrota show #f)
          (al-menu))])

  ; reinicia el juego con el mismo tamaño de tablero
  (new button%
       [parent ventana-derrota]
       [label "Reintentar"]
       [callback
        (lambda (b e)
          (send ventana-derrota show #f)
          (reintentar))])

  (send ventana-derrota show #t))

; ============================================================
; VENTANA DEL JUEGO
; ============================================================

; iniciar-juego:
; recibe las dimensiones del tablero y la función al-menu.
; al-menu se pasa hacia abajo hasta procesar-jugada para poder
; llamarla cuando el juego termina.
(define (iniciar-juego filas columnas al-menu)

  (define TAM-CASILLA 80)
  (define MARGEN 10)
  (define ANCHO (+ (* columnas TAM-CASILLA) (* 2 MARGEN)))
  (define ALTO (+ (* filas TAM-CASILLA) 80))

  ; estado del juego usando boxes
  (define estado-tablero
    (box (insertar-dos-iniciales (crear-tablero filas columnas))))
  (define estado-puntaje (box 0))
  (define estado-juego (box 'jugando))

  ; --------------------------------------------------------
  ; FUNCIONES DE DIBUJO
  ; --------------------------------------------------------

  ; dibujar-celda:
  ; dibuja una casilla con su color de fondo y número centrado.
  (define (dibujar-celda dc fila col valor)
    (define x (+ MARGEN (* col TAM-CASILLA)))
    (define y (+ 60 (* fila TAM-CASILLA)))
    (send dc set-brush (color-ficha valor) 'solid)
    (send dc set-pen "white" 2 'solid)
    (send dc draw-rectangle x y TAM-CASILLA TAM-CASILLA)
    (when (> valor 0)
      (define texto (number->string valor))
      (send dc set-font (make-object font% 22 'default 'normal 'bold))
      (send dc set-text-foreground (color-texto valor))
      (define-values (tw th _ __) (send dc get-text-extent texto))
      (send dc draw-text texto
            (+ x (round (/ (- TAM-CASILLA tw) 2)))
            (+ y (round (/ (- TAM-CASILLA th) 2))))))

  ; dibujar-fila:
  ; recorre una fila y dibuja cada casilla.
  (define (dibujar-fila dc fila fila-idx col-idx)
    (cond
      [(null? fila) (void)]
      [else
       (dibujar-celda dc fila-idx col-idx (car fila))
       (dibujar-fila dc (cdr fila) fila-idx (+ col-idx 1))]))

  ; dibujar-tablero:
  ; recorre todas las filas y llama a dibujar-fila.
  (define (dibujar-tablero dc tablero fila-idx)
    (cond
      [(null? tablero) (void)]
      [else
       (dibujar-fila dc (car tablero) fila-idx 0)
       (dibujar-tablero dc (cdr tablero) (+ fila-idx 1))]))

  ; dibujar-todo:
  ; limpia la pantalla y dibuja el puntaje y el tablero.
  (define (dibujar-todo dc)
    (send dc set-background (make-object color% "whitesmoke"))
    (send dc clear)
    (send dc set-text-foreground "black")
    (send dc set-font (make-object font% 16 'default 'normal 'bold))
    (send dc draw-text
          (string-append "Puntaje: " (number->string (unbox estado-puntaje)))
          MARGEN 10)
    (dibujar-tablero dc (unbox estado-tablero) 0))

  ; --------------------------------------------------------
  ; LÓGICA DE JUGADA
  ; --------------------------------------------------------

  ; procesar-jugada:
  ; aplica el movimiento, actualiza el estado y verifica si
  ; el juego terminó. Si ganó, abre mostrar-victoria.
  ; Si perdió, abre mostrar-derrota pasando también reintentar,
  ; que es un lambda que llama iniciar-juego con el mismo tamaño.
  (define (procesar-jugada direccion canvas ventana)
    (when (equal? (unbox estado-juego) 'jugando)
      (define resultado
        (aplicar-jugada-con-puntos (unbox estado-tablero)
                                   direccion
                                   (unbox estado-puntaje)))
      (set-box! estado-tablero (car resultado))
      (set-box! estado-puntaje (cadr resultado))
      (cond
        [(victoria? (unbox estado-tablero))
         (set-box! estado-juego 'victoria)
         (send canvas refresh)
         (send ventana show #f)
         (mostrar-victoria (unbox estado-puntaje) al-menu)]

        [(derrota? (unbox estado-tablero))
         (set-box! estado-juego 'derrota)
         (send canvas refresh)
         (send ventana show #f)
         (mostrar-derrota
          (unbox estado-puntaje)
          al-menu
          ; reintentar es un lambda que abre un juego nuevo
          ; con el mismo tamaño, sin pasar por el menú
          (lambda () (iniciar-juego filas columnas al-menu)))]

        [else
         (send canvas refresh)])))

  ; --------------------------------------------------------
  ; CONSTRUCCIÓN DE LA VENTANA
  ; --------------------------------------------------------

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
               [(equal? tecla 'left)  (procesar-jugada 'izquierda this ventana)]
               [(equal? tecla 'right) (procesar-jugada 'derecha this ventana)]
               [(equal? tecla 'up)    (procesar-jugada 'arriba this ventana)]
               [(equal? tecla 'down)  (procesar-jugada 'abajo this ventana)])
             #t)
           (define/override (on-paint)
             (dibujar-todo (send this get-dc))))
         [parent ventana]))

  (send canvas focus)
  (send ventana show #t))

; ============================================================
; VENTANA DEL MENÚ PRINCIPAL
; ============================================================

; iniciar-interfaz:
; punto de entrada del programa.
; Se pasa a sí misma como al-menu a iniciar-juego, de forma que
; victoria y derrota puedan llamarla para regresar aquí.
(define (iniciar-interfaz)
  (define menu
    (new frame%
         [label "2048 - Seleccionar tamaño"]
         [width 300]
         [height 220]))

  (new message%
       [parent menu]
       [label "Ingrese el tamaño del tablero (entre 4 y 10):"])

  (new message% [parent menu] [label "Filas:"])
  (define campo-filas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  (new message% [parent menu] [label "Columnas:"])
  (define campo-columnas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  (new button%
       [parent menu]
       [label "Jugar"]
       [callback
        (lambda (boton evento)
          (define filas (string->number (send campo-filas get-value)))
          (define columnas (string->number (send campo-columnas get-value)))
          (cond
            [(or (not filas) (not columnas)
                 (< filas 4) (> filas 10)
                 (< columnas 4) (> columnas 10))
             (message-box "Error"
                          "Ingrese números válidos entre 4 y 10."
                          menu)]
            [else
             (send menu show #f)
             (iniciar-juego filas columnas iniciar-interfaz)]))])

  (send menu show #t))