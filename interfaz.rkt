#lang racket

; racket/gui provee todas las clases para construir ventanas,
; botones, campos de texto, canvas y captura de eventos.
(require racket/gui)

; logica.rkt contiene todas las funciones puras del juego:
; crear tablero, mover fichas, detectar victoria/derrota, etc.
(require "logica.rkt")

; exporta iniciar-interfaz para que main.rkt pueda llamarla
(provide iniciar-interfaz)

; ============================================================
; COLORES DE LAS FICHAS
; ============================================================

; color-ficha:
; recibe el valor numérico de una casilla y devuelve un string
; con el nombre del color de fondo que le corresponde.
; Cada potencia de 2 tiene su propio color para diferenciarlas visualmente.
; El 0 representa una casilla vacía y se muestra en gris claro.
(define (color-ficha valor)
  (cond
    [(= valor 0)    "lightgray"]    ; casilla vacía
    [(= valor 2)    "lightyellow"]  ; valor más bajo
    [(= valor 4)    "moccasin"]
    [(= valor 8)    "orange"]
    [(= valor 16)   "darkorange"]
    [(= valor 32)   "tomato"]
    [(= valor 64)   "orangered"]
    [(= valor 128)  "gold"]
    [(= valor 256)  "yellow"]
    [(= valor 512)  "khaki"]
    [(= valor 1024) "goldenrod"]
    [(= valor 2048) "darkgoldenrod"] ; valor de victoria
    [else           "dimgray"]))     ; valores mayores a 2048

; color-texto:
; devuelve el color del número escrito dentro de la ficha.
; Fichas con valores bajos tienen fondo claro, por eso usan texto negro.
; Fichas con valores altos tienen fondo oscuro, por eso usan texto blanco.
(define (color-texto valor)
  (if (< valor 16) "black" "white"))

; ============================================================
; PANTALLA DE VICTORIA
; ============================================================

; mostrar-victoria:
; recibe el puntaje final, la ventana del juego y al-menu.
; Abre una ventana nueva que felicita al jugador y muestra el puntaje.
; La ventana del juego se mantiene visible hasta que el jugador
; presione "Menú inicial", momento en que se cierran ambas ventanas.
(define (mostrar-victoria puntaje ventana-juego al-menu)

  ; crea la ventana de victoria
  (define ventana-victoria
    (new frame%
         [label "¡Ganaste!"]
         [width 300]
         [height 200]))

  ; mensaje principal de felicitación
  (new message%
       [parent ventana-victoria]
       [label "¡Felicidades, lograste el 2048!"])

  ; muestra el puntaje acumulado durante la partida
  (new message%
       [parent ventana-victoria]
       [label (string-append "Puntaje final: " (number->string puntaje))])

  ; botón para regresar al menú de selección de tamaño
  (new button%
       [parent ventana-victoria]
       [label "Menú inicial"]
       [callback
        (lambda (b e)
          (send ventana-victoria show #f)  ; cierra la ventana de victoria
          (send ventana-juego show #f)     ; cierra la ventana del juego
          (al-menu))])                     ; abre el menú de selección

  ; hace visible la ventana de victoria
  (send ventana-victoria show #t))

; ============================================================
; PANTALLA DE DERROTA
; ============================================================

; mostrar-derrota:
; recibe el puntaje final, la ventana del juego, al-menu y reintentar.
; Abre una ventana que indica que no hay más movimientos posibles.
; La ventana del juego se mantiene visible hasta que el jugador
; presione uno de los dos botones:
;   - "Menú inicial": cierra ambas ventanas y vuelve al selector de tamaño
;   - "Reintentar": cierra ambas ventanas e inicia una partida nueva
(define (mostrar-derrota puntaje ventana-juego al-menu reintentar)

  ; crea la ventana de derrota
  (define ventana-derrota
    (new frame%
         [label "¡Perdiste!"]
         [width 300]
         [height 220]))

  ; mensaje informando que no hay movimientos disponibles
  (new message%
       [parent ventana-derrota]
       [label "No hay más movimientos posibles."])

  ; muestra el puntaje acumulado durante la partida
  (new message%
       [parent ventana-derrota]
       [label (string-append "Puntaje final: " (number->string puntaje))])

  ; botón para regresar al menú de selección de tamaño
  (new button%
       [parent ventana-derrota]
       [label "Menú inicial"]
       [callback
        (lambda (b e)
          (send ventana-derrota show #f)  ; cierra la ventana de derrota
          (send ventana-juego show #f)    ; cierra la ventana del juego
          (al-menu))])                    ; abre el menú de selección

  ; botón para iniciar una nueva partida con el mismo tamaño
  (new button%
       [parent ventana-derrota]
       [label "Reintentar"]
       [callback
        (lambda (b e)
          (send ventana-derrota show #f)  ; cierra la ventana de derrota
          (send ventana-juego show #f)    ; cierra la ventana del juego
          (reintentar))])                 ; abre un juego nuevo

  ; hace visible la ventana de derrota
  (send ventana-derrota show #t))

; ============================================================
; VENTANA DEL JUEGO
; ============================================================

; iniciar-juego:
; recibe filas y columnas (enteros) y al-menu (función).
; Construye toda la ventana del juego: canvas, eventos de teclado
; y el estado interno usando boxes.
; al-menu se propaga hacia procesar-jugada para poder llamarla
; cuando la partida termina.
(define (iniciar-juego filas columnas al-menu)

  ; tamaño fijo de cada casilla en píxeles
  (define TAM-CASILLA 80)

  ; espacio entre el borde de la ventana y el tablero
  (define MARGEN 10)

  ; ancho total: columnas * tamaño de casilla + márgenes laterales
  (define ANCHO (+ (* columnas TAM-CASILLA) (* 2 MARGEN)))

  ; alto total: filas * tamaño de casilla + 80px para el puntaje arriba
  (define ALTO (+ (* filas TAM-CASILLA) 80))

  ; -- Estado del juego --
  ; Se usan boxes (contenedores mutables) porque la GUI necesita
  ; modificar el estado entre eventos de teclado.
  ; Toda la lógica del juego en logica.rkt sigue siendo funcional pura.

  ; estado-tablero: lista de listas con los valores actuales del tablero
  (define estado-tablero
    (box (insertar-dos-iniciales (crear-tablero filas columnas))))

  ; estado-puntaje: puntaje acumulado durante la partida
  (define estado-puntaje (box 0))

  ; estado-juego: controla el flujo, puede ser:
  ;   'jugando  -> la partida está activa
  ;   'victoria -> el jugador formó el 2048
  ;   'derrota  -> no hay más movimientos posibles
  (define estado-juego (box 'jugando))

  ; --------------------------------------------------------
  ; FUNCIONES DE DIBUJO
  ; --------------------------------------------------------

  ; dibujar-celda:
  ; recibe el contexto de dibujo (dc), la fila, la columna y el valor.
  ; Calcula la posición en píxeles, pinta el rectángulo de fondo
  ; con el color que corresponde al valor, y si la celda no está
  ; vacía (valor > 0), escribe el número centrado dentro del rectángulo.
  (define (dibujar-celda dc fila col valor)

    ; posición horizontal: margen + columna * tamaño de casilla
    (define x (+ MARGEN (* col TAM-CASILLA)))

    ; posición vertical: 60px de offset para dejar espacio al puntaje
    (define y (+ 60 (* fila TAM-CASILLA)))

    ; configura el color de relleno del rectángulo según el valor
    (send dc set-brush (color-ficha valor) 'solid)

    ; configura el borde blanco de 2px entre casillas
    (send dc set-pen "white" 2 'solid)

    ; dibuja el rectángulo de fondo de la casilla
    (send dc draw-rectangle x y TAM-CASILLA TAM-CASILLA)

    ; solo dibuja el número si la casilla no está vacía
    (when (> valor 0)
      ; convierte el número a texto para poder dibujarlo
      (define texto (number->string valor))

      ; fuente en negrita tamaño 22
      (send dc set-font (make-object font% 22 'default 'normal 'bold))

      ; color del texto según si el fondo es claro u oscuro
      (send dc set-text-foreground (color-texto valor))

      ; get-text-extent devuelve el ancho (tw) y alto (th) del texto
      ; en píxeles, para poder centrarlo dentro de la casilla
      (define-values (tw th _ __) (send dc get-text-extent texto))

      ; dibuja el texto centrado horizontal y verticalmente en la casilla
      (send dc draw-text texto
            (+ x (round (/ (- TAM-CASILLA tw) 2)))   ; centrado horizontal
            (+ y (round (/ (- TAM-CASILLA th) 2)))))) ; centrado vertical

  ; dibujar-fila:
  ; recorre recursivamente una fila del tablero dibujando cada casilla.
  ; fila-idx: número de fila (para calcular posición vertical).
  ; col-idx: número de columna actual, se incrementa en cada llamada.
  (define (dibujar-fila dc fila fila-idx col-idx)
    (cond
      [(null? fila) (void)]  ; caso base: fila vacía, no hay nada que dibujar
      [else
       ; dibuja la casilla actual
       (dibujar-celda dc fila-idx col-idx (car fila))
       ; continúa con la siguiente casilla de la fila
       (dibujar-fila dc (cdr fila) fila-idx (+ col-idx 1))]))

  ; dibujar-tablero:
  ; recorre recursivamente todas las filas del tablero
  ; y llama a dibujar-fila para cada una.
  ; fila-idx: número de fila actual, se incrementa en cada llamada.
  (define (dibujar-tablero dc tablero fila-idx)
    (cond
      [(null? tablero) (void)]  ; caso base: tablero vacío
      [else
       ; dibuja la fila actual
       (dibujar-fila dc (car tablero) fila-idx 0)
       ; continúa con la siguiente fila
       (dibujar-tablero dc (cdr tablero) (+ fila-idx 1))]))

  ; dibujar-todo:
  ; función principal de renderizado. Se llama automáticamente
  ; cada vez que el canvas necesita redibujarse (on-paint)
  ; o manualmente después de cada jugada (refresh).
  ; Limpia la pantalla, dibuja el puntaje arriba y el tablero completo.
  (define (dibujar-todo dc)

    ; establece el color de fondo de la ventana
    (send dc set-background (make-object color% "whitesmoke"))

    ; limpia todo lo dibujado anteriormente
    (send dc clear)

    ; configura el color y fuente para el puntaje
    (send dc set-text-foreground "black")
    (send dc set-font (make-object font% 16 'default 'normal 'bold))

    ; dibuja el puntaje en la esquina superior izquierda
    (send dc draw-text
          (string-append "Puntaje: " (number->string (unbox estado-puntaje)))
          MARGEN 10)  ; posición: x=MARGEN, y=10

    ; dibuja el tablero completo empezando desde la fila 0
    (dibujar-tablero dc (unbox estado-tablero) 0))

  ; --------------------------------------------------------
  ; LÓGICA DE JUGADA
  ; --------------------------------------------------------

  ; procesar-jugada:
  ; recibe una dirección ('izquierda, 'derecha, 'arriba, 'abajo),
  ; el canvas y la ventana actual.
  ; Solo actúa si el juego está en estado 'jugando.
  ; Llama a aplicar-jugada-con-puntos de logica.rkt, que devuelve
  ; (list nuevo-tablero nuevo-puntaje), y actualiza los boxes.
  ; Luego verifica si la partida terminó:
  ;   - victoria: cierra la ventana del juego y abre mostrar-victoria
  ;   - derrota:  cierra la ventana del juego y abre mostrar-derrota
  ;   - sigue:    solo redibuja el canvas
  (define (procesar-jugada direccion canvas ventana)
    (when (equal? (unbox estado-juego) 'jugando)  ; solo procesa si está jugando

      ; aplica el movimiento y obtiene (nuevo-tablero nuevo-puntaje)
      (define resultado
        (aplicar-jugada-con-puntos (unbox estado-tablero)
                                   direccion
                                   (unbox estado-puntaje)))

      ; actualiza el tablero con el resultado del movimiento
      (set-box! estado-tablero (car resultado))

      ; actualiza el puntaje sumando los puntos de esta jugada
      (set-box! estado-puntaje (cadr resultado))

      (cond
        ; si alguna casilla llegó a 2048, el jugador ganó
        [(victoria? (unbox estado-tablero))
         (set-box! estado-juego 'victoria)  ; marca el juego como terminado
         (send canvas refresh)              ; redibuja el estado final
         ; pasa ventana para que el botón del menú pueda cerrarla
         (mostrar-victoria (unbox estado-puntaje) ventana al-menu)]

        ; si no quedan movimientos posibles, el jugador perdió
        [(derrota? (unbox estado-tablero))
         (set-box! estado-juego 'derrota)   ; marca el juego como terminado
         (send canvas refresh)              ; redibuja el estado final
         ; pasa ventana para que los botones puedan cerrarla
         (mostrar-derrota
          (unbox estado-puntaje)
          ventana
          al-menu
          ; reintentar es un lambda que crea una partida nueva
          ; con el mismo tamaño sin pasar por el menú de selección
          (lambda () (iniciar-juego filas columnas al-menu)))]

        ; si el juego sigue activo, solo redibuja el tablero
        [else
         (send canvas refresh)])))

  ; --------------------------------------------------------
  ; CONSTRUCCIÓN DE LA VENTANA
  ; --------------------------------------------------------

  ; crea la ventana principal del juego con el tamaño calculado
  (define ventana
    (new frame%
         [label "2048"]   ; título de la ventana
         [width ANCHO]    ; ancho calculado según columnas
         [height ALTO]))  ; alto calculado según filas

  ; crea el canvas personalizado heredando de canvas%:
  ; - on-char: se ejecuta cada vez que el usuario presiona una tecla
  ; - on-paint: se ejecuta cada vez que el sistema necesita redibujar
  (define canvas
    (new (class canvas%
           (super-new)  ; llama al constructor de la clase padre canvas%

           ; on-char: captura eventos de teclado
           ; send event get-key-code devuelve un símbolo como 'left, 'right, etc.
           (define/override (on-char evento)
             (define tecla (send evento get-key-code))
             (cond
               [(equal? tecla 'left)  (procesar-jugada 'izquierda this ventana)]
               [(equal? tecla 'right) (procesar-jugada 'derecha  this ventana)]
               [(equal? tecla 'up)    (procesar-jugada 'arriba   this ventana)]
               [(equal? tecla 'down)  (procesar-jugada 'abajo    this ventana)])
             #t)  ; devuelve #t para indicar que el evento fue manejado

           ; on-paint: redibuja el canvas completo
           ; send this get-dc obtiene el contexto de dibujo del canvas
           (define/override (on-paint)
             (dibujar-todo (send this get-dc))))
         [parent ventana]))  ; el canvas pertenece a la ventana del juego

  ; da el foco al canvas para que pueda recibir eventos de teclado
  ; sin necesidad de hacer click primero
  (send canvas focus)

  ; hace visible la ventana del juego
  (send ventana show #t))

; ============================================================
; VENTANA DEL MENÚ PRINCIPAL
; ============================================================

; iniciar-interfaz:
; punto de entrada de todo el programa, llamada desde main.rkt.
; Muestra el menú de selección de tamaño del tablero.
; Se define como función (no como código suelto) para poder pasarse
; a sí misma como parámetro al-menu a iniciar-juego, permitiendo
; que las pantallas de victoria y derrota puedan regresar aquí.
(define (iniciar-interfaz)

  ; crea la ventana del menú de selección
  (define menu
    (new frame%
         [label "2048 - Seleccionar tamaño"]
         [width 300]
         [height 220]))

  ; instrucción al usuario sobre los valores válidos
  (new message%
       [parent menu]
       [label "Ingrese el tamaño del tablero (entre 4 y 10):"])

  ; etiqueta para el campo de filas
  (new message% [parent menu] [label "Filas:"])

  ; campo de texto para ingresar la cantidad de filas
  ; init-value "4" muestra 4 como valor por defecto
  (define campo-filas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  ; etiqueta para el campo de columnas
  (new message% [parent menu] [label "Columnas:"])

  ; campo de texto para ingresar la cantidad de columnas
  (define campo-columnas
    (new text-field%
         [parent menu]
         [label ""]
         [init-value "4"]))

  ; botón "Jugar": lee los campos, valida y arranca el juego
  (new button%
       [parent menu]
       [label "Jugar"]
       [callback
        (lambda (boton evento)
          ; string->number convierte el texto a número,
          ; devuelve #f si el texto no es un número válido
          (define filas   (string->number (send campo-filas get-value)))
          (define columnas (string->number (send campo-columnas get-value)))
          (cond
            ; valida que ambos valores sean números enteros entre 4 y 10
            [(or (not filas) (not columnas)   ; no son números
                 (< filas 4) (> filas 10)     ; fuera del rango de filas
                 (< columnas 4) (> columnas 10)) ; fuera del rango de columnas
             ; muestra un cuadro de error sin cerrar el menú
             (message-box "Error"
                          "Ingrese números válidos entre 4 y 10."
                          menu)]
            [else
             (send menu show #f)  ; cierra la ventana del menú
             ; abre el juego pasando iniciar-interfaz como al-menu
             ; para poder regresar aquí desde victoria o derrota
             (iniciar-juego filas columnas iniciar-interfaz)]))])

  ; hace visible la ventana del menú
  (send menu show #t))