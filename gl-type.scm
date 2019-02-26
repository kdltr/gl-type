(module gl-type
  (load-face
   face?
   face-atlas
   face-char-set
   face-height
   string-mesh
   string-width
   pixel-density-ratio)

(import
  scheme
  (chicken base)
  (chicken bitwise)
  (chicken gc)
  (chicken memory)
  (chicken sort)
  (chicken string)
  srfi-1
  srfi-4
  srfi-13
  srfi-14
  miscmacros
  freetype
  (prefix epoxy #:gl)
  gl-utils)

(define lib (ft-init-freetype))

(define-record face
  atlas char-set glyphs height ascender space-advance atlas-width atlas-height)

(define-record glyph
  offset-x offset-y width height advance bearing-x bearing-y)

(define-parameter pixel-density-ratio 1)

(define (pixel-size x) (arithmetic-shift x -6))

(define (next-power-of-two n)
  (inexact->exact (expt 2 (ceiling (/ (log n)
                                      (log 2))))))

;; Used for create-glyph bin-packing
(define row-height (make-parameter #f))
(define pen-x (make-parameter 0))
(define pen-y (make-parameter 0))

;; Easy bin packing: sort glyphs by decreasing height, each row of glyphs uses the height of the first glyph of that row
(define (create-glyph char face mode texture-data tex-width tex-height)
  (ft-load-char face (char->integer char) FT_LOAD_DEFAULT)
  (ft-render-glyph (ft-face-glyph face) (if (eq? mode mono:)
                                            FT_RENDER_MODE_MONO
                                            FT_RENDER_MODE_NORMAL))
  (let* ((glyph (ft-face-glyph face))
         (metrics (ft-glyph-slot-metrics glyph))
         (bitmap (ft-glyph-slot-bitmap glyph))
         (width (ft-bitmap-width bitmap))
         (rows (ft-bitmap-rows bitmap))
         (buf (ft-bitmap-buffer bitmap)))
    (unless (row-height)
      (row-height rows))
    (when (>= (+ (pen-x) width) tex-width)
      (pen-x 0)
      (pen-y (+ (pen-y) (row-height) 1))
      (row-height rows))
    (dotimes (i rows)
      (dotimes (j width)
        (u8vector-set! texture-data (+ j (pen-x)
                                       (* (+ i (pen-y))
                                          tex-width))
                       (pointer-u8-ref
                        (pointer+ buf (+ j (* i width)))))))
    (begin0
        (cons char
              (make-glyph
               (/ (pen-x) tex-width) (/ (pen-y) tex-height)
               width rows
               (pixel-size (ft-glyph-metrics-hori-advance metrics))
               (pixel-size (ft-glyph-metrics-hori-bearing-x metrics))
               (pixel-size (ft-glyph-metrics-hori-bearing-y metrics))))
      (pen-x (+ (pen-x) width 1)))))

(define (load-face path size #!key (char-set char-set:graphic) (mode normal:))
  (define face (ft-new-face lib path))
  (unless face
    (error 'load-face "File not found or loadable" path))
  (ft-set-pixel-sizes face 0 (* size (pixel-density-ratio)))
  (pen-x 0)
  (pen-y 0)
  (row-height #f)
  (let* ((dimensions (map (lambda (char)
                            (ft-load-char face (char->integer char) FT_LOAD_DEFAULT)
                            (let ((metrics (ft-glyph-slot-metrics
                                            (ft-face-glyph face))))
                              (list char
                                    (pixel-size (ft-glyph-metrics-width metrics))
                                    (pixel-size (ft-glyph-metrics-height metrics)))))
                          (char-set->list char-set)))
         (dimensions (sort dimensions (lambda (a b)
                                        (> (third a) (third b)))))
         (max-height (third (car dimensions)))
         (tex-width (next-power-of-two (sqrt (* (fold (lambda (a b)
                                                     (+ (second a) b))
                                                   0 dimensions)
                                             max-height))))
         (tex-height (let loop ((chars dimensions) (x 0) (y (+ 2 max-height)))
                       (if (null? chars)
                           y
                           (let ((char-width (second (car chars)))
                                 (char-height (third (car chars))))
                             (if (> (+ x char-width) tex-width)
                                 (loop chars 0 (+ y char-height 1))
                                 (loop (cdr chars) (+ x char-width 1) y))))))
         (face-height (pixel-size (ft-face-height face)))
         (space-width (begin (ft-load-char face (char->integer #\space)
                                           FT_LOAD_DEFAULT)
                             (pixel-size (ft-glyph-metrics-hori-advance
                                          (ft-glyph-slot-metrics
                                           (ft-face-glyph face))))))
         (texture-data (make-u8vector (* tex-width tex-height) #f #t))
         (gl-tex (gen-texture))
         (glyphs (map (lambda (glyph)
                        (create-glyph (car glyph) face mode texture-data
                                      tex-width tex-height))
                      dimensions))
         (ascender (fold (lambda (glyph x)
                           (max (glyph-bearing-y (cdr glyph))
                                x))
                         0 glyphs)))
    (with-texture gl:+texture-2d+ gl-tex
      (set-texture-properties #f
                              mag: gl:+nearest+
                              min: gl:+nearest+
                              wrap: gl:+clamp-to-edge+)
      (gl:tex-image-2d gl:+texture-2d+ 0 gl:+red+ tex-width tex-height 0 gl:+red+
                       gl:+unsigned-byte+ (->pointer texture-data)))
    (free-ft-face face)
    (set-finalizer! (make-face gl-tex char-set glyphs face-height ascender
                               space-width tex-width tex-height)
                    (lambda (face) (delete-texture (face-atlas face))))))

(define (string-mesh string face #!key (line-spacing 1.3) max-width (x 0) (y 0) mesh)
  (define w-scale (/ (face-atlas-width face)))
  (define h-scale (/ (face-atlas-height face)))
  (define pixel-scale (/ (pixel-density-ratio)))
  (let* ((glyphs (map (lambda (char)
                            (if* (alist-ref char (face-glyphs face))
                                 it
                                 char))
                      (string->list (if max-width
                                        (word-wrap string face max-width)
                                        string))))
         (pen-x x)
         (pen-y y)
         (ascender (face-ascender face))
         (line-height (* ascender line-spacing))
         (space-advance (face-space-advance face))
         (newline (lambda ()
                    (set! pen-x x)
                    (set! pen-y (- pen-y line-height))))
         (position (append-map (lambda (glyph)
                                 (if (char? glyph)
                                     (cond
                                      ((char=? glyph #\space)
                                       (set! pen-x (+ space-advance pen-x))
                                       '())
                                      ((char=? glyph #\newline)
                                       (newline)
                                       '())
                                      (else '()))
                                     (let* ((w (glyph-width glyph))
                                            (h (glyph-height glyph))
                                            (x (+ pen-x (glyph-bearing-x glyph)))
                                            (y (+ pen-y (- ascender)
                                                  (glyph-bearing-y glyph)))
                                            (x2 (+ x w))
                                            (y2 (- y h)))
                                       (begin0
                                           (map (lambda (n)
                                                  (* n pixel-scale))
                                                (list x y2
                                                      x2 y2
                                                      x2 y
                                                      x y))
                                         (set! pen-x (+ (glyph-advance glyph)
                                                        pen-x))))))
                               glyphs))
         (tex-coord (append-map (lambda (glyph)
                                  (if (char? glyph)
                                      '()
                                      (let* ((x (glyph-offset-x glyph))
                                             (y (glyph-offset-y glyph))
                                             (w (* w-scale (glyph-width glyph)))
                                             (h (* h-scale (glyph-height glyph)))
                                             (x2 (+ x w))
                                             (y2 (+ y h)))
                                        (list x y2
                                              x2 y2
                                              x2 y
                                              x y))))
                                glyphs))
         (n-characters (string-length (string-delete char-set:whitespace string)))
         (indices (append-map (lambda (x)
                                (let ((x (* x 4)))
                                  (list x (+ 1 x) (+ 2 x)
                                        x (+ 2 x) (+ 3 x))))
                             (iota n-characters)))
         (vertices `((position . ,position)
                     (tex-coord . ,tex-coord))))
    (if mesh
        (mesh-update! mesh vertices indices)
        (make-mesh vertices: `(attributes: ((position #:float 2)
                                            (tex-coord #:unsigned-short 2
                                                       normalized: #t))
                               initial-elements: ,vertices)
                indices: `(type: #:ushort
                           initial-elements: ,indices)))))

(define (word-wrap string face max-width)
  (define space-advance (face-space-advance face))
  (define (wrap-line string)
    (if (equal? string "")
        '("")
        (let loop ((words (string-tokenize string)) (x 0) (line '()))
          (if (null? words)
              (list (string-join (reverse line) " "))
              (let ((width (string-width (car words) face)))
                (cond
                 ((and (zero? x) (>= width max-width))
                  (cons (car words) (loop (cdr words) 0 '())))
                 ((> (+ x width) max-width)
                  (cons (string-join (reverse line) " ")
                        (loop words 0 '())))
                 (else
                  (loop (cdr words) (+ x width space-advance)
                        (cons (car words) line)))))))))
  (string-join (append-map wrap-line (string-split string "\n" #t))
               "\n"))

(define (string-width string face)
  (let ((glyphs (map (lambda (char)
                       (if* (alist-ref char (face-glyphs face))
                            it
                            char))
                     (string->list string)))
        (space-advance (face-space-advance face)))
    (/ (fold (lambda (glyph w)
               (+ (if (char? glyph)
                      (cond
                       ((char=? glyph #\space)
                        space-advance)
                       (else 0))
                      (glyph-advance glyph))
                  w))
             0 glyphs)
       (pixel-density-ratio))))

) ; end module gl-type
