(import chicken scheme)
(use gl-type glls-render (prefix glfw3 glfw:)
     (prefix opengl-glew gl:) gl-math gl-utils)

;;; Matrices
(define projection-matrix
  (ortho 512 512 1 100))

(define model-matrix (translation (make-point -230 240 -2)))

(define mvp (m* projection-matrix
                model-matrix
                #t ; Matrix should be in a non-GC'd area
                ))

;;; Pipeline definition
(define-pipeline text-shader
  ((#:vertex input: ((position #:vec2) (tex-coord #:vec2))
             uniform: ((mvp #:mat4))
             output: ((tex-c #:vec2))) 
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 0.0 1.0)))
     (set! tex-c tex-coord)))
  ((#:fragment input: ((tex-c #:vec2))
               uniform: ((tex #:sampler-2d))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (let ((r #:float (field (texture tex tex-c) r)))
       (set! frag-color (vec4 0 0 0 r))))))

;; Text
(define lorem-ipsum  "Lorem ipsum dolor sit amet, soluta eleifend euripidis usu ut, eros deserunt no has. Ad sed vocent persequeris intellegebat, et eum expetenda periculis adipiscing. Sed ad quis accusata. Omnes sanctus veritus vis ei, inani molestiae contentiones id pri.

Cum ut quaestio temporibus. Cum meis harum nemore ea. Ad ius nusquam efficiendi. Eirmod aperiri legendos te sea. Et pro illud fabulas. Ut sed integre nominati, splendide omittantur est te, wisi imperdiet his ut. Vel appareat mandamus disputando in.

At tale ipsum ius. Harum putent theophrastus ad pro, semper debitis prodesset his et. Nostrud iuvaret verterem no mei, te vis illud adolescens. Cu alterum partiendo vel, quo ei quis voluptatum, usu consul iisque denique id. Mei ut mucius ceteros conclusionemque.

Semper commune id ius, purto augue posidonium ei cum. Id facer delectus iracundia vix, in consul corpora elaboraret sed. Ne sea torquatos definitiones. Sit ut quas omnesque, eos ea erant sonet voluptaria, labitur copiosae splendide cum ne.")

;;; Initialization and main loop
(glfw:with-window (512 512 "Example" resizable: #f)
  (gl:init)
  (gl:clear-color 1 1 1 1)
  (gl:enable gl:+depth-test+)
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (compile-pipelines)
  (let* ((face (load-face "/usr/share/fonts/truetype/msttcorefonts/arial.ttf" 20))
         (test-string (string-mesh lorem-ipsum face
                                   line-spacing: 0.5
                                   max-width: 460)))
    (mesh-make-vao! test-string (pipeline-mesh-attributes text-shader))
    (let ((renderable (make-text-shader-renderable
                      mesh: test-string
                      tex: (face-atlas face)
                      mvp: mvp)))
      (let loop ()
        (glfw:swap-buffers (glfw:window))
        (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
        (render-text-shader renderable)
        (check-error)
        (glfw:poll-events)
        (unless (glfw:window-should-close (glfw:window))
          (loop))))))

