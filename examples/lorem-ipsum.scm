;; lorem-ipsum.scm

;; Renders Lorem Ipsum. Run with csi or compile with csc.
;; If /usr/share/fonts/truetype/msttcorefonts/arial.ttf is not on your system, substitute with a font that is.

;;;; NOTE:
;;;; This uses glls-render, so if this file is compiled it must be linked with libepoxy
;;;; E.g.:
;;;; csc -L -lepoxy lorem-ipsum.scm

(import
  scheme
  gl-type
  glls-render
  (prefix glfw3 glfw:)
  (prefix epoxy gl:)
  gl-math
  gl-utils)

;;(pixel-density-ratio 2) ; Un-comment for high density displays

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

At tale ipsum ius. Harum putent theophrastus ad pro, semper debitis prodesset his et. Nostrud iuvaret verterem no mei, te vis illud adolescens. Cu alterum partiendo vel, quo ei quis voluptatum, usu consul iisque denique id. Mei ut mucius ceteros conclusionemque.")

;;; Initialization and main loop
(glfw:with-window (512 512 "Lorem Ipsum" resizable: #f
                   client-api: glfw:+opengl-api+
                   context-version-major: 3
                   context-version-minor: 3)
  (gl:clear-color 1 1 1 1)
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (compile-pipelines)
  (let* ((face (load-face "/usr/share/fonts/truetype/msttcorefonts/arial.ttf" 20))
         (test-string (string-mesh lorem-ipsum face
                                   max-width: 460)))
    (mesh-make-vao! test-string (pipeline-mesh-attributes text-shader))
    (let ((renderable (make-text-shader-renderable
                      mesh: test-string
                      tex: (face-atlas face)
                      mvp: mvp)))
      (let loop ()
        (glfw:swap-buffers (glfw:window))
        (gl:clear gl:+color-buffer-bit+ )
        (render-text-shader renderable)
        (check-error)
        (glfw:poll-events)
        (unless (glfw:window-should-close (glfw:window))
          (loop))))))
