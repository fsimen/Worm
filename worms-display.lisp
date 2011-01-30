;;; glut-template.lisp
;;#<:use "load opengl">


(defclass my-window (glut:window)
  ()
  (:default-initargs
   :width 400 :height 150 :pos-x 100 :pos-y 100
   :mode '(:single :rgb) :title "lines.lisp"))

(defmethod glut:display-window :before ((w my-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back))

(defmethod glut:keyboard ((w my-window) key x y)
  (declare (ignore x y))
  (flet ((update (fn)
           (setf *vierme* (deplasare *vierme* fn))
           (glut:post-redisplay)))
    (case key
      (#\Esc (glut:destroy-current-window))
      (#\8 (update 'jos))
      (#\2 (update 'sus))
      (#\4 (update 'stanga))
      (#\6 (update 'dreapta)))))


(defun draw-one-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defmethod glut:display ((w my-window))
  (gl:clear :color-buffer)
  ;; Select white for all lines.
  (gl:color 1 1 1)
  (deseneaza *vierme*)
  (gl:flush))

(defmethod glut:reshape ((w my-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height))

(defun my_plot ()
  (glut:display-window (make-instance 'my-window)))

(defun deseneaza (vierme)
  (dolist (x vierme) 
    (destructuring-bind ((x1 y1) (x2 y2)) x
      (draw-one-line x1 y1 x2 y2))))
