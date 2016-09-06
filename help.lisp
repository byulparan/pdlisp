(defclass foo (pdlisp)
  ((inlets :initform 2 :reader inlets)
   (outlets :initform 2 :reader outlets)
   (receiver :initform nil :accessor receiver)))

(defmethod initialize ((foo foo) &rest rest)
  (setf (receiver foo) (receiver-new foo "recv"))
  (with-pd-stream nil
    (format t "Hello. You must make beautiful music :-)")))

(defmethod finalize ((foo foo))
  (when (receiver foo)
    (receiver-free (receiver foo))))

(defmethod bang-1 ((foo foo))
  (outlet 1 0))

(defmethod float-1 ((foo foo) x)
  (outlet 1 (+ x x)))

(defmethod float-2 ((foo foo) x)
  (send "obj" (* x 3)))

(defmethod symbol-1 ((foo foo) symb)
  (outlet 2 (format nil "are you ~a?" symb)))

(defmethod list-1 ((foo foo) list)
  (outlet 1 (mapcar #'(lambda (x) (* x 100)) list)))

(defmethod msg-1 ((foo foo) list)
  (outlet 1 (cons "rerun" (mapcar #'1+ list))))


(defmethod recv-float ((foo foo) x)
  (send "obj" (+ x 2)))


