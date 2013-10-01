(make-package :pd)

(defpackage #:pd-core
  (:use #:cl)
  (:export #:pdlisp
	   #:with-pd-stream
	   #:inlets
	   #:outlets
	   #:initialize
	   #:finalize
	   #:outlet
	   #:send
	   #:receiver-new
	   #:receiver-free
	   #:timer-new
	   #:timer-free
	   #:timer-set
	   #:timer-delay
	   #:timer-unset
	   #:timer-bang))

(in-package :pd-core)

;; ignore output stream.....
;; GUI 기반의 제어터미널이 없는 환경에서는 반드시 I/O 스트림을 닫아야함(only ECL)...

(defparameter *string-stream* (make-string-output-stream))
(setf *error-output* *string-stream*)
(setf *terminal-io*
      (make-two-way-stream (make-string-input-stream "") *string-stream*))

(setf *debug-io* *terminal-io*)
(setf *standard-output* *string-stream*)



;;; prologue
(pd_post "-------------------------------------")
(pd_post "ECL (Embeddable Common-Lisp) 13.5.1") 
(pd_post "Copyright (C) 1984 Taiichi Yuasa and Masami Hagiya") 
(pd_post "Copyright (C) 1993 Giuseppe Attardi") 
(pd_post "Copyright (C) 2000 Juan J. Garcia-Ripoll") 
(pd_post "ECL is free software, and you are welcome to redistribute it") 
(pd_post "under certain conditions")
(pd_post "")
(pd_post "<PDLISP> Lisp code in PureData")
(pd_post "2013.9.7 made by parksungmin")
(pd_post "-------------------------------------")


;;; 유틸리티 함수
(defmacro pd-post (fmt &rest strings)
  `(pd_post (format nil ,fmt  ,@strings)))

(defmacro pd-error (fmt &rest strings)
  `(pd_error (format nil ,fmt ,@strings)))

(defmacro with-pd-stream ((&key dummy)&body body)
  (let ((stream (gensym))
	(ret (gensym)))
    `(let* ((,stream (make-string-output-stream))
	    (*standard-output* ,stream))
       ,@body
       (let ((,ret (get-output-stream-string ,stream)))
	 (unless (zerop (length ,ret))
	   (pd-post "~a" ,ret))))))



;;; lisp 파일 로딩
(defun pd-load (file cur-dir)
  (handler-case (progn
		  (unless (find (aref file 0) (list #\/ #\~))
		      (setf file (format nil "~a/~a" cur-dir file)))
		  (load file)
		  0)
    (error (e) (pd-error "fail file loading:~a ~a" file e) -1)))


;;; 새로 생성된 객체 리턴....
(defparameter *new-object* nil)

(defun get-new-object ()
  *new-object*)








(defclass pdlisp ()
  ((inlets :initform 1 :reader inlets)
   (outlets :initform 1 :reader outlets)
   (pd-ptr :reader pd-ptr :writer set-pd-ptr)))


(defvar *self*)

(defmethod initialize ((pdlisp pdlisp) &rest rest)
  ())

(defun make-object (name rest)
  (let ((obj-name (intern (string-upcase name) :pd)))
    (handler-case (let* ((object (make-instance (find-class obj-name)))
			 (*self* object))
		    (apply #'initialize object rest)
		    (setf *new-object* object)
		    0)
      (error (e) (pd-error "make-object fail! ~a" e) -1))))

(defun wrap-finalize (pdlisp)
  (handler-case (let ((*self* pdlisp)) (finalize pdlisp))
    (error (e) (pd-error "error in finalizing: ~a" e))))

(defmethod finalize ((pdlisp pdlisp))
  ())



(defun wrap-outlet (outlet msg len value)
  (when (and (> outlet 0) (>= (outlets *self*)  outlet)) 
    (pd-outlet-anything *self* (floor outlet) msg len value)))

(defmethod outlet (outlet (v (eql 'bang)))
  (wrap-outlet outlet "bang" 0 nil))

(defmethod outlet (outlet (v number))
  (wrap-outlet outlet "float" 1 (list v)))

(defmethod outlet (outlet (v string))
  (wrap-outlet outlet "symbol" 1 (list v)))

(defmethod outlet (outlet (v list))
  (if (numberp (car v)) (wrap-outlet outlet "list" (length v) v)
      (wrap-outlet outlet (car v) (length (cdr v)) (cdr v))))



(defmethod send ((receiver string) (v (eql 'bang)))
  (pd-send-anything receiver "bang" 0 nil))

(defmethod send ((receiver string) (v number))
  (pd-send-anything receiver "float" 1 (list v)))

(defmethod send ((receiver string) (v string))
  (pd-send-anything receiver "symbol" 1 (list v)))

(defmethod send ((receiver string) (v list))
  (if (numberp (car v)) (pd-send-anything receiver "list" (length v) v)
      (pd-send-anything receiver (car v) (length (cdr v)) (cdr v))))



(defun pd-callback (typename name obj args)
  (handler-case
      (let ((*self* obj))
	(cond ((string= typename "bang") (funcall (symbol-function name) obj))
	      ((or (string= typename "float") (string= typename "symbol"))
	       (funcall (symbol-function name) obj (car args)))
	      (t (funcall (symbol-function name) obj args))))
    (undefined-function (e) (pd-error "undefined-function: ~a from ~a/~a" e obj name))
    (error (e) (pd-error "error: ~a from ~a/~a" e obj name))))

(defun inlet-callback (obj inlets typename args)
  (let ((name (intern (string-upcase (format nil "~a-~d" typename (+ 1 inlets))) :pd)))
    (pd-callback typename name obj args)))

(defun receive-callback (obj id typename args)
  (let ((name (intern (string-upcase (format nil "~a-~a" id typename)) :pd)))
    (pd-callback typename name obj args)))

(defun clock-callback (obj)
  (handler-case (let ((*self* obj))
		  (timer-bang obj))
    (error (e) (pd-error "clock error: ~a from a" e obj))))


(defpackage #:pd
  (:use #:cl #:pd-core))

(in-package #:pd)

(defmethod eval-1 ((pdlips pdlisp) form)
  (with-pd-stream nil
    (eval (car (read-from-string (format nil "~@{~x~}" form))))))
