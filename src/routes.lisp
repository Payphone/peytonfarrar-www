(defpackage #:peytonwww.routes
  (:use #:cl #:peytonwww.config)
  (:export #:*app*))

(in-package :peytonwww.routes)

(defvar *app* (make-instance 'ningle:<app>))

;; Common Lisp Recipes by Edmund Weiz
(defun file-at-once (filespec &rest open-args)
  "Reads an entire file into a string."
  (with-open-stream (stream (apply #'open filespec open-args))
    (let* ((buffer (make-array (file-length stream)
                               :element-type (stream-element-type stream)
                               :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))

(defun param-get (param params)
  (cdr (assoc param params)))

(defun render (filename &optional (directory *page-directory*))
  "Reads a file from the page directory."
  (file-at-once (merge-pathnames filename directory)))

(defmacro route (args &body body)
  `(setf (ningle:route *app* ,@args)
         ,@body))

(route ("/")
  (render #P"index.html"))

(route ("/docs")
  (render #P"docs.html"))

(route ("/docs/:document")
  #'(lambda (params)
      (render (param-get :document params) *doc-directory*)))
