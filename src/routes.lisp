(defpackage #:peytonwww.routes
  (:use #:cl
        #:caveman2
        #:peytonwww.config
        #:peytonwww.view)
  (:export :*app*))
(in-package :peytonwww.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *app* (make-instance '<web>))
(clear-routing-rules *app*)

;;
;; General routing rules

(defroute "/" ()
  (render "index.html"))

;; Error pages

(let ((http-error
       '((404 . "Not Found")
         (403 . "Insufficient Permissions"))))
  (defun error-reason (error-code)
    (cdr (assoc error-code http-error))))

(defmethod on-exception ((app <web>) error-code)
  (declare (ignore app))
  (render "error.html"
          (list :error-code error-code :error-message (error-reason error-code))))
