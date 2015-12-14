(in-package :cl-user)
(defpackage peytonwww.web 
  (:use :cl
        :caveman2
        :peytonwww.config
        :peytonwww.view
        :peytonwww.db 
        :datafly
        :sxql)
  (:export :*web*))
(in-package :peytonwww.web)

;;
;; Application

(defclass <web> (<app>) ()) 
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Helper funtions

(defun absolute-path (file-path)
  (merge-pathnames file-path *template-directory*))

(defun absolute-directory (directory-path)
  (directory (absolute-path directory-path))) 

(defun root-path (file-path)
  (merge-pathnames file-path *application-root*))

(defun root-directory (directory-path)
  (directory (root-path directory-path)))

(defun random-file (files)
  (nth (random (list-length files)) files))

(defmacro with-group (group &body body)
  `(if (search ,group (gethash :groups *session*))
       (progn ,@body)
       (throw-code 403)))

(defmacro with-item (item &body body)
  `(if ,item
       (progn ,@body)
       (throw-code 404)))

;;
;; Routing rules

(defroute "/" ()
  (render (absolute-path "index.html")))

(defroute "/jazz" ()
  (let ((images (root-directory "static/images/Night/*.jpg"))
        (songs (root-directory "static/music/Jazz/*.ogg")))
    (render (absolute-path "jazz.html")
            (list :image (enough-namestring 
                          (random-file images)
                          *static-directory*)
                  :song (enough-namestring 
                         (random-file songs)
                         *static-directory*)))))
;;
;; Error pages

(defparameter *http-error*
  '((404 . "Not Found")
    (403 . "Insufficient Permissions")))

(defun error-reason (error-code)
  (cdr (assoc error-code *http-error*)))

(defmethod on-exception ((app <web>) error-code)
  (declare (ignore app))
  (render (absolute-path (format nil "error.html"))
          (list :error-code error-code :error-message (error-reason error-code))))
