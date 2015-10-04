(in-package :cl-user)
(defpackage peytonwww.web
  (:use :cl
        :caveman2
        :peytonwww.config
        :peytonwww.view
        :peytonwww.db
        :datafly
        :sxql
        :cl-who)
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

; Blog post helper functions
; Example post: 2015-06-27:I've created a Monster
; Note that the date is a 10 character string and the colon is at charater 11
(defun post-extract-date (post-file-name)
  (subseq post-file-name 0 10))

(defun post-extract-subject (post-file-name)
  (subseq post-file-name 11))

(defun post-p (file-name)
  (if (equal (subseq file-name 10 11) ":")
    t
    nil))

(defun generate-post (post-directory)
  (let ((post-list nil))
    (dolist (p post-directory)
      (let ((post (pathname-name p)))
        (when (post-p post)
          (push (list :subject (post-extract-subject post)
                      :date (post-extract-date post)
                      :url (format nil "/blog/posts/~A.html" post))
                post-list))))
    (list :posts post-list)))
    
;;
;; Routing rules

(defroute "/" ()
  (render (first (absolute-directory "blog/posts/*.html"))))

(defroute "/blog" ()
          (render (absolute-path "blog/index.html")
                  (generate-post (absolute-directory "blog/posts/*.html"))))

(defroute "/blog/posts/*.html" (&key splat)
  (let* ((post-name (first splat))
         (post-path (absolute-path (format nil "blog/posts/~A.html" post-name))))
        (if (find post-path (absolute-directory "blog/posts/*.html") :test #'equal)
          (render post-path)
          (render #P"_errors/404.html"))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (absolute-path "_errors/404.html"))
