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

;;
;; Blog post functions

; Example post: 2015-06-27:I've created a Monster
; Note that the date is a 10 character string and the colon is at charater 11
(defun post-extract-date (file-name)
  (subseq file-name 0 10))

(defun post-extract-subject (file-name)
  (subseq file-name 11))

(defun post-p (file-name)
  (if (equal (subseq file-name 10 11) ":")
    t
    nil))

(defun generate-post-list (post-directory)
  (let ((post-list nil))
    (dolist (p post-directory)
      (let ((post (pathname-name p)))
        (when (post-p post)
          (push (list :subject (post-extract-subject post)
                      :date (post-extract-date post)
                      :url (format nil "/blog/posts/~A.html" post))
                post-list))))
    (list :posts post-list)))

(defun format-post (str post-file)
  (format str "{% extends \"layouts/default.html\" %} ~%
               {% block title %}~A{% endblock %} ~% 
               {% block content %} ~%
			   <div class='content'> ~%" 
	          (post-extract-subject (pathname-name post-file)))
  (cl-markdown:markdown post-file :stream str)
  (format str "</div> ~%
               {% endblock %} ~%"))

(defun markdown-to-html (markdown-list)
  (if (eq markdown-list nil) 
	nil
	(let ((markdown-file (first markdown-list)))
		(with-open-file (md (make-pathname :directory (pathname-directory markdown-file)
										   :name (pathname-name markdown-file)
										   :type "html")
						    :direction :output
						    :if-exists :supersede
							:if-does-not-exist :create)
		  (format-post md markdown-file))
		(delete-file markdown-file)
	    (markdown-to-html (cdr markdown-list)))))

;;
;; Routing rules

(defroute "/" ()
  (render (first (reverse (absolute-directory "blog/posts/*.html")))))

(defroute "/blog" ()
  (unless (eq (absolute-directory "blog/posts/*.md") nil)
	(markdown-to-html (absolute-directory "blog/posts/*.md")))
  (render (absolute-path "blog/index.html")
		  (generate-post-list (absolute-directory "blog/posts/*.html"))))

(defroute "/blog/posts/*.html" (&key splat)
  (let* ((post-name (first splat))
         (post-path (absolute-path (format nil "blog/posts/~A.html" post-name))))
        (if (find post-path (absolute-directory "blog/posts/*.html") :test #'equal)
          (render post-path)
          (render #P"_errors/404.html"))))

(defroute "/jazz" ()
  (let ((images (directory (merge-pathnames "static/images/Night/*.*" *application-root*)))
		(songs (directory (merge-pathnames "static/music/Jazz/*.ogg" *application-root*))))
	(render (absolute-path "jazz.html")
  			(list :image (enough-namestring (nth (random (list-length images)) images)
											*static-directory*)
				  :song (enough-namestring (nth (random (list-length songs)) songs)
										    *static-directory*)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (absolute-path "_errors/404.html"))
