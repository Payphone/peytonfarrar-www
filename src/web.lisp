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

;;
;; Blog post functions

(defun latest-post ()
  (with-connection (db)
    (retrieve-one
      (select :*
        (from :posts)
        (order-by (:desc :id))))))

(defun post-by-id (id)
  (with-connection (db)
    (retrieve-one
      (select :*
        (from :posts)
        (where (:= :id id))))))

(defun posts-by-limit (post-limit)
  (with-connection (db)
    (retrieve-all
      (select :*
        (from :posts)
        (order-by (:desc :id))
        (limit post-limit)))))

(defun posts-by-tag (tag)
  (with-connection (db)
    (retrieve-all
      (select :*
        (from :posts)
        (where (:raw (format nil "tags similar to '%((~A))%'" tag)))))))

(defun render-post (post)
  (render (absolute-path "post.html")
          post))

;;
;; Routing rules

(defroute "/" ()
  (render-post (latest-post)))

(defroute ("/blog/page/([\\d]+)" :regexp :t) (&key captures)
  (render (absolute-path "blog_index.html")
          (posts-by-limit 10)))

(defroute ("/blog/post/([\\d]+)" :regexp t) (&key captures)
  (let ((id (parse-integer (first captures))))
    (if (eq (post-by-id id) nil)
      (render #P"_errors/404.html")
      (render-post (post-by-id (first captures))))))

(defroute "/jazz" ()
  (let ((images (root-directory "static/images/Night/*.jpg"))
		(songs (root-directory "static/music/Jazz/*.ogg")))
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
