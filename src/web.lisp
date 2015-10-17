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

(defstruct post
     id
     subject
     date
     content
     tags)

(defun latest-post ()
  (with-connection (db)
    (retrieve-one
      (select :*
        (from :posts)
        (order-by (:desc :id)))
      :as 'post)))

(defun post-by-id (id)
  (with-connection (db)
    (retrieve-one
      (select :*
        (from :posts)
        (where (:= :id id)))
      :as 'post)))

(defun get-posts (post-limit &key (post-offset 0) (tag ""))
  (if (< post-offset 0)
    nil
    (with-connection (db)
      (retrieve-all
        (select :*
          (from :posts)
          (offset post-offset)
          (order-by (:desc :id))
          (where (:raw (format nil "tags similar to '%((~A))%'" tag)))
          (limit post-limit))))))

(defun render-post (post)
  (render (absolute-path "post.html")
          (list :subject (post-subject post)
                :date (post-date post)
                :content (post-content post)
                :tags (split-sequence:split-sequence #\Space (post-tags post)))))

;;
;; Routing rules

(defroute "/" ()
  (render-post (latest-post)))

(defroute ("/blog/page/([1-9]+)" :regexp :t) (&key captures)
  (let* ((page (parse-integer (first captures)))
         (posts (list :posts (get-posts 10 :post-offset (* 10 (1- page))))))
    (if (eq (cadr posts) nil)
      (render (absolute-path "_errors/404.html"))
      (render (absolute-path "blog_index.html")
              posts))))

(defroute ("/blog/post/([\\d]+)" :regexp t) (&key captures)
  (let ((id (parse-integer (first captures))))
    (if (eq (post-by-id id) nil)
      (render #P"_errors/404.html")
      (render-post (post-by-id (first captures))))))

(defroute ("/blog/tag/([\\w]+)/([1-9]+)" :regexp :t) (&key captures)
          (let* ((tag (first captures))
                 (page (parse-integer (second captures)))
                 (posts (get-posts 10 :post-offset (* 10 (1- page)) :tag tag)))
            (if (eq (cadr posts) nil)
              (render (absolute-path "_errors/404.html"))
              (render (absolute-path "blog_index.html")
                      (list :posts posts)))))

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
