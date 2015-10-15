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

(defparameter *time-format* '((:year 4) #\- (:month 2) #\- (:day 2)))
(defun format-date (date)
  (local-time:format-timestring nil (local-time:universal-to-timestamp date) :format *time-format*)) 
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

(defun post-by-tag (tag)
  (with-connection (db)
    (retrieve-all
      (select :*
        (from :posts)
        (where (:raw (format nil "tags similar to '%((~A))%'" tag))))
      :as 'post)))

(defun post-count ()
  (with-connection (db)
    (cadr (retrieve-one (select ((:count :*)) (from :posts))))))

(defun render-post (post)
  (render (absolute-path "post.html")
          (list :title (post-subject post)
                :date (format-date (post-date post))
                :content (post-content post))))

;;
;; Routing rules

(defroute "/" ()
  (render-post (latest-post)))

(defroute ("/blog/page/([\\d]+)" :regexp :t) (&key captures)
  (render (absolute-path "blog_index.html")
          (do* ((post-list nil)
                (page (parse-integer (first captures)))
                (page-end (* page 10))
                (page-start (- page-end 10))
                (id page-start (1+ id))) 
            ((> id page-end) (list :posts post-list))
            (if (eq (post-by-id id) nil)
              nil
              (push (list :subject (post-subject (post-by-id id))
                          :date (format-date (post-date (post-by-id id)))
                          :url (format nil "/blog/post/~A" (post-id (post-by-id id))))
                    post-list)))))
    
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
