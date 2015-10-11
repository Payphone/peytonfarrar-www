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
        (from :post)
        (order-by (:desc :id)))
      :as 'post)))

(defun post-by-id (id)
  (with-connection (db)
    (retrieve-one
      (select :*
        (from :post)
        (where (:= :id id)))
      :as 'post)))

(defun render-post (post)
  (render (absolute-path "post.html")
          (list :title (post-subject post)
                :date (format-date (post-date post))
                :content (post-content post))))
;;
;; Routing rules

(defroute "/" ()
  (render-post (latest-post)))

(defroute "/blog" ()
  (let ((post-list nil))
    (dotimes (n 2) 
      (let ((id (+ n 1)))
        (push (list :subject (post-subject (post-by-id id))
                    :date (format-date (post-date (post-by-id id)))
                    :url (format nil "/blog/post/~A" (post-id (post-by-id id))))
              post-list)))
      (render (absolute-path "blog_index.html")
              (list :posts post-list))))

(defroute "/blog/post/:id" (&key id)
  (let ((post-id (parse-integer id :junk-allowed t)))
    (if (and (integerp post-id) (not (eq (post-by-id post-id) nil)))
       (render-post (post-by-id post-id))
       (render #P"_errors/404.html"))))

(defroute "/jazz" ()
  (let ((images (directory (merge-pathnames "static/images/Night/*.jpg" *application-root*)))
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
