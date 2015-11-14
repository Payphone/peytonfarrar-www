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

(defun get-posts (post-limit &key (post-offset 0) (tag "%"))
  (if (< post-offset 0)
      nil
      (with-connection (db)
        (retrieve-all
         (select :*
           (from :posts)
           (offset post-offset)
           (order-by (:desc :id))
           (where (:like :tags (concatenate 'string "%" tag "%")))
           (limit post-limit))))))

(defun render-post (post)
  (render (absolute-path "post.html")
          (list :subject (post-subject post)
                :date (post-date post)
                :content (post-content post)
                :tags (split-sequence:split-sequence #\Space (post-tags post)))))

(defun submit-post (&key subject date content tags)
  (with-connection (db)
    (execute
     (insert-into :posts
       (set= :subject subject
             :date date
             :content content
             :tags tags)))))

(defun alter-post (id &key subject content tags)
  (with-connection (db)
    (execute
     (update :posts
       (set= :subject subject
             :content content
             :tags tags)
       (where (:= :id id))))))

(defun post-count (&optional tag)
  (with-connection (db)
    (cadr
     (retrieve-one
      (select ((:count :*))
        (from :posts)
        (where (:like :tags (concatenate 'string "%" tag "%"))))))))

;;
;; User functions

(defstruct user
  id
  username
  password
  groups)

(defun get-user (username password)
  (with-connection (db)
    (retrieve-one
     (select :*
       (from :users)
       (where (:and (:= :username username)
                    (:= :password (:crypt password :password)))))
     :as 'user)))

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

(defun of-group (group)
 (search group (gethash :groups *session*)))

(defun random-file (files)
  (nth (random (list-length files)) files))

;;
;; Routing rules

(defroute "/" ()
  (render-post (latest-post)))

(defroute ("/login" :method :GET) (&key |error|)
  (render (absolute-path "login.html")
          (if (string= |error| "t")
              (list :text "  Incorrect username or password")
              nil)))

(defroute ("/login" :method :POST) (&key |username| |password|)
  (let ((current-user (get-user |username| |password|)))
    (when (null current-user)
      (redirect "/login?error=t"))  
    (unless (null current-user)
      (setf (gethash :username *session*) |username|)
      (setf (gethash :groups *session*) (user-groups current-user))
      (redirect "/blog/new"))))
    
(defroute "/logout" ()
    (clrhash *session*)
    (redirect "/login"))

(defroute ("/blog/post/([\\d]+)" :regexp t) (&key captures)
  (let ((id (parse-integer (first captures))))
    (if (null (post-by-id id))
        (throw-code 404)
        (render-post (post-by-id id)))))

(defroute ("/blog/([1-9]+)" :regexp :t) (&key captures)
  (let* ((page (parse-integer (first captures)))
         (limit 20)
         (posts (get-posts limit :post-offset (* limit (1- page)))))
    (if (null posts)
        (throw-code 404)
        (render (absolute-path "blog_index.html")
                (list :posts posts
                      :previous (if (> page 1) (1- page))
                      :next (if (<= (* limit page) (post-count)) (1+ page)))))))

(defroute ("/blog/tag/([\\w]+)/([\\d]+)" :regexp :t) (&key captures)
  (let* ((tag (first captures))
         (page (parse-integer (second captures)))
         (limit 20)
         (posts (get-posts limit :post-offset (* limit (1- page)) :tag tag)))
    (if (null posts)
        (throw-code 404)
        (render (absolute-path "blog_index.html")
                (list :posts posts
                      :previous (if (> page 1) (1- page))
                      :next (if (<= (* limit page) (post-count tag)) (1+ page)))))))

(defroute ("/blog/new" :method :GET) (&key |error|)
  (if (of-group "dev")
      (render (absolute-path "new_post.html")
              (list :title "New Post"))
      (throw-code 403)))

(defroute ("/blog/new" :method :POST) (&key |subject| |content| |tags|)
  (when (of-group "dev")
    (submit-post 
     :subject |subject|
     :date (get-universal-time)
     :content |content|
     :tags |tags|)
    (redirect "/")))

(defroute ("/blog/edit/([\\d]+)" :regexp :t) (&key captures)
  (let* ((id-string (first captures))
         (id (parse-integer id-string))
         (post (post-by-id id)))
    (unless post (throw-code 404))
    (if (of-group "dev")
        (render (absolute-path "new_post.html")
                (list :title "Edit Post"
                      :page (concatenate 'string "/blog/edit/" id-string)
                      :subject (post-subject post)
                      :content (post-content post)
                      :tags (post-tags post)))
        (throw-code 403))))

(defroute ("/blog/edit/([\\d]+)" :regexp :t :method :POST)
    (&key captures |subject| |content| |tags|)
  (when (of-group "dev")
    (alter-post (parse-integer (first captures))
                :subject |subject|
                :content |content|
                :tags |tags|)
    (redirect "/")))

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
