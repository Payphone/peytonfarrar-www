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
                     (:raw (format nil "password = crypt(\'~A\', password)" 
                                   password)))))
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

(defun markdown-to-html (markdown)
  (cl-markdown:render-to-stream 
    (cl-markdown:markdown markdown) :html nil))

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
    (when (eq current-user nil)
      (redirect "/login?error=t"))  
    (unless (eq current-user nil)
      (setf (gethash :username *session*) |username|)
      (setf (gethash :groups *session*) (user-groups current-user))
      (redirect "/blog/new"))))
    
(defroute "/logout" ()
    (clrhash *session*)
    (redirect "/login"))

(defroute ("/blog/page/([1-9]+)" :regexp :t) (&key captures)
  (let* ((page (parse-integer (first captures)))
         (posts (get-posts 20 :post-offset (* 20 (1- page))))
         (previous-page (unless (< (1- page) 1) (1- page)))
         (next-page (when (get-posts 1 :post-offset (* 20 page)) (1+ page))))
    (if (eq posts nil)
        (throw-code 404)
        (render (absolute-path "blog_index.html")
                (list :posts posts :previous previous-page :next next-page)))))

(defroute ("/blog/post/([\\d]+)" :regexp t) (&key captures)
  (let ((id (parse-integer (first captures))))
    (if (eq (post-by-id id) nil)
        (throw-code 404)
        (render-post (post-by-id id)))))

(defroute ("/blog/tag/([\\w]+)/([1-9]+)" :regexp :t) (&key captures)
  (let* ((tag (first captures))
         (page (parse-integer (second captures)))
         (posts (get-posts 20 :post-offset (* 10 (1- page)) :tag tag))
         (previous-page (unless (< (1- page) 1) (1- page)))
         (next-page (when (get-posts 1 :post-offset (* 20 page)) (1+ page))))
    (if (eq (car posts) nil)
        (throw-code 404)
        (render (absolute-path "blog_index.html")
                (list :posts posts :previous previous-page :next next-page)))))

(defroute ("/blog/new" :method :GET) (&key |error|)
  (if (of-group "dev")
      (render (absolute-path "new_post.html"))
      (throw-code 403)))

(defroute ("/blog/new" :method :POST) (&key |subject| |content| |tags|)
  (when (of-group "dev")
    (submit-post 
     :page "/blog/new"
     :subject |subject|
     :date (get-universal-time)
     :content (markdown-to-html |content|)
     :tags |tags|)
    (redirect "/")))

(defroute ("/blog/edit/([\\d]+)" :regexp :t) (&key captures)
  (let* ((id-string (first captures))
         (id (parse-integer id-string))
         (post (post-by-id id)))
    (unless post (throw-code 404))
    (if (of-group "dev")
        (render (absolute-path "new_post.html")
                (list :page (concatenate 'string "/blog/edit/" id-string)
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

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (absolute-path "_errors/404.html"))

(defmethod on-exception ((app <web>) (code (eql 403)))
  (declare (ignore app))
  (absolute-path "_errors/403.html"))
