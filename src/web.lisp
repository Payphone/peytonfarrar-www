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
;; User functions

(defstruct user
  id
  username
  password
  groups)

(defun get-user (username password)
  (if (or (null username) (null password))
      nil
      (with-connection (db)
        (retrieve-one
         (select :*
                 (from :users)
                 (where (:and (:= :username username)
                              (:= :password (:crypt password :password)))))
         :as 'user))))

;;
;; General Routing rules

(defroute "/" ()
  (render "index.html"))

(defroute ("/login" :method :GET) (&key |error|)
  (render "login.html"
          (if (string= |error| "t")
              (list :text "  Incorrect username or password"))))

(defroute ("/login" :method :POST) (&key |username| |password|)
  (let ((current-user (get-user |username| |password|)))
    (when (null current-user)
      (redirect "?error=t"))
    (unless (null current-user)
      (setf (gethash :username *session*) |username|)
      (setf (gethash :groups *session*) (user-groups current-user))
      (redirect "/blog/new"))))

(defroute "/logout" ()
  (clrhash *session*)
  (redirect "/login"))

(defroute "/jazz" ()
  (let ((images (root-directory "static/images/Night/*.jpg"))
        (songs (root-directory "static/music/Jazz/*.ogg")))
    (render "jazz.html"
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
  (render "error.html"
          (list :error-code error-code :error-message (error-reason error-code))))
