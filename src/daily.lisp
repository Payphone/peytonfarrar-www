(in-package :peytonwww.web)

(defmacro with-universal-time (&body body)
  `(multiple-value-bind (second minute hour date month year dow dst-p tz)
       (get-decoded-time)
     ,@body))

(defmacro get-daily (&body body)
  `(with-connection (db)
     (retrieve-all
      (select :*
        (from :daily)
        ,@body))))

(defun daily-today ()
  (labels ((lst ()
             (get-daily
              (order-by (:asc :id))
              (where (:<= (:- (round (/ (get-universal-time) 60 60))
                              (:/ :date 60 60))
                          24)))))
    (let ((daily-list (lst)))
      (if (null daily-list)
          nil
          daily-list))))

(defun daily-week ()
  (labels ((lst ()
             (get-daily
              (order-by (:desc :id))
              (where (:<= (:- (round (/ (get-universal-time) 60 60 24))
                              (:/ :date 60 60 24))
                          7)))))
    (let ((weekly-list (lst)))
      (if (null weekly-list)
          nil
          weekly-list))))

(defun submit-daily (&key title time tags)
  (with-connection (db)
    (execute
     (insert-into :daily
       (set= :title title
             :date (with-universal-time
                     (declare (ignore second minute hour dow dst-p))
                     (encode-universal-time 0 0 0 date month year tz))
             :time time
             :tags tags
             :username (gethash :username *session*))))))

(defroute "/daily" ()
  (render (absolute-path "daily.html")
          (list :dailies (daily-today)
                :week (daily-week))))

(defroute ("/daily/new" :method :GET) ()
  (with-group "dev"
    (render (absolute-path "new_daily.html"))))

(defroute ("/daily/new" :method :POST) (&key |title| |time| |tags|)
  (with-group "dev"
    (submit-daily :title |title|
                  :time  |time|
                  :tags  |tags|)
    (redirect "/daily/new")))

