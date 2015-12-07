(in-package :peytonwww.web)

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
             :date (get-universal-time)
             :time time
             :tags tags
             :username (gethash :username *session*))))))

(defroute "/daily" ()
  (render (absolute-path "daily.html")
          (list :dailies (daily-today)
                :week (daily-week))))
