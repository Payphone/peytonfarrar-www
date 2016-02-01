(in-package :peytonwww.web)

;;
;; Time Functions

(defmacro with-universal-time (&body body)
  `(multiple-value-bind (second minute hour date month year dow dst-p tz)
       (get-decoded-time)
     ,@body))

(defun current-day ()
  (with-universal-time
      (declare (ignore second minute hour dow dst-p))
    (encode-universal-time 0 0 0 date month year tz)))

(defun current-week ()
  (with-universal-time
      (declare (ignore second minute hour dst-p))
    (encode-universal-time 0 0 0 (- date dow) month year tz)))

(defun universal-hour ()
  (round (/ (get-universal-time) 60 60)))

;;
;; Daily Helper Functions

(defstruct daily
  id
  title
  time
  date
  tags
  username)

(defun export-daily (d)
  (list :title (daily-title d) :time (daily-time d)))

(defun condense-daily (daily-lst)
  (labels ((title= (d1)
             (lambda (d2)
               (and d1 d2
                    (string= (daily-title d1) (daily-title d2))))))
    (do* ((lst daily-lst (cdr lst))
          (clst nil)
          (d1 (first lst) (first lst))
          (d2 nil (find-if (title= d1) clst)))
         ((null lst) clst)
      (if (and d2 d1)
          (incf (daily-time d2) (daily-time d1))
          (push d1 clst)))))

;;
;; Database calls

(defmacro get-dailies (&body body)
  `(with-connection (db)
     (retrieve-all
      (select :*
        (from :daily)
        ,@body)
      :as 'daily)))

(defun dailies-today ()
  (get-dailies
    (order-by (:desc :id))
    (where (:<= (:- (universal-hour)
                    (:/ :date 60 60))
                24))))

(defun dailies-week ()
  (get-dailies
   (order-by (:desc :id))
   (where (:>= (:- (:/ :date 60 60 24)
                   (round (/ (current-week) 60 60 24)))
               0))))

(defun submit-daily (&key title time tags)
  (if (and title time tags)
      (with-connection (db)
        (execute
         (insert-into :daily
                      (set= :title title
                            :date (current-day)
                            :time time
                            :tags tags
                            :username (gethash :username *session*)))))))

;;
;; Routes

(defroute "/daily" ()
  (render "daily.html"
          (list :dailies (mapcar #'export-daily (dailies-today))
                :week (mapcar #'export-daily (condense-daily (dailies-week))))))

(defroute ("/daily/new" :method :GET) ()
  (with-group "dev"
    (render "new_daily.html")))

(defroute ("/daily/new" :method :POST) (&key |title| |time| |tags|)
  (with-group "dev"
    (submit-daily :title |title|
                  :time  |time|
                  :tags  |tags|)
    (redirect "/daily/new")))
