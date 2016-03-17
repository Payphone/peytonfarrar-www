(in-package :peytonwww.web)

;;
;; Time Functions

(defun current-day ()
  "Returns the timestamp for the current day"
  (timestamp-minimize-part (now) :hour))

(defun current-week ()
  "Gives the universal time of the Monday of the current week"
  (adjust-timestamp (current-day) (offset :day-of-week :monday)))

(defun universal-hour ()
  "Gives the total number of hours since January 1, 1970"
  (values (round (/ (get-universal-time) 60 60))))

;;
;; Daily Helper Functions

(defun condense-dailies (daily-lst)
  "Combines the time of daily entries with the same title"
  (flet ((title= (d1)
           (lambda (d2)
             (and d1 d2
                  (string= (getf d1 :title) (getf d2 :title))))))
    (do* ((lst daily-lst (cdr lst))
          (clst nil)
          (d1 (first lst) (first lst))
          (d2 nil (find-if (title= d1) clst)))
         ((null lst) clst)
      (if (and d2 d1)
          (incf (getf d2 :time) (getf d1 :time))
          (push d1 clst)))))

;;
;; Database calls

(defmacro get-dailies (&body body)
  "Generic way of quering for dailies"
  `(with-connection (db)
     (retrieve-all
      (select :*
              (from :daily)
              ,@body))))

(defun dailies-today ()
  "Returns all the dailies for the current day"
  (get-dailies
   (order-by (:desc :id))
   (where (:<= (:- (universal-hour)
                   (:/ :date 60 60))
               24))))

(defun dailies-week ()
  "Returns all the dailies for the current week"
  (get-dailies
    (order-by (:desc :id))
    (where (:>= (:- (:/ :date 60 60 24)
                    (round (/ (timestamp-to-universal (current-week))
                              60 60 24)))
                0))))

(defun submit-daily (&key title time tags)
  "Inserts a new daily into the daily table"
  (if (and title time tags)
      (with-connection (db)
        (execute
         (insert-into :daily
           (set= :title title
                 :date (timestamp-to-universal (current-day))
                 :time time
                 :tags tags
                 :username (gethash :username *session*)))))))

;;
;; Routes

(defroute "/daily" ()
  (render "daily.html"
          (list :dailies (dailies-today)
                :week (condense-dailies (dailies-week)))))

(defroute ("/daily/new" :method :GET) ()
  (with-group "dev"
    (render "new_daily.html")))

(defroute ("/daily/new" :method :POST) (&key |title| |time| |tags|)
  (with-group "dev"
    (submit-daily :title |title|
                  :time  |time|
                  :tags  |tags|)
    (redirect "/daily/new")))
