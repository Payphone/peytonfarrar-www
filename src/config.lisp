(defpackage peytonwww.config
  (:use #:cl)
  (:export
   #:config
   #:*application-root*
   #:*static-directory*
   #:*template-directory*
   #:*doc-directory*))
(in-package :peytonwww.config)


(defparameter *application-root*   (asdf:system-source-directory :peytonwww))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
(defparameter *doc-directory* (merge-pathnames #P"docs/" *static-directory*))
