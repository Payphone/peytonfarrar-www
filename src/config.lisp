;;; config.lisp

(defpackage #:peytonwww.config
  (:use #:cl)
  (:export #:*application-root*
           #:*page-directory*
           #:*static-directory*
           #:*template-directory*))

(in-package :peytonwww.config)

(defparameter *application-root* (asdf:system-source-directory :peytonwww))
(defparameter *page-directory* (merge-pathnames #P"pages/" *application-root*))
(defparameter *doc-directory* (merge-pathnames #P"docs/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/"
                                                    *application-root*))
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
