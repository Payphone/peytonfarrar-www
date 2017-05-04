;;;; package.lisp

(defpackage #:peytonwww
  (:use #:cl #:peytonwww.main)
  (:import-from #:peytonwww.config
                #:*static-directory*)
  (:import-from #:peytonwww.routes
                #:*app*)
  (:import-from :lack.builder
                :builder)
  (:export #:start
           #:stop))
(in-package :peytonwww)

(builder
 :accesslog
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/css/|/misc/)" path)
              path
              nil))
  :root *static-directory*)
 *app*)
