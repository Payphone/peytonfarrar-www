(defpackage #:peytonwww.app
  (:use #:cl)
  (:import-from #:lack.builder
                #:builder)
  (:import-from #:ppcre
                #:scan
                #:regex-replace)
  (:import-from #:peytonwww.routes
                #:*app*)
  (:import-from #:peytonwww.config
                #:*static-directory*))
(in-package :peytonwww.app)

(builder
 :accesslog
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/css/|/misc/)" path)
              path
              nil))
  :root *static-directory*)
 *app*)
