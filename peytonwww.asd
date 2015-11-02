(in-package :cl-user)
(defpackage peytonwww-asd
  (:use :cl :asdf))
(in-package :peytonwww-asd)

(defsystem peytonwww
  :version "0.1"
  :author "Peyton Farrar"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; HTML template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "The source code for the website peytonfarrar.com")
