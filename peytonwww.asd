;;;; peytonwww.asd

(asdf:defsystem #:peytonwww
  :version "0.1"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:clack
               #:lack
               #:caveman2
               #:cl-ppcre
               #:cl-fad

               ;; HTML template
               #:djula)

  :components ((:module "src"
                        :components
                        ((:file "main" :depends-on ("config" "view"))
                         (:file "routes" :depends-on ("view"))
                         (:file "view" :depends-on ("config"))
                         (:file "config"))))
  :description "The source code for peytonfarrar.com")
