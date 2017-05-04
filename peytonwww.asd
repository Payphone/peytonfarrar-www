;;;; peytonwww.asd

(asdf:defsystem #:peytonwww
  :description "The source for peytonfarrar.com"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:clack
               #:lack
               #:ningle
               #:cl-markup
               #:lass
               #:cl-fad
               #:cl-ppcre)
  :serial t
  :components ((:file "src/config")
               (:file "src/main")
               (:file "src/routes")
               (:file "app")))
