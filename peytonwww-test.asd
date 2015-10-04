(in-package :cl-user)
(defpackage peytonwww-test-asd
  (:use :cl :asdf))
(in-package :peytonwww-test-asd)

(defsystem peytonwww-test
  :author "Peyton Farrar"
  :license ""
  :depends-on (:peytonwww
               :prove)
  :components ((:module "t"
                :components
                ((:file "peytonwww"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
