(defpackage #:peytonwww.compile
  (:use #:cl #:peytonwww.config #:cl-markup)
  (:export #:compile-html-to-file))

(in-package :peytonwww.compile)

(defun read-file (file)
  (with-open-file (in file)
    (read in)))

(defun load-templates (&rest files)
  (loop for file in files do
       (load (merge-pathnames (make-pathname :type "lisp")
                              (merge-pathnames file *template-directory*)))))

(defmacro compile-html-from-file (file)
  (let ((html (with-open-file (in file)
                 (loop for object = (read in nil nil t) while object
                    collect object))))
    `(progn ,@html)))

(defmacro compile-html-to-file (file)
  `(let ((output-file (merge-pathnames (make-pathname :type "html") ,file)))
     (with-open-file (out output-file :direction :output :if-exists :supersede)
       (princ (compile-html-from-file ,file) out))
     (uiop:native-namestring output-file)))

(defun conc (&rest strings)
  (reduce #'(lambda (s1 s2) (concatenate 'string s1 s2)) strings))

(defun lispp (file)
  "Returns true if the pathname has a file type of lisp."
  (string= (pathname-type file) "lisp"))

(defun lassp (file)
  "Returns true if the pathname has a file type of lass."
  (string= (pathname-type file) "lass"))

#|
;; Load HTML templates
(cl-fad:walk-directory *template-directory* (lambda (f)
                                              (if (lispp f) (load f))))

;; Compile HTML pages
(cl-fad:walk-directory *page-directory* (lambda (f)
                                          (if (lispp f)
                                              (compile-html-to-file f))))

;; Compile LASS
(cl-fad:walk-directory (merge-pathnames #P"css/" *static-directory*)
                       (lambda (f)
                         (if (lassp f) (lass:generate f))))
|#
