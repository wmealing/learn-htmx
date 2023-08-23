;;;; learn-htmx.asd

(asdf:defsystem #:learn-htmx
  :description "Describe learn-htmx here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot #:cl-template #:str #:cl-who #:easy-routes)
  :components ((:file "package")
               (:file "learn-htmx")))
