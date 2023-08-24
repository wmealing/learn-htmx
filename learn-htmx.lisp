;;;; learn-htmx.lisp

(in-package #:learn-htmx)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defvar *db* nil)
(setq *db* nil)
;; two test contacts, remove FIXME LATER.
(add-contact '(:first "Athene" :last "Chan" :phone "5551234" :email "achan@achan.com"))
(add-contact '(:first "Wade" :last "Mealing" :phone "5552345" :email "wmealing@wmealing.com"))


(defvar *template-dir*
  (asdf:system-relative-pathname "learn-htmx" "templates/"))

(defun add-contact (contact) (push contact *db*))

(defparameter *port* 4242
  "We can override it in the config file or from an environment variable.")

(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/"
       (truename "./static/"))
      hunchentoot:*dispatch-table*)

(defun apply-template (filename values)
  (with-open-file (template-file (merge-pathnames *template-dir* filename))
    (let ((template (make-string (file-length template-file))))
      (read-sequence template template-file)
      (funcall (cl-template:compile-template template) values))))

(defun select-by-contact (contact-name)
  (if (string= contact-name "")
      *db*
      (remove-if-not
       #'(lambda (contact) (equal (getf contact :first) contact-name ))*db*)
      ))

(easy-routes:defroute contacts ("/contacts" :method :get) (q)
  (apply-template "index.html" (list :people (select-by-contact q)))
  )

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))
