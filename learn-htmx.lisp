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

(easy-routes:defroute root ("/" :method :get) ()
  (cl-who:with-html-output-to-string (s)
    (:body
     (:p "Hello world!"))))

(defun match-contact (q-value)
  (format t "Q VAL: |~A|~%" q-value)
  (case q-value
    ('nil *db*)
    (otherwise (select-by-contact q-value)))
  )

(defun select-by-contact (contact-name)
  (remove-if-not
   #'(lambda (contact) (equal (getf contact :first) contact-name ))))


(easy-routes:defroute contacts ("/contacts" :method :get) (q)
  (format t "Q IS: |~A|~%" q)
  (apply-template "index.html" (list :people (match-contact q))))

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))
