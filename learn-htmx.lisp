;;;; learn-htmx.lisp

(in-package #:learn-htmx)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defvar *db* nil)

(setq *db* nil)

(defun add-contact (contact) (push contact *db*))

(defun setup ()
  (add-contact '(:id "0" :first_name "Athene" :last_name "Chan" :phone "5551234" :email "achan@achan.com"))
  (add-contact '(:id "1" :first_name "Wade"   :last_name "Mealing" :phone "5552345" :email "wmealing@wmealing.com")))

(defvar *template-dir*
  (asdf:system-relative-pathname "learn-htmx" "templates/"))

(defparameter *port* 4242
  "we can override it in the config file or from an environment variable.")

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
  (if (or (string= contact-name "")
          (eq contact-name 'nil))
      *db*
      (remove-if-not
       #'(lambda (contact) (equal (getf contact :first_name) contact-name ))*db*)
      ))

(defun select-by-contact-id (id)
  (if (or (string= id "")
          (eq id 'nil))
      *db*
      (remove-if-not
       #'(lambda (contact) (equal (getf contact :id) id ))*db*)
      ))

(easy-routes:defroute index-contacts ("/contacts" :method :get) (q)
  (apply-template "index.html" (list :people (select-by-contact q))))

(easy-routes:defroute contact-new-get ("/contacts/new" :method :get) ()
  (apply-template "new-contact.html" (list :person '(:id 1 :first_name "Wade"))))

;; TODO use some validation magic here, return errors.
(defun validate-contact (contact)
  ;;  (list :first_name "Invalid first name , dawg")
  '()
  )

(easy-routes:defroute contact-new-post ("/contacts/new" :method :post) ()
  (progn
    (let* ((email (hunchentoot:post-parameter "email"))
           (first_name (hunchentoot:post-parameter "first_name"))
           (last_name (hunchentoot:post-parameter "last_name"))
           (phone (hunchentoot:post-parameter "phone"))
           (next-id "10")
           (contact `(:email      ,email
                      :first_name ,first_name
                      :last_name  ,last_name
                      :phone      ,phone
                      :id         ,next-id))
           (errors (validate-contact contact)))

      (if (eq errors 'nil)
          (progn
            (add-contact contact) ;; maybe better error handling here ?
            (hunchentoot:redirect "/contacts")            )
          (progn
            (apply-template "new-contact.html" (list :person contact :errors errors)))))))

;; view contact
(easy-routes:defroute contact-view-get ("/contacts/:id" :method :get) ()
  (let ((contact (first (select-by-contact-id id))))
    (format t "GOT CONTACT/:ID : ~s~%" (select-by-contact-id id))
    (apply-template "show.html" (list :contact contact))))

(easy-routes:defroute contact-edit-screen-get ("/contacts/:id/edit" :method :get) ()
  (let ((contact (first (select-by-contact-id id))))
    (format t "GOT CONTACT/:ID : ~s~%" (select-by-contact-id id))
    (apply-template "edit.html" (list :contact contact))))

(easy-routes:defroute contact-edit-screen-post ("/contacts/:id/edit" :method :post) ()
  (let* ((email (hunchentoot:post-parameter "email"))
         (first_name (hunchentoot:post-parameter "first_name"))
         (last_name (hunchentoot:post-parameter "last_name"))
         (phone (hunchentoot:post-parameter "phone"))
         (posted-contact `(:email      ,email
                           :first_name ,first_name
                           :last_name  ,last_name
                           :phone      ,phone
                           :id         ,id)))

    ;; validate things here.
    (add-contact posted-contact) ;; maybe better error handling here ?

    (let ((errors (validate-contact posted-contact)))

      (if (not errors)
          (apply-template "show.html" (list :contact posted-contact :errors errors))
          (hunchentoot:redirect "/contacts")))))

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))

(setup)
