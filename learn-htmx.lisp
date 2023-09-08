;;;; learn-htmx.lisp

(in-package #:learn-htmx)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defvar *db* nil)

(defun get-db ()
  *db*)

(setq *db* nil)

(defvar *next* 3)

(defun get-next ()
  (setq *next* (+ *next* 1))
  *next*)

(defun add-contact (contact) (push contact *db*))

(defun setup ()
  (add-contact '(:id "0" :first_name "Athene" :last_name "Chan" :phone "5551234" :email "achan@achan.com"))
  (add-contact '(:id "1" :first_name "Wade"   :last_name "Mealing" :phone "5552345" :email "wmealing@wmealing.com")))


;; stolen from gigamonkeys.
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key id first_name last_name phone email)
  #'(lambda (record)
      (and
       (if id          (equal (getf record :id)  id)  t)
       (if first_name  (equal (getf record :first_name) first_name) t)
       (if last_name   (equal (getf record :last_name) last_name) t)
       (if email       (equal (getf record :last_name) email) t)
       (if phone       (equal (getf record :phone) phone) t))))

(defun update (selector-fn &key id first_name last_name phone email)
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if id    (setf (getf row :id) id))
               (if first_name   (setf (getf row :first_name) first_name))
               (if last_name   (setf (getf row :last_name) last_name))
               (if email   (setf (getf row :email) email))
               (if phone   (setf (getf row :phone) phone)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


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
      (select (where :first_name contact-name))))

(defun select-by-contact-id (id)
  (if (or (string= id "")
          (eq id 'nil))
      *db*
      (select (where :id id))))

(easy-routes:defroute index-contacts-get ("/contacts" :method :get) ()
  (apply-template "index.html" (list :people *db*)))

;; htmx apparently.
(easy-routes:defroute index-contacts-post ("/contacts" :method :post) ()
  (let ((contacts (select-by-contact (hunchentoot:post-parameter "q"))))
    (apply-template "just-contacts.html" (list :people contacts))))

(easy-routes:defroute contact-new-get ("/contacts/new" :method :get) ()
  (apply-template "new-contact.html" (list :person '(:id ""
                                                     :first_name ""
                                                     :last_name ""
                                                     :email ""
                                                     :phone "") :errors '(:first_name ""))))

;; TODO use some validation magic here, return errors.
(defun validate-contact (contact)
  (declare (ignore contact))
  'nil
  )

(easy-routes:defroute contact-new-post ("/contacts/new" :method :post) ()
  (let* ((email (hunchentoot:post-parameter "email"))
         (first_name (hunchentoot:post-parameter "first_name"))
         (last_name (hunchentoot:post-parameter "last_name"))
         (phone (hunchentoot:post-parameter "phone"))
         (contact `(:email      ,email
                    :first_name ,first_name
                    :last_name  ,last_name
                    :phone      ,phone
                    :id         ,(write-to-string (get-next))))
         (errors (validate-contact contact)))
    (format t "HAS ERRORS: ~A~%" errors)

    (if (eq errors 'nil)
        (progn
          (add-contact contact) ;; maybe better error handling here ?
          (hunchentoot:redirect "/contacts"))
        (progn
          (apply-template "new-contact.html" (list :person contact :errors errors))))))

;; view contact
(easy-routes:defroute contact-view-get ("/contacts/:id" :method :get) ()
  (let ((contact (first (select-by-contact-id id))))
    (apply-template "show.html" (list :contact contact))))

(easy-routes:defroute contact-edit-screen-get ("/contacts/:id/edit" :method :get) ()
  (let ((contact (first (select-by-contact-id id))))
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

    (format t "YEAH POST ?~%")
    (format t "POSTED CONTACT: ~A~%" posted-contact)

    ;; validate things here.
    ;;(add-contact posted-contact)
    (let ((errors (validate-contact posted-contact)))

      (if (not errors)
          ;; no errors update the contact
          (progn
            (update (where :id id) :email email
                                   :first_name first_name
                                   :last_name last_name
                                   :phone phone)

            (hunchentoot:redirect "/contacts"))
          ;; otherwise show the errors.
          (apply-template "show.html" (list :contact posted-contact :errors errors))))))

(easy-routes:defroute contact-delete-get ("/contacts/:id/delete") ()
  (progn
    (delete-rows (where :id id))
    (hunchentoot:redirect "/contacts")))

(easy-routes:defroute contact-delete-delete ("/contacts/:id" :method :delete) ()
  (progn
    (delete-rows (where :id id))
    (format t "DELETING THE CONTACT~%")
    (hunchentoot:redirect "/contacts" :code 303)))

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))

(setup)
