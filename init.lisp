(in-package :wookie-doc)

(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5)

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun error-handler (err)
  (unless (typep err 'as:tcp-info)
    (format t "(wookie-doc) UNcaught error: ~a~%" err)))

(defun start (&key bind (port 8080))
  ;; setup the wookie log
  (setf *log-level* :notice)

  (setf *error-handler* 'error-handler)

  ;; load/cache all the views
  (load-views)

  ;; start the server
  (let ((listener (make-instance 'listener :bind bind :port port)))
    (as:with-event-loop (:catch-app-errors t)
      (start-server listener))))

