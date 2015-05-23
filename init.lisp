(in-package :wookie-doc)

(setf cl-who:*attribute-quote-char* #\"
      (cl-who:html-mode) :html5)

;; load all enabled wookie plugins
(load-plugins :use-quicklisp t)

(defun error-handler (err &optional socket)
  (unless (typep err 'as:tcp-info)
    (format t "(wookie-doc) UNcaught error: ~a~%" err)))

(defun start (&key bind (port 8080))
  ;; setup the wookie log
  (setf *log-level* :notice)

  (setf *error-handler* 'error-handler)

  ;; load/cache all the views
  (load-views)

  ;; start the server
  (as:with-event-loop (:catch-app-errors nil)
    (let* ((listener (make-instance 'listener :bind bind :port port))
           (server (start-server listener)))
      (as:signal-handler 2
        (lambda (sig)
          (declare (ignore sig))
          (as:free-signal-handler 2)
          (as:close-tcp-server server)
          (as:exit-event-loop))))))

