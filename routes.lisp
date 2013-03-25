(in-package :wookie-doc)

(defun page-not-found (res)
  "Sends a 404 error."
  (send-response res
                 :status 404
                 :body (layout 'default (list :content "Page not found."
                                              :title "Page not found."))))

;; clear out all routes (start anew)
(clear-routes)

(defroute (:get "/") (req res)
  (let ((body (load-view :index/index)))
    (send-response res :headers '(:content-type "text/html") :body body)))

(defroute (:get "/refresh-views") (req res)
  (load-views)
  (send-response res :body "Views refreshed!!"))

(def-directory-route "/" (format nil "~awebroot" *root*))

(add-error-handler
  t (lambda (err sock &rest _)
      (declare (ignore _))
      (format t "err: ~s~%" (list err sock))))

(defroute (:get "/docs(/(.*))?") (req res args)
  (handler-case
    (let* ((view (intern (string-upcase (format nil "docs/~a" (or (cadr args) "index"))) :keyword))
           (content (load-view view)))
      (send-response res :headers '(:content-type "text/html") :body content)
      (as:delay (lambda ()
                  (format t "LOLOLOL~%")
                  (error "FUUUUU"))
                :time 1))
    (view-not-found ()
      (page-not-found res))
    (error (e)
      (send-response res :status 500 :body (format nil "~a" e)))))

(defroute (:get ".+") (req res)
  (page-not-found res))
