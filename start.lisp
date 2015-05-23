(ql:quickload :wookie-doc)
(vom:config t :info)
(let ((blackbird:*debug-on-error* t)
      (wookie-config:*debug-on-error* t))
  (wookie-doc:start :port 9061))

