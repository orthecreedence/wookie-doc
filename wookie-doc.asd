(asdf:defsystem wookie-doc
  :author "Andrew Lyon <andrew@musio.com>"
  :licence "MIT"
  :version "0.0.1"
  :depends-on (#:cl-fad #:wookie #:cl-who)
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "util" :depends-on ("config"))
   (:file "init" :depends-on ("util"))
   (:file "routes" :depends-on ("init"))))

