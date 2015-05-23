---
title: Documentation
layout: documentation
---

Getting started
=============
{{toc}}

Here are some quick examples to get you started using Wookie. Remember that
Wookie runs on top of a [cl-async](http://orthecreedence.github.com/cl-async) event loop,
so you must start Wookie from within an event loop for it to work.

If you need a reference for Wookie, the source for this doc site is built on top
of Wookie and [is available on github](https://github.com/orthecreedence/wookie-doc).

### Getting Wookie
Wookie is available on quicklisp:

```lisp
(ql:quickload :wookie)
```

However, sometimes Wookie has pending changes/fixes. If you want to grab the
latest version you can do:

```bash
cd ~/quicklisp/local-projects
git clone git://github.com/orthecreedence/wookie
```

### Starting Wookie
This example shows how to set up a very basic app using Wookie. It uses a
[listener](/docs/listeners#listener), which describes how people can connect to
the server, and passes it to [start-server](/docs/listeners#start-server).

This example also shows how to load Wookie's [plugins](/docs/plugins),
which provide key functionality such as parsing GET/POST parameters, file and
directory serving, etc.

```lisp
(defpackage :my-website
  (:use :cl :wookie :wookie-plugin-export))
(in-package :my-website)

;; load Wookie's core plugins. much of Wookie's functionality is written as
;; plugins which can be enabled/disabled/overwritten it desired.
(load-plugins)

;; setup a simple homepage
(defroute (:get "/") (req res)
  (send-response res :body "Welcome to my app!"))

;; setup a mapping for the url / to a local directory assets. a request for
;; 
;;   /images/background.jpg
;;
;; will load "./assets/images/background.jpg"
(def-directory-route "/" "./assets")

;; start the event loop that Wookie runs inside of
(as:with-event-loop ()
  ;; create a listener object, and pass it to start-server, which starts Wookie
  (let* ((listener (make-instance 'listener
                                  :bind nil  ; equivalent to "0.0.0.0" aka "don't care"
                                  :port 80))
         ;; start it!! this passes back a cl-async server class
         (server (start-server listener)))
    ;; stop server on ctrl+c
    (as:signal-handler 2
      (lambda (sig)
        (declare (ignore sig))
        ;; remove our signal handler (or the event loop will just sit indefinitely)
        (as:free-signal-handler 2)
        ;; graceful stop...rejects all new connections, but lets current requests
        ;; finish.
        (as:close-tcp-server server)))))
```

