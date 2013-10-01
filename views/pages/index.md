---
title: Wookie: An asynchronous web server for Common Lisp
layout: default
---

Wookie - An asynchronous HTTP server
====================================
Wookie is an asynchronous HTTP server written in Common Lisp. It is built on top
of [cl-async](http://orthecreedence.github.com/cl-async) and
[http-parse](https://github.com/orthecreedence/http-parse). Wookie's code is
hosted on [github](https://github.com/orthecreedence/wookie).

*Wookie is somewhat new and considered beta.*

See __[Wookie's documentation](/docs).__

```lisp
(defpackage :wookie-test
  (:use :cl :wookie))
(in-package :wookie-test)

;; load Wookie's core plugins
(load-plugins)

;; define our homepage route
(defroute (:get "/") (req res)
  (send-response res :body "Thanks for stopping by!"))

;; start serving requests!
(as:with-event-loop ()
  (start-server (make-instance 'listener :port 80)))
```

Wookie is [licensed MIT](https://github.com/orthecreedence/wookie/blob/master/LICENSE).

