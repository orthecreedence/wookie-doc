---
title: Wookie: An asynchronous HTTP server for Common Lisp
layout: base
---

{{tpl|tag|div.home}}
{{tpl|tag|div.inner}}

Wookie - An async HTTP server for Common lisp
====================================
```lisp
(defpackage :my-app
  (:use :cl :wookie))
(in-package :my-app)

;; load Wookie's core plugins
(load-plugins)

;; define our homepage route
(defroute (:get "/") (req res)
  (send-response res :body "Thanks for stopping by!"))

;; start serving requests!
(as:with-event-loop ()
  (start-server (make-instance 'listener :port 80)))
```
{{tpl|tag|/div}}
{{tpl|tag|/div}}


{{tpl|tag|content.callout}}

Wookie is an asynchronous HTTP server built on top of
[cl-async](http://orthecreedence.github.com/cl-async) and
[fast-http](https://github.com/fukamachi/fast-http). Wookie functions both as a
simple HTTP server and as an app server to run your HTTP-based Common Lisp app.

{{tpl|tag|div.doclink}}
[Get started &raquo;](/docs)
{{tpl|tag|/div}}

