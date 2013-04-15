---
title: Documentation
layout: default
---

<a id="documentation"></a>
Documentation
=============
Wookie's documentation is split into several parts:

- [Listeners](/docs/listeners)<br>
  Listeners are how you start/stop a Wookie server
- [Routes](/docs/routes)<br>
  Routes are how you tell Wookie how you want to handle certain requests
- [Request handling](/docs/request-handling)<br>
  Covers handling incoming requests, responding to them, and everything inbetween.
- [Error handling](/docs/error-handling)<br>
  Covers catching and handling of asynchronous errors/conditions while Wookie is
  running.
- [Plugins](/docs/plugins)<br>
  Wookie provides much functionality (even what some consider core features) in
  the form of plugins.
- [Core plugins](/docs/core-plugins)<br>
  Learn how to use the core plugins to make your experience with Wookie easier.
- [Hooks](/docs/hooks)<br>
  Hooks allow you to tie in to specific points of execution in the processing of
  a request.
- [Writing plugins](/docs/writing-plugins)<br>
  Learn how to write your own plugins for Wookie to extend the functionality to
  your liking.

<a id="quick-start"></a>
Quick start
-----------
Here are some quick examples to get you started using Wookie. Remember that
Wookie runs on top of a [cl-async](http://orthecreedence.github.com/cl-async) event loop, so you must start
Wookie from within an event loop for it to work.

### Starting Wookie
This example shows how to set up a very basic website using Wookie. It uses a
[listener](/docs/listeners#listener), which describes how people can connect to
the server, and passes it to [start-server](/docs/listeners#start-server).

This example also shows how to load Wookie's [plugins](/docs/plugins),
which provide key functionality such as parsing GET/POST parameters, file and
directory serving, etc.

```lisp
(universal-time-to-rfc822 (get-universal-time)) ;=> "Tue, 09 Apr 2013 14:04:52 +0800"

(defpackage :my-website
  (:use :cl :wookie :wookie-plugin-export))
(in-package :my-website)

;; load Wookie's core plugins. much of Wookie's functionality is written as
;; plugins which can be enabled/disabled/overwritten it desired.
(load-plugins)

;; setup a simple homepage
(defroute (:get "/") (req res)
  (send-response res :body "Welcome to my website!"))

;; setup a mapping for the url / to a local directory assets. a request for
;; 
;;   /images/background.jpg
;;
;; will load "./assets/images/background.jpg"
(def-directory-route "/" "./assets")

;; create a listener that accepts connections on port 80
(let ((listener (make-instance 'listener
                               :bind nil  ; equivalent to "0.0.0.0" aka "don't care"
                               :port 80)))
  ;; start an event loop and pass our listener to Wookie's start-server method
  (as:start-event-loop
    (lambda () (start-server listener))
    ;; it's generally a good idea to catch errors here (otherwise they bubble
    ;; up to the REPL and Wookie won't have a chance to handle them)
    :catch-app-errors t))
```
