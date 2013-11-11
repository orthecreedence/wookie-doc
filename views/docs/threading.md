---
title: Threading | Documentation
layout: documentation
---

Threading
=========
There are instances where you want to run multiple instances of Wookie in the
same process. For instance, if you're running an app server with four cores, you
can make use of all four cores by starting Wookie on four separate ports in the
same lisp process. This will give you the best of both worlds: asynchronous HTTP
handling that utilizes all of your server's cores. Keep in mind that you'll most
likely need a reverse proxy to distribute requests to the different ports you
open (see [best practices](/best-practices)).

However, doing so requires some special attention. As of version 0.3.3, Wookie
stores all server state in one global object: `wookie:*state*`. This object is
by default created for you while loading Wookie, but you can also create your
own instance of it via the `wookie-state` class. This allows you to create a
thread-local variable in each of your threads via `let` that will keep your
Wookie threads from sharing state (and possibly running into corruption issues
because of it):

```lisp
(defun start-wookie ()
  ;; this let turns wookie:*state* into a thread-local variable
  (let ((wookie:*state* (make-instance 'wookie:wookie-state)))
    ;; load our plugins
    (wookie:load-plugins)
    ;; load our homepage route
    (wookie:defroute (:get "/") (req res) (wookie:send-response res :body "Thanks for stopping by."))
    (as:with-event-loop ()
      (wookie:start-server (make-instance 'wookie:listener :port 6969)))))

(dotimes (i 4)
  ;; assumes you have bordeaux-threads loaded
  (bt:make-thread 'start-wookie))
```

Note that this example reloads the routes and the plugins for each thread. Since
routes and plugins are by default read-only, you can set them up once and then
set them into your global state.

```lisp
(defun start-wookie ()
  ;; this let turns wookie:*state* into a thread-local variable
  (let* ((routes (wookie:wookie-state-routes wookie:*state*))
         (plugins (wookie:wookie-state-plugins wookie:*state*))
         ;; create a new state, but store our routes/plugins from the original,
         ;; global state in it. this lets us load our routes/plugins once, and
         ;; use them in each thread.
         (wookie:*state* (make-instance 'wookie:wookie-state
                                        :routes routes
                                        :plugins plugins)))
    (as:with-event-loop ()
      (wookie:start-server (make-instance 'wookie:listener :port 6969)))))

;; load our plugins
(wookie:load-plugins)

;; load our homepage route
(wookie:defroute (:get "/") (req res) (wookie:send-response res :body "Thanks for stopping by."))

(dotimes (i 4)
  ;; assumes you have bordeaux-threads loaded
  (bt:make-thread 'start-wookie))
```

Note that if you dynamically load or define routes/plugins while the threads are
running, you run the risk of data corruption.

