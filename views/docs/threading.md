---
title: Threading | Documentation
layout: documentation
---

Threading
=========
{{toc}}

Sometimes you'll want to run multiple instances of Wookie in the same lisp
process, a thread for each Wookie. This allows you the best of both worlds: an
async app server that utilizes all the cores of the machine you're on.

There are two basic ways you can do this. You can run the same app on multiple
ports (a thread for each port) or different apps on different ports.

Wookie tracks all of its state (hooks, routes, plugins) in a global variabe,
`wookie:*state*`, which is an instance of the `wookie-state` class. This can be
bound to a thread-local variable (if needed), giving you a safe way to keep
states of different threads from bumping into each other.

Note that if using multiple ports, you'll likely need some sort of reverse proxy
to distribute incoming requests to your app (see [best practices](/docs/best-practices)).

### Same app, multiple ports
This is the easiest way to run multiple Wookie instances: you load your plugins,
load your app-specific hooks, and define all your app's routes. This happens on
load (not when you start the server). This way by the time you actually start
your threads, all the state is set up and from here on out, Wookie will only be
reading values from the state, not writing them...meaning all your threads can
use the global state without fear of corruption.

However, you *cannot* modify hooks, routes, or plugins once your setup is done!
If you do this, you're asking for trouble.

Here's an example:

```lisp
(load-plugins)

(defroute (:get "/") (req res)
  (send-response res :body "Welcome to my homepage!"))

(defun start-instance (port)
  (bt:make-thread
    (lambda ()
      (as:with-event-loop ()
        (start-server (make-instance 'listener :port port))))))

;; all the setup is done! from here on out, our state will be read only. now
;; start our instances, all of which will use the global wookie:*state*
(loop for port from 8000 to 8003 do
  (start-instance port))
```

Easy right? Just remember, don't change your server state once the app threads
are started!

### Different apps, multiple ports
There may be times when you want different apps running on different ports and
you want to use one lisp instance to do so. In this case, you want to give each
thread its own instance of the `wookie-state` class. This can be done easily
using a `let` form wrapping the inside of our threads, which creates a
thread-local variable and ensures that when a thread references `wookie:*state*`
it is reading its own verison, not the global version.

To do this, you'll want to set up your plugins, hooks, and routes *inside* each
thread (after binding your thread-local state variable).

__NOTE:__ ASDF is a whiny little baby when it comes to threading, and Wookie's
`load-plugins` function uses ASDF. What this means is that when you call
`load-plugins` in each thread, you'll have to implement locking to keep the
threads from calling it at the same time.

Here's an example:

```lisp
(defparameter *plugin-lock* (bt:make-lock))

(bt:make-thread
  (lambda ()
    ;; this let makes wookie:*state* a thread-local variable
    (let ((wookie:*state* (make-instance 'wookie-state)))
      ;; lock our ASDF ops!!
      (bt:with-lock-held (*plugin-lock*)
        (load-plugins))
      (defroute (:get "/") (req res) (send-response res :body "Welcome to app1!!"))
      (as:with-event-loop ()
        (start-server (make-instance 'listener :port 8000))))))

(bt:make-thread
  (lambda ()
    ;; this let makes wookie:*state* a thread-local variable
    (let ((wookie:*state* (make-instance 'wookie-state)))
      ;; lock our ASDF ops!!
      (bt:with-lock-held (*plugin-lock*)
        (load-plugins))
      (defroute (:get "/") (req res) (send-response res :body "This is app2."))
      (as:with-event-loop ()
        (start-server (make-instance 'listener :port 8001))))))
```

