---
title: Error handling | Documentation
layout: documentation
---

Error handling
==============
Error handling is where asynchronous systems have trouble. Wookie is no
different. While it's possible to set a global error handler for your
application, per-route error handling is difficult because for each async
action triggered, a new stack is created and any error handling will be lost
with the old stack. Uncaught conditions thrown will be caught by the global
app error handler, where you will have very little context about the error.
In most cases, you won't have access to the [response object](/docs/request-handling#response)
for that request, meaning you won't be able to send a response and the
client will hang until the connection times out. No bueno.

These problems can be mitigated somewhat by using [cl-async's futures](http://orthecreedence.github.com/cl-async/),
which provide [error handling abstraction](http://orthecreedence.github.com/cl-async/future#error-handling)
that make async programming a bit nicer. However, while I encourage the use of
futures in async programming as much as possible, sometimes they are not the
correct way to solve a problem.

__The best way to handle errors in Wookie is to catch any possible conditions in
your code without letting them escape to top level.__

{{toc}}

### Catching errors before they reach top-level
While you can setup a global error handler to keep your app from crashing in the
event of an uncaught error/condition, it's best to keep uncaught
errors/conditions from ever happening.

The best way to do this is to be very meticulous about error handling in your
routes and any code the routes call. Whenever an async action occurs, you *will*
lose all error handling you've established already, and you'll have to set up
new error handlers.

As mentioned, this can be mitigated somewhat by using [future-handler-case](http://orthecreedence.github.com/cl-async/future#error-handling),
but this will only work well on code that uses futures as an async interface.
Say you are doing direct TCP calls, you'll have to explicitely setup good error
handling in your `event-cb` that you pass to your async functions. Be *sure* to
read and understand the [application error handling section of the cl-async
docs](http://orthecreedence.github.com/cl-async/event-handling).

### Setting up a global error handler
Wookie has no facilities for setting up a global error handler, mainly because
cl-async already provides this ability when you start your event loop:

```lisp
(as:start-event-loop
  (lambda () ...)
  :default-event-cb 'app-error-handler
  :catch-app-errors t)
```
So we specify the default handler as `app-error-handler`, and also specify that
we want cl-async to catch *any* errors it can and forward them to this handler.
Note that this only happens when an event occurs inside of an action that
doesn't have an explicit `event-cb` set on it (otherwise errors are sent to
that event callback instead of the global handler).

Your `app-error-handler` function can look for any errors/conditions it sees
fit (it will be different for every application), but generally you'd do
something like this:

```lisp
(defun app-error-handler (err)
  (handler-case (throw err)
    ;; TCP eof, happens all the time
    (as:tcp-eof ()
      (format t "EOF on socket ~a~%" (as:tcp-socket err)))
    ;; general cl-async error, probably want to take notice
    (as:event-error ()
      (omg-got-a-cl-async-error!!))
    ;; was not an event-error, just info, let's ignore it (pretty safe bet)
    (as:event-info () nil)
    ;; catch any other errors
    (error ()
      (format t "uncaught error: ~a~%" err)))))
```

