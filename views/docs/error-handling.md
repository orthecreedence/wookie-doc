---
title: Error handling | Documentation
layout: documentation
---

Error handling
==============
Error handling is always a sore spot for asynchronous systems, and Wookie is no
different. Any time an asynchronous action occurs, the entire stack unwinds,
undoing your error handling. Once the action finishes, it will continue at the
point you tell it to, but with a completely new stack devoid of any error
handling.

It's really important that errors be caught every step of the way, because a
rogue condition or error can be very difficult to trace. It won't be at all
obvious where/how it was triggered because it got its own stack and the debugger
won't be able to help much. This could leave the client waiting for a response
that will never come, or in worse cases, a completely non-responsive server.

These problems can be mitigated somewhat by using [cl-async's futures](http://orthecreedence.github.com/cl-async/),
which provide [error handling abstraction](http://orthecreedence.github.com/cl-async/future#error-handling)
that make async programming a bit nicer. However, while I encourage the use of
futures in async programming as much as possible, sometimes they are not the
correct way to solve a problem.

__The best way to handle errors in Wookie is to catch any possible conditions in
your code without letting them escape to top level.__

{{toc}}

### Catching errors before they reach top-level
While you can setup a global error handler (in both cl-async *and* Wookie) to
keep your app from crashing in the event of an uncaught error/condition, it's
best to keep uncaught errors/conditions from ever happening.

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
Cl-async allows you to set a default event/error handler when you start your
event loop. As mentioned, it's better to have context-specific event handlers
for each of your async actions (via the `event-cb`) but a global handler can do
a lot to keep errors from hitting the REPL and grinding things to a halt.

In cl-async you can set the default error handler like so (making sure
`:catch-app-errors` is `t`):

```lisp
(as:start-event-loop
  (lambda () ...)
  :default-event-cb 'app-error-handler
  :catch-app-errors t)
```

Since Wookie installs its own error handler to the TCP listener it opens, we
have to explicitely tell Wookie to use this handler as well:

```lisp
(setf wookie:*error-handler* 'app-error-handler)
```

Once these are both set up, any errors that were not caught in your async
`event-cb` callbacks will be routed through the function `app-error-handler`.
Your `app-error-handler` function can look for any errors/conditions it sees
fit (it will be different for every application), but generally you'd do
something like this:

```lisp
(defun app-error-handler (err)
  (handler-case (error err)
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

### \*error-handler\*
This global variable holds a function of exactly one argument and allows you to
tell Wookie that you want all errors it catches while processing to be routed to
the given function.

If this is left as `nil`, then Wookie will rethrow any errors it doesn't know
how to handle (this means any non-wookie-specific errors will be rethrown).

