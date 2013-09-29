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

### Setting up an app error handler
Wookie has an internal event handler it uses to catch errors. You can tie into
this handler and override its defaults by specifying your own handler function.
This function takes two arguments: the event/error object and a [cl-async socket
object](http://orthecreedence.github.io/cl-async/tcp#socket).

The socket object is interesting because it allows you to access some other
objects that might be useful for debugging purposes. For instance the
Wookie stores the [request/response](/docs/request-handling) objects in the
socket's `cl-async:socket-data` accessor. Depending on where/when the error
was caught, you can access these objects (example below).

Here's how to install your custom error handler:

```lisp
(setf wookie:*error-handler* 'app-error-handler)
```

Here's what an app error handler might look like. Notice, as mentioned above,
we're grabbing the `response` object from the socket data and sending a failure
response to our client. This can mean the difference between an app that is
responsive (albeit broken) and an app that lets the client wait indefinitely
(bad).

```lisp
(defun app-error-handler (err socket)
  ;; grab our response object from the socket (keep in mind it may be null, so
  ;; check before using it)
  (let* ((socket-data (when socket (as:socket-data socket)))
         (response (getf socket-data :response)))
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
        (format t "uncaught error: ~a~%" err)))
    ;; if we have a response object AND it hasn't been responded on yet AND we
    ;; aren't dealing with some lowly info event, notify the client
    (when (and response
               (not (response-finished-p response))
               (typep err 'as:tcp-info))
      (send-response response :status 500 :body "There was an error processing your request."))))
```

Note that if you make the `socket` arg `&optional` you could easily share this
error handler with your [default event handler](http://orthecreedence.github.io/cl-async/event-handling#default-event-handler)
in cl-async.

### \*error-handler\*
This global variable holds a function of exactly two arguments and allows you to
tell Wookie that you want all errors it catches while processing to be routed to
the given function. The function takes an error/event condition as its first arg
and a cl-async socket as the second.

If this is left as `nil`, then Wookie will rethrow any errors it doesn't know
how to handle (this means any non-wookie-specific errors will be rethrown).

