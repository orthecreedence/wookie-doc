---
title: Error handling | Documentation
layout: documentation
---

Error handling
==============
{{toc}}

This covers how Wookie handles errors.

### Listener's event-cb
Error handling in Wookie is done by passing an event/error handler function to
your listener on instantiation:

```lisp
(make-instance 'listener :port 8181 :event-cb 'my-app-error-handler)
```

The handler is a function of two arguments, the event/error object that was
caught and the cl-async socket we're processing on. You can grab the
[request/response](/docs/request-handling) objects out of the socket using
cl-async's `socket-data` accessor:

```lisp
(defun my-app-error-handler (err sock)
  (let* ((sockdata (when sock (as:socket-data sock)))
         (response (getf sockdata :response))
         (request (when response (response-request response))))
    ...))
```

Any errors caught *by Wookie* during normal operation will be handed to the
given event handler. If a handler is not specified, Wookie will take some
default actions with the caught error (such as send an `HTTP 500` response).

Note that it's possible for errors to occur in your app that Wookie will not
catch, for instance:

```lisp
(defroute (:get "/") (req res)
  (as:with-delay ()
    (error "wookie will not catch this")))
```

In such cases, you will want to pass an error handler to cl-async's
`:catch-app-errors` startup keyword:

```lisp
(as:with-event-loop (:catch-app-errors (lambda (ev) (my-app-error-handler ev nil)))
  ...)
```

### \*debug-on-error\*
This is a veriable defined in the `wookie-config` package that basically tells
Wookie if it encounters an error, let the error pass through to the debugger...
don't try to catch it and hand it off to the listener's `event-cb`.

This can be really useful for apps being developed locally in debug mode because
you'll get full stack traces for your errors:

```lisp
(let ((wookie-config:*debug-on-error* t)
      (blackbird:*debug-on-error* t))
  ;; notice we omit :catch-app-errors here
  (as:with-event-loop ()
    (start-server (make-instance 'listener :port 80))))
```

Notice that this pairs very will with setting blackbird and cl-async into
pass-through mode as well.

