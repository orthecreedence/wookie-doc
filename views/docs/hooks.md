---
title: Hooks | Documentation
layout: documentation
---

Hooks
=====
Hooks provide tie-ins to specific points of execution during the processing of
a request. Hooks are the foundation on which plugins are built, but can be used
outside of plugins.

For instance, if you want to run initialization code for every incoming request,
or do some sort of cleanup after a response goes out, you can use hooks.

{{toc}}

### add-hook (function)
```lisp
(defun add-hook (hook function &optional hook-name))
  => nil
```
Tie into a hook, running the given function whenever that hook is called by
Wookie.

Optionally, you can name your hook, which makes it much easier to remove later
on if you need to.

Here's a simple example where we make sure an incoming request is acceptable,
and also name the hook `:check-request`:
```lisp
(add-hook :pre-route
  (lambda (req res)
    (declare (ignore res))
    (my-app:make-sure-request-is-ok req))
  :check-request)
```

### remove-hook (function)
```lisp
(defun remove-hook (hook function/hook-name))
  => nil
```
Takes a hook name and either the function the hook being removed calls **or**
the name of the hook (specified during [add-hook](#add-hook)) and removes the
hook from the system so it won't be called anymore.

To remove the hook we added in the [add-hook](#add-hook) example:
```lisp
(remove-hook :pre-route :check-request)
```

If you add hooks with anonymous lambdas, naming can be very useful during the
removal process so you don't have to track your lambdas.

### Provided hooks
This is a list of *all* hooks the wookie system allows tying into, along with
the arguments each one will be called with.

##### :connect
```lisp
(lambda (socket) ...)
```
Called directly after a client connects. The `socket` is a [cl-async socket
object](http://orthecreedence.github.com/cl-async/tcp#socket).

##### :pre-route
```lisp
(lambda (request response) ...)
```
Called directly before the routing of a request takes place.

##### :post-route
```lisp
(lambda (request response) ...)
```
Called directly after a request has been routed.

##### :parsed-headers
```lisp
(lambda (request) ...)
```
Called directly after the headers of a request have been parsed.

##### :body-chunk
```lisp
(lambda (request chunk-data last-chunk-p) ...)
```
Called directly after a chunk in a chunked request comes in. `chunk-data` is a
byte array, `last-chunk-p` specifies if the current call is the *last* chunk
coming in for this request.

##### :body-complete
```lisp
(lambda (request) ...)
```
Called directly after the full body of an incoming request has been parsed
(chunked or not).

##### :response-started
```lisp
(lambda (response request http-status http-headers http-body) ...)
```
Called directly before a response is started. For instance will be called when
either [send-response](/docs/request-handling#send-response) or
[start-response](/docs/request-handling#start-response).

Note that the `http-headers` variable represents the headers as they exist in
the `response` object, not necessarily *all* the headers that will be sent
(since more can be specified via the `:headers` keyword in send/start-response).
This may change to include *all* headers to be sent sometime in the future.
