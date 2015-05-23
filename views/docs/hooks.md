---
title: Hooks | Documentation
layout: documentation
---

Hooks
=====
{{toc}}

Hooks provide tie-ins to specific points of execution during the processing of
a request. Hooks are the foundation on which plugins are built, but can be used
outside of plugins.

For instance, if you want to run initialization code for every incoming request,
or do some sort of cleanup after a response goes out, you can use hooks.

### Hooks and promises
A useful feature of hooks is that if a hook function returns a
[promise](http://orthecreedence.github.io/blackbird), Wookie will
wait for that promise to resolve before continuing execution on the current
request.

For instance, if you need to check a user's authentication against your database
and only want to continue processing the request if the info checks out, you 
could hook into the `:pre-route` hook and return a promise that's resolved once
the user's auth info checks out. Then when the route for the current request is
loaded, that user is already authenticated.

Note that if multiple functions on the same hook return promises, Wookie will
wait for *all* of those promises to resolve before continuing processing.

See notes on [using chunking and promises with :pre-route](/docs/hooks#pre-route)
below.

### Error handling
Wookie's hooks are promise-aware as explained above, but Wookie also watches the
promises for errors. If any of the promises Wookie's hook system is waiting on
is rejected, Wookie will *cancel/destroy* the current request __without__
sending a response.

Let's say you're checking user authentication info in `:pre-route`, but the auth
info is wrong and you don't want the user to even get to the route (why waste
the CPU cycles on some deadbeat?)...you can send a 401 HTTP response via
[send-response](/docs/request-handling#send-response) and then reject the hook
function's returned promise.

As mentioned, when an error is passed back from a hook function, the request,
its data, its callbacks, etc etc will all be obliterated. Send back your
error response *before* signaling an error or the client will be left hanging.

It's important to note that Wookie ignores the actual error object on rejected
hook promises. You could just send `t` instead of an error object if you want
to, but it's best to be more descriptive since there's a chance the error object
may be used for something in the future.

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

Here's an example using promise:
```lisp
(add-hook :pre-route
  (lambda (req res)
    (with-promise (resolve reject)
      ;; grab our user from the database
      (alet ((user-info (my-app:get-user-from-db)))
        (if user-info
            ;; got auth info! continue the request
            (progn
              ;; allow other parts of the app access to the user info through request-data
              (setf (request-data req) user-info)
              ;; let the hook system know we're done here
              (resolve))
            ;; bad auth info (blank user returned). send a "nope" response and boot them
            (progn
              (send-response res :status 401 :body "User auth failed!")
              ;; discontinue the request
              (reject (make-instance 'error))))))))
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

`:pre-route` is probably the best place to check application authentication. By
this time, you have all the GET variables and all your request headers
(including the `Authorization` header). `:pre-route` hook functions can return a
promise that resolves either with a value (ignored) or has an error triggered on
it depending on whether authentication was successful or not.

However, this poses some problems. Let's say you want to support large file
uploads in your app:

```lisp
(defroute (:post "/files" :chunk t) (req res)
  (with-chunking (chunk last-chunk-p)
    ;; save file to storage system chunk by chunk
    ))
```

However, if you check auth on this call in `:pre-route` via a promise, it's very
possible that the client will start sending the body chunks *before* your
[with-chunking](/docs/request-handling#with-chunking) handler is called, meaning
body chunks that come in in the meantime are gone. Forever.

Don't lose hope though, eager app developer! You can specify to your route that
you want to tell the client to wait to send the body until we say it's ok:

```lisp
;; note the :suppress-100 t here, which tells Wookie to not blindly send a
;; "100 Continue" header for us (we'll be doing it manually in our route).
(defroute (:post "/files" :chunk t :suppress-100 t) (req res)
  ;; IF the client sent an "Expect: 100-continue" header, send back our, "yes
  ;; please continue" header, instructing the client to send the body now that
  ;; our route is set up.
  (when (string= (gethash "expect" (request-headers req))
                 "100-continue")
    (send-100-continue res))
  (with-chunking (chunk last-chunk-p)
    ;; save file to storage system chunk by chunk
    ))
```

Looks great! Right? Not quite there yet. There are two things that could go
horribly wrong:

1. The client could just not wait for us to send "100 Continue", in which case
we wind up with the same problem as before. It's in the HTTP spec that a client
must not wait indefinitely for the "100 Continue" header to come through if it
sends an "Expect: 100-continue" in the request. The length is waits is up to the
client, so we have to be prepared for this possibility.
1. The client could just not send chunked data to this route (ie, it doesn't
send "Expect: 100-continue" and doesn't use "Transfer-encoding: chunked"),
meaning we don't have a chance to tell it to wait for us to send the body over.

However, you have one more trick up your jedi sleeve:

```lisp
;; note here we specify both :suppress-100 *and* :buffer-body
(defroute (:post "/files" :chunk t :suppress-100 t :buffer-body t) (req res)
  ;; IF the client sent an "Expect: 100-continue" header, send back our, "yes
  ;; please continue" header, instructing the client to send the body now that
  ;; our route is set up.
  (when (string= (gethash "transfer-encoding" (request-headers req))
                 "chunked")
    (send-100-continue res))
  (with-chunking (chunk last-chunk-p)
    ;; save file to storage system chunk by chunk
    ))
```

__NOTE:__ As of Wookie 0.3.6 `:buffer-body` is `t` by default. Without it, data
would be lost into the ether which doesn't scream of sane defaults.

So we added `:buffer-body t` to our route options, meaning that if the client
starts chunking for any reason before we call `with-chunking`, all lost chunks
will be saved into a buffer (in order) and passed as one large chunk once our
route calls `with-chunking`. Even if the client doesn't chunk the body or the 
client does chunk and every chunk comes in before we set up our chunking
handler, the handler will always be called with the missed data (and
`last-chunk-p` will still be accurate).

While this is all fine and great, it means that your app server is going to be
saving large chunks of data in memory while waiting for your DB to respond, so
keep those auth queries simple and fast!

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
