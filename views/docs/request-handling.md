---
title: Request handling | Documentation
layout: documentation
---

Request handling
================
{{toc}}

Starting a server and setting up routes is kewl, but unless you know what to do
once an actual request comes in, it's all just an exercise in academia.

Wookie supports simple request/response handling, but also has the ability to
stream incoming/outgoing chunked HTTP content.

### request (class)
The request class holds information about an incoming request: the socket the
request happened on, the HTTP method/resource/headers/etc.

It is passed to all routes defined by [defroute](/docs/routes#defroute).

##### request-socket (accessor)
Accessor for the request's [cl-async socket](http://orthecreedence.github.com/cl-async/tcp#socket).
Can be used to send data directly to the client if needed, although Wookie
provides interfaces for most of the communication you'll need.

##### request-method (accessor)
The HTTP method (`:get`, `:post`, `:delete`, etc) for the incoming request.

##### request-resource (accessor)
The resource being requested. For example `/docs/search?class=listener` (the
entire resource is gathered, even the GET vars).

##### request-headers (accessor)
The HTTP headers that came in with this request. The headers are in hash-table format:
```lisp
#{:host "musio.com"
 :accept "text/html"
 :connection "close"}
```

##### request-data (accessor)
Used to store arbitrary data within a request object.

This comes in handy for things like user authentication. You may have a
`:pre-route` [hook](/docs/hooks) that looks up a user based on the auth headers,
and you could store that user into in `request-data` so that whatever route
loads will have access to the info and won't have to look it up again.

##### request-uri (accessor)
This is a [quri](http://quickdocs.org/quri/) object of the parsed HTTP
[request resource](#request-resource).

##### request-http (accessor)
This is the raw [http-request](https://github.com/fukamachi/fast-http#structure-http-request-extends-http)
object. It's created by [fast-http](https://github.com/fukamachi/fast-http),
the HTTP parsing library used by Wookie.

##### request-data (accessor)
This accessor allows passing of arbitrary data along with a request. For example
a `:pre-route` [hook](/docs/hooks) could process part of the request and save it
so that later routes would have access to it.

##### request-store-body (accessor)
This boolean value determines whether or not we should save the HTTP body of the
incoming request (used to parse out POST variables or file data).

Generally, it's better to stream this data using [chunked handling](/docs/request-handling#with-chunking),
but many times you just want to gather simple post data (perhaps for use with
[Wookie's core post plugin](/docs/core-plugins#post)).

Default `t` (disable in [:pre-route](/docs/hooks#pre-route) if you want tighter control over memory consumption)

### response (class)
The response class is used to send a response from Wookie to the connecting
client by passing it to [send-response](#send-response) or [start-response](#start-response).

It is passed to all routes defined by [defroute](/docs/routes#defroute).

##### response-request (accessor)
This is a convenience accessor to get the [request](/docs/request-handling#request)
object attached to the response.

```lisp
(when (as:socket-closed-p (request-socket (response-request response)))
  ...)
```

##### response-headers (accessor)
This is the accessor for the headers to send back to the client. They are in plist
format:
```lisp
(:content-type "application/json"
 :cache-control "max-age=10, public")
```

To specify multiple headers with the same name, the value can be a list instead
of a string:
```lisp
(:set-cookie ("user=rick; Expires=Wed, 09-Jun-2021 10:18:14 GMT")
             ("session=abc123"))
```

##### response-finished-p (accessor)
This stores whether or not the response has been sent already. Only one response
can be sent per request. Wookie can and will, however, re-use a connection after
a response has been sent if the client allows it.

### send-response (function)
```lisp
(defun send-response (response &key (status 200) headers body (close nil close-specified-p)))
  => response
```
This function sends a response to the client. It takes the response object
passed into a route. When using `send-response`, the response is sent all at
once (as opposed to chunking, which can be done using [start-response](#start-response)
and [finish-response](#finish-response).

`:status` specifies the HTTP status code to send back (200, 404, etc).

`:body` is used to send a body payload back to the client. When `:body` is
specified, the "Content-Length" header is automatically populated *unless it is
already present in either the [response object's headers](#response-headers) or
send-response's `:headers` parameter*.

`:headers` is used to specify any extra headers to send back to the client, in
addition to the headers passed into the [response object](#response-headers). A
few headers are automatically passed back to the client, but can be overridden
by the response object's headers or the `:headers` keyword if needed:
```lisp
(:server "Wookie (0.1.2)"
 :date "Fri, 22 Mar 2013 22:07:59 UTC")
```

<a id="send-response-close-keyword"></a>
`:close` is used to tell Wookie to close the connection (or keep it open) after
the response is sent. If not specified, Wookie will read the "Connection" header
to determine whether or not to close the connection. In other words, feel free
to leave this blank unless you have a good reason not to.

`send-response` returns the same response object passed in.

Here's an example `send-response` usage:

```lisp
(defroute (:get "/") (req res)
  (send-response res
                 :headers '(:content-type "text/html")
                 :body (load-html-view :homepage)))
```

`send-response` can throw the following errors:

- [response-already-sent](#response-already-sent-condition)
- [socket-closed](http://orthecreedence.github.io/cl-async/tcp#socket-closed)

### start-response (function)
```lisp
(defun start-response (response &key (status 200) headers))
  => chunga:chunked-output-stream
```
This function allows you to send a response back to the client in chunks. It
sends the headers stored in the [response object](#response-headers) along
with those passed in `:headers`, and returns a chunked stream (courtesy of
[chunga](http://weitz.de/chunga/)).

The body of the response is written by sending data into the chunked stream. It
must be binary data, but you can wrap the chunga stream in a
[flexi-stream](http://weitz.de/flexi-streams/) if you want to send encoded
string-data (but be sure to specify your flex stream encoding matches the
"Content-Type: my/type; encoding=xxx" header you send back).

Note that `start-response` calls [send-response](#send-response) to send all the
needed headers back to the client.

`start-response` automatically marks the response to support chunking by sending
the "Transfer-Encoding: chunked" header.

Once you are done sending your chunked data, you must call [finish-response](#finish-response)
with the same response object you passed into `start-response`.

Let's do an example:

<a id="start-response-example"></a>
```lisp
(defroute (:get "/my-file") (req res)
  (let ((stream (start-response res :headers '(:content-type "application/javascript"))))
    ;; `load-file-in-chunks` is not a part of Wookie, this is just for demonstration
    (my-app:load-file-in-chunks "file.js"
      (lambda (contents-sequence)
        (write-sequence contents-sequence stream)))
    (finish-response res)))
```

### finish-response (function)
```lisp
(defun finish-response (response &key (close nil close-specified-p)))
  => response
```
This function finishes up a chunked response (started with [start-response](#start-response))
by flushing the chunked stream and ensuring any data on it is sent out.

The `:close` keyword can be used to specify whether or not Wookie should close
the connection (or keep it open), but as with [send-response](#send-response-close-keyword),
Wookie is more than capable of determining this automatically, so don't specify
it unless you have a good reason.

`finish-response` returns the same response object passed in.

See the [start-response example](#start-response-example) for usage.

### with-chunking (macro)
```lisp
(defmacro with-chunking (request (chunk-data last-chunk-p &key start end store-body) &body body)
```
This macro is used to support an incoming request that's chunked. For instance,
let's say a user wants to upload a file to you, and instead of storing the
entire file in memory while it's uploading, you want to stream it off to a file
storage system (like Amazon's S3 for instance) piece by piece.

This macro is meant to be used in conjuction with a route that has chunking
enabled: `(defroute (:post "/files" :chunk t) ...)`. It takes the request object
passed into the route and sets up a handler in that request for chunked data to
be passed into. The `body` can be called multiple times, one for each chunk of
incoming data.

`chunk-data` is a octet (byte) sequence holding the data for the incoming chunk.

`last-chunk-p` is a boolean. If it's `nil`, expect more chunks on the way. If
it's `t`, then `chunk-data` is the last chunk in the data, and you should do
any cleanup you need to.

The `:start` and `:end` keywords let you specify *bindings* that will hold the
start and end positions for `chunk-data`. If either `:start` or `:end` are not
specified, then `chunk-data` will be a subseq (a copy) of the array passed in by
the HTTP parser. Basically, you should probably specify the `:start`/`:end`
keywords to avoid a needless copy, but if you don't, Wookie will fall back to
its old default behavior (which is that `chunk-data` has exactly what you need
in it).

By default, `with-chunking` will disable storing the HTTP body on any request
we set up chunking for (see [request-store-body](/docs/request-handling#request-store-body)).
The `:store-body` keyword (boolean) lets you specify that you want to chunk this
request *and* you also want to buffer the body. Keep in mind that if this is set
to `t` and the HTTP body size exceeds [\*max-body-size\*](/docs/config#max-body-size),
the request will be stopped.

This is all confusing, so let's see an example where we stream an upload, chunk
by chunk, to S3 using a fictional (but plausible) uploader object:

```lisp
(defroute (:post "/files" :chunk t) (req res)
  (let ((s3-uploader nil))
    (with-chunking req (chunk-bytes finishedp)
      ;; create the fictional uploader
      (unless s3-uploader
        (setf s3-uploader (my-app:make-async-s3-uploader)))
      ;; send the chunk to the uploader
      (my-app:send-chunk-to-s3 s3-uploader chunk-bytes)
      ;; once finished, cleanup and send the response to the client
      (when finishedp
        (my-app:finish-s3-upload s3-uploader)
        (send-response res :body "Upload processed, thxlolol")))))
```

### send-100-continue (function)
```lisp
(defun send-100-continue (response))
  => nil
```
This function sends a `100 Continue` header on the given response's socket. This
is mainly meant to be used in conjunction with [defroute's](/docs/routes#defroute)
`:suppress-100` option. You can probably safely ignore this function (along with
`:suppress-100`) since Wookie handles everything properly for you anyway. there
may however be cases where you want fine-tuned control of where/when a `100
Continue` is sent though (ie, checking a database or something).

If there is no `Expect: 100-continue` header, sending this will probably make
your client confused, so be sure to check first.

Note that if you don't call `send-100-continue` within a second or so of the
request coming in, the client will most likely just start dumping the body on
you without waiting (this is specified in the HTTP 1.1 RFC).

### get-socket (method)
This helper method takes either a [request](#request-class) object or a 
[response](#response-class) object and returns the [cl-async socket](http://orthecreedence.github.io/cl-async/tcp#socket)
the given request came in on.

### \_method GET var
When calling Wookie via AJAX, it can be useful to specify more than just your
standard GET/POST verbs. Since browsers are completely undecided about which
ones support which verbs from an XHR request, we work around it by copying the
Rails framework: passing a GET var called `_method`.

    http://mysite.com/api/albums/1234?_method=DELETE

The above URL would match the following route:

```lisp
(defroute (:delete "/api/albums/([0-9]+)") (req res args)
  ...)
```

Note that `\_method` is case INsensitive. Also note that `\_method` *must be
passed in a GET parameter!* If you pass it as part of the POST body, it *will be
ignored*.

### response-error (condition)
This describes an error that occurs while sending a response.

##### response-error-response (accessor)
Allows access to the [response](#response) class the error occured on.

### response-already-sent (condition)
_extends [response-error](#response-error)_

This error is thrown when a response is sent over the same [response object](#response)
more than once. Only one response can be sent per request.

