---
title: Routes | Documentation
layout: documentation
---

Routes
======
Routes allow you to map a URL to a block of code to be run when that URL is
requested.

The Wookie routing system uses regular expression routing (the default) or exact
match string routing. Wookie routes on the HTTP method of the request (`GET`,
`POST`, `PUT`, `DELETE`, etc) and the resource being requested (ie the URL).

Routes are added to the routing table via [defroute](#defroute) (and are added
to the end of the table). When searching for a matching route when a request
comes in, routes at the beginning of the routing table are given preference.

Routes are capable of handling streaming (chunked) data. Routes can also tell
the router that they are the wrong route for the job and that the routing
system should use the next matching route (known as "route jumping," done via
the [next-route](#next-route) function).

- [defroute](#defroute) _macro_
- [with-vhost](#with-vhost) _macro_
- [clear-route](#clear-route) _function_
- [clear-routes](#clear-routes) _function_
- [next-route](#next-route) _function_
- [route-error](#route-error) _condition_
- [route-not-found](#route-not-found) _condition_

<a id="defroute"></a>
### defroute (macro)
```lisp
(defmacro defroute ((method resource &key (regex t) (case-sensitive t)
                                          chunk replace
                                          vhost)
                    (bind-request bind-response &optional bind-args)
                    &body body))
```

This sets up a route in the routing system such that when a request comes in
that matches on the method/resource, the body will be run.

`:regex` specifies whether the `resource` is a regular expression (matched with
[cl-ppcre](http://weitz.de/cl-ppcre/)). If a regular expression is selected
(the default) then regular expression groups can be accessed from the route via
the `bind-args` argument. `:case-sensitive` specifies if the route's regex is
case-sensitive.

`:chunk` tells us that this route is more than willing to stream chunked
content. In other words, we'll set up a handler in our route using [with-chunking](/docs/request-handling#with-chunking)
to stream content over HTTP.

`:replace` tells the routing system that this route should replace the first 
route with the same method/resource in the routing table.

The `:vhost` keyword specifies that this route should only load for a specific
`Host: ...` header. This is a string in the format "host.com" or
"host.com:8080". If a port is not specified, the route will match on *any* port
provided the host matches.

`bind-request` and `bind-response` are the variables we want to be available to
the route body that hold our respective [request](/docs/request-handling#request)
and [response](/docs/request-handling#response) objects for the incoming
request. If `bind-args` is passed, it will be a list that holds all the matched
groups from the `resource` regex.

Let's dive in with a few examples:

```lisp
;; Set up a route that serves up our app's homepage
(defroute (:get "/") (req res)
  (send-response res :body "Welcome to my app!"))

;; Grab an album by its numeric ID
(defroute (:get "/albums/([0-9]+)") (req res args)
  ;; our album id is the first value in `args`
  (let ((album (my-app:get-album-by-id (car args))))
    (if album
        (progn
          ;; set the Content-Type for the response
          (setf (getf (response-headers res) :content-type) "application/vnd.myapp.album+json")
          ;; send back JSON for the album we found
          (send-response res :body (yason:encode album)))
        ;; NOPE
        (send-response res :status 404 :body "That album wasn't found =["))))

;; set up a route that can handle chunking
(defroute (:post "/files" :chunk t) (req res)
  (with-chunking req (chunk-bytes finishedp)
    (my-app:send-content-chunk-to-storage chunk-bytes)
    (when finishedp
      (my-app:finish-file)
      (send-response res :body "Thanks for the file."))))
```

<a id="with-vhost"></a>
### with-vhost (macro)
```lisp
(defmacro with-vhost (host &body body))
```

All routes defined via [defroute](#defroute) in the `body` of this macro will
use the virtual host specified in `host` (unless you explicitely set a host via
defroute's `:vhost` keyword).

```lisp
;; set up routes for different hostnames
(with-vhost "filibuster.com"
  (defroute (:get "/") (req res)
    (send-response res :body "welcome to the filibuster homepage where the text never ends and the fun never stops...in fact, i'd like to tell you a story abou..."))
  (defroute (:get ".+") (req res)
    (send-response res :body "filibuster (page not found)" :status 404)))

(with-vhost "sarcastic-ass.com"
  (defroute (:get "/") (req res)
    (send-response res :body "wow, SUPER glad you're on my site..."))
  (defroute (:get ".+") (req res)
    (send-response res :body "you hit the error page. you must be REALLY smart.")))
```

<a id="clear-route"></a>
### clear-route (function)
```lisp
(defun clear-route (method resource-str))
  => nil
```

Removes all routes that match the given method/resource from the routing table.

```lisp
;; add a route
(defroute (:get "/friends") (req res) ...)

;; clear out the route we just added
(clear-route :get "/friends")
```

<a id="clear-routes"></a>
### clear-routes (function)
```lisp
(defun clear-routes ())
  => nil
```

Clear out all routes in the routing table.

<a id="next-route"></a>
### next-route (function)
```lisp
(defun next-route ())
  => nil
```

This function allows a route to notify the routing system that it's not the
right route (even though it matched on method/resource). There are a few
reasons this could be useful (for instance if you want to route based on a
specific `Accept` header).

```lisp
;; define a route that returns a file
(defroute (:get "/thefile") (req res)
  (if (probe-file "thefile")
      ;; found the file...serve it!
      (send-response :body (get-file-contents "thefile"))
      ;; file not found, let another route try
      (next-route)))

;; define a catch-all route
(defroute (:get ".+") (req res)
  (send-response res :status 404 :body "What you're looking for isn't here."))
```

<a id="route-error"></a>
### route-error (condition)
A condition that describes a general error with the routing system.

<a id="route-not-found"></a>
### route-not-found (condition)
_extends [route-error](#route-error)_

This is a condition that's thrown when a route for the current method/resource
is not found.

