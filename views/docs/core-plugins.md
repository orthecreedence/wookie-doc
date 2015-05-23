---
title: Core plugins | Documentation
layout: documentation
---

Core plugins
============
{{toc}}

Wookie's core plugins provide simple support for many things an HTTP/app server
might support: GET/POST variable processing, multipart form processing, cookie
support, and file/directory serving.

Wookie's core plugins export all public functions to the `wookie-plugin-export`
package.

### get (plugin)
The `get` plugin parses and provides an interface to variables stored in the GET
querystring.

##### get-var (function)
```lisp
(defun get-var (request key))
  => string
```
Takes a the [request object](/docs/request-handling#request) and the variable
key and returns the string value for that query string parameter:

```lisp
;; request was "/search?name=fred"
(defroute (:get "/search") (req res)
  (get-var req "name")  =>  "fred"
  ...)
```

### post (plugin)
The `post` plugin parses and provides an interface to variable passing in an
HTTP POST body.

##### post-var (function)
```lisp
(defun post-var (request key))
  => string
```
This function is almost exactly like [get-var](#get-var) except it searches POST
data instead of GET data.

### multipart (plugin)
This plugin parses multipart form data (including files) and allows access to
the parsed data. Any files uploaded are saved to a temporary file which the
app will have access to (and which is automatically removed after the response
is sent).

##### form-var (function)
```lisp
(defun form-var (request field-name))
  => byte array
```
This function takes a [request object](/docs/request-handling#request) and looks
through the parsed form data, returning the binary data associated with the
given field name.

##### file-upload (function)
```lisp
(defun file-upload (request field-name))
  => plist
```
This function takes the current [request object](/docs/request-handling#request)
and the form field name of an uploaded file and returns a plist containing the
name of the file, the location of the temporary file on the local system, and
the file's mime type as reported by the uploader:

```lisp
(:filename "IMG2024.jpg"
 :tmp-file "~/.asdf/systems/wookie/upload-tmp/57"
 :mime-type "image/jpeg")
```

Using this information, you can easily do what you wish with the file: copy it
to a permanent location, parse it and respond with the processed data, etc.

### cookie (plugin)
This plugin supports the reading/writing of cookie data.

##### cookie-var (function)
```lisp
(defplugfun cookie-var (request key))
  => string
```
Given a [request object](/docs/request-handling#request) and a cookie name/key,
pull out the string cookie data.

##### set-cookie (function)
```lisp
(defun set-cookie (response key val &key expires max-age path domain http-only secure))
  => nil
```
This function takes a [response object](/docs/request-handling#response), a key,
and a value, and sets the cookie headers for the given data accordingly. Note
that by using the `:expires`, `:make-age`, `:path`, `:domain`, `:http-only`, and
`:secure` keywords, you can manipulate how the cookie functions on the client.
Feel free to [read more about how cookies work](http://en.wikipedia.org/wiki/HTTP_cookie).

Let's set a login cookie that expires in one day:
```lisp
(defroute (:post "/login") (req res)
  (let ((unique-hash (process-login)))
    (if unique-hash
        ;; success, save the login hash
        (progn
          (set-cookie res "user" unique-hash :max-age 86400)
          (send-response res :body "login success!"))
        ;; error!
        (send-response res :status 401 :body "login failed."))))
```

### directory-router (plugin)
The directory router allows your app to map a local folder to a url, making it
easy to set up simple file/directory serving.

##### def-directory-route (function)
```lisp
(defun def-directory-route (route-path local-path &key disable-directory-listing))
  => nil
```
This function takes the path of a URL route and the local path to a directory
and automatically serves files and directory listings from that local path. For
instance, let's say you have a folder in your project called `images/` and you
want that folder to be accessible through the url `/assets/images`. You could
do:

```lisp
(def-directory-route "/assets/images" "./images")
```

Now going to `/assets/images/1234.jpg` in a browser will attempt to load and
serve the local file `./images/1234.jpg`.

Note that if a file/directory is matched to the directory router but doesn't
exist, the directory router will call [next-route](/docs/routes#next-route),
letting your application deal with the non-existent file/directory how it sees
fit.

Directory listings can be disabled by passing `:disable-directory-listing t`.
