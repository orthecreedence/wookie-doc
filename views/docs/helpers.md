---
title: Helpers | Documentation
layout: documentation
---

Helpers
=======
{{toc}}

Wookie comes bundled with some helper functions to make all our lives easier.

### start-static-server (function)
```lisp
(defun start-static-server (&key (asset-dir "./") bind (port 8080)))
```

Starts a static file server in the current directory. This is great if you need
to access local resources via a web browser but don't want to have to fire up
Apache or similar and set up a bunch of vhosts. Example:

```lisp
(wookie-helper:start-static-server :port 8090 :asset-dir "./assets")
```

The above will serve all files under `./assets` at the top-level path. In other
words, `http://127.0.0.1:8090/test.html` will be loaded from `./assets/test.html`.

### serve-html5-app (function)
```lisp
(defun serve-html5-app (&key bind port (index-file "index.html"))
```

Like its cousin [start-static-server](/docs/helpers#start-static-server), starts
a server on the given `:bind`ing and `:port`, but routes any requests that don't
match a file to the file given by `:index-file`.

This assumes the index.html file loads javascript that is able to look at the
current URL path and route itself accordingly.

Note that this function *must be run within the directory of the app being
served*.

Example:

```lisp
(serve-html5-app :port 6969)
```

Listens on port 6969. Any incoming requests that do not match a local file are
given `index.html` as a response (which presumably loads/routes the front-end
app).

