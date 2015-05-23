---
title: Helpers | Documentation
layout: documentation
---

Helpers
=======
{{toc}}

Wookie comes bundled with some helper functions to make all our lives easier.

### start-static-server
Starts a static file server in the current directory. This is great if you need
to access local resources via a web browser but don't want to have to fire up
Apache or similar and set up a bunch of vhosts. Example:

```lisp
(wookie-helper:start-static-server :port 8090 :asset-dir "./assets")
```

The above will serve all files under `./assets` at the top-level path. In other
words, `http://127.0.0.1:8090/test.html` will be loaded from `./assets/test.html`.

Definition:
```lisp
(defun start-static-server (&key (asset-dir "./") bind (port 8080)))
  => nil
```



