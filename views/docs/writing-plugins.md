---
title: Writing plugins | Documentation
layout: documentation
---

Writing plugins
===============
{{toc}}

Knowing how to write plugins can make Wookie a lot more useful to you, and if
you share your plugins, possibly others!

The general concept behind plugins is that you define a `plugin.asd` file which
loads all your plugin's files and dependencies. The plugin code itself ties into
Wookie using hooks (or builds on top of existing Wookie functionality) and
exports your public functions to the `woookie-plugin-export` package. There are
a number of tools Wookie provides to make this process easy.

### defplugin (macro)
```lisp
(defmacro defplugin (&rest asdf-defsystem-args))
  => nil
```
This macro wraps creation of `asdf:defsystem` and makes sure to match the ASDF
system name passed to `defsystem` with the plugin name of the plugin being
defined. For instance, if you're loading a plugin named `:get`, you can have an
ASDF system in your `plugin.asd` called `wookie-get-plugin` and `defplugin` will
match the two together. This way you can have simple names when loading plugins,
but not have to use simple names when creating your ASDF system.

This macro is *required* if you're writing plugins. It also implicitely selects
the short plugin name based on the directory it's loading from, there's no way
to specify the short name.

### register-plugin (function)
```lisp
(defun register-plugin (plugin-name init-function unload-function))
  => nil
```
Register a plugin into Wookie. This *must* be called for all plugins that wish
to be loaded by Wookie, even if the plugin doesn't need to use the init/unload
functions.

It's standard to place this in a top-level for in one of your plugin files
loaded by the `plugin.asd`.

For example, if I was loading a plugin called "get":

```lisp
(register-plugin :get 'init-get-vars 'unload-get-vars)
```

### plugin-config (function)
```lisp
(defun plugin-config (plugin-name))
  => config-data
```
This is a setfable function that allows a standard interface for accessing a
plugin's configuration data. Data can be stored arbitrarily (plist, hash table,
etc).

### plugin-request-data (function)
```lisp
(defun plugin-request-data (plugin-name request))
  => plugin-request-data
```
This is a setfable function that provides per-request storage and retrieval of
plugin data. For instance, if you want to parse GET variables out of the query
string, you could do so in a [:parsed-headers](/docs/hooks#parsed-headers) hook
and store the data so that the app has later access to it.

Generally you wrap any access to `plugin-request-data` in public functions that
hide the interface you use for storing data. For instance, when we parse the
GET variables, we may store them in the plugin like so:

```lisp
(setf (plugin-request-data :get request) get-kv-pairs)
```

but also provide a public function to retrieve this data without having to use
`plugin-request-data`:

```lisp
(defplugfun get-var (request key)
  "Get a value from the GET data by key."
  (let ((hash-get-vars (plugin-request-data :get request)))
    (gethash key hash-get-vars)))
```

### defplugfun (macro)
```lisp
(defmacro defplugfun (name args &body body))
  => nil
```
Defines a function to be exported from the current plugin into the
`wookie-plugin-export` package automatically. It's fine to use `defun` and
export your functions/symbols manually, but this is a quick way of doing so and
also lets readers of the plugin know what functions are to be publically
available.

### Plugin example
Let's work through a simple GET example plugin. First, we need to create our
directory structure:

```
my-plugins/
  get/
    plugin.asd
    get.lisp
```

Remember to push the full path of `my-plugins/` to [\*plugin-folders\*](/docs/plugins#plugin-folders)
so it loads properly.

Next, let's make our `plugin.asd` file:

```lisp
(wookie:defplugin wookie-plugin-core-get
  :author "Andrew Danger Lyon &lt;my@email.com>"
  :license "MIT"
  :version "0.2.1"
  :description "A GET plugin for Wookie"
  :depends-on (#:wookie)
  :components
  ((:file "get")))
```

Fairly simple, looks just like a `asdf:defsystem` entry, and in fact it pretty
much is the same thing, however Wookie matches `wookie-plugin-core-get` with
the name "get" which it derives from the directory name. Remember, the ASDF
system name can be arbitrary (except when resolving ASDF dependencies).

Now let's take a look at `get.lisp`:

```lisp
(defpackage :wookie-plugin-core-get
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-get)

;; called on each request, parses the query string and stores the data into the
;; request's plugin data storage via plugin-request-data
(defun parse-get-vars (request)
  "Grab GET data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-get-vars (make-hash-table :test #'equal)))
    (map-querystring (puri:uri-query (request-uri request))
      (lambda (key val)
        (setf (gethash key hash-get-vars) val)))
    (setf (plugin-request-data :get request) hash-get-vars)))

;; define a publically exported function, that adds itself to the
;; wookie-plugin-export package. see here we're building an interface to get
;; data out of (plugin-request-data ...)
(defplugfun get-var (request key)
  "Get a value from the GET data by key."
  (let ((hash-get-vars (plugin-request-data :get request)))
    (gethash key hash-get-vars)))

;; when the plugin loads, hook into the :parsed-headers hook so that parse-get-vars
;; is called on each incoming request
(defun init-get-vars ()
  (add-hook :parsed-headers 'parse-get-vars :get-core-parse-vars))

;; if the plugin is unloaded, unhook the above hook
(defun unload-get-vars ()
  (remove-hook :parsed-headers :get-core-parse-vars))

;; register-plugin is top-level. this is required!
(register-plugin :get 'init-get-vars 'unload-get-vars)
```

Your plugins can tie into as many hooks in Wookie as needed and export as many
functions as needed. As mentioned, your ASDF system name and plugin name do not
need to match.
