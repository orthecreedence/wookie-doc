---
title: F.A.Q.
layout: default
---

Frequently Asked Questions
==========================

### I want to explicitly import a symbol exported in the package `wookie-plugin-export` but the package is empty until I run `(wookie:load-plugins)`?

You can use ASDF to call `load-plugins` before your system is loaded. To do so,
one needs to define a :before `perform` method that specializes on your system
and the prepare operation. One caveat is that because the asd file is read
before package is loaded one can't call `(wookie:load-plugins)` as th epackage
wookie doesn't exist, to work around this ASDF comes with `symbol-call`.

```lisp
;; An example: System foo
(asdf:defsystem #:foo
    ...)

(defmethod perform :before ((op asdf:prepare-op)
                            (system (eql (find-system :foo)))
    (asdf/package:symbol-call :wookie 'load-plugins)))
```

### I get errors about SSL while loading Wookie

This can be caused if you don't have OpenSSL installed on your machine or it is
installed in a non-standard location.

You can disable loading OpenSSL by doing the following *before* you Quickload or
ASDF load Wookie:

```lisp
(push :wookie-no-ssl *features*)
```
