---
title: F.A.Q.
layout: default
---

Frequently Asked Questions
==========================

- I want to explicitly import a symbol exported in package `wookie-plugin-export` but the package is empty until I run `(wookie:load-plugins)`?

  You can use ASDF to call `load-plugins` before your system is loaded. To do so, one needs to define a :before `perform` method that specializes in your system and the prepare operation. One caveat is that because the asd file is read before package is loaded one can't call `(wookie:load-plugins)` as the package wookie doesn't exist, luckily ASDF come with `symcol-call` for this situations.

```lisp
;; An example: System foo
(asdf:defsystem #:foo
    ...)

(defmethod perform :before ((op asdf:prepare-op)
                            (system (eql (find-system :foo)))
    (asdf/package:symbol-call :wookie 'load-plugins)))
```
