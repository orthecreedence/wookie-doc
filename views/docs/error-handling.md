---
title: Error handling | Documentation
layout: documentation
---

<a style="float: right;" href="/docs">&laquo; Back to docs</a>
Error handling
==============
Error handling is where asynchronous systems have much trouble. Wookie is no
different. While it's possible to set a global error handler for your
application, per-route error handling is difficult because for each async
callback triggered, a new stack is created and any error handling will be lost
with the old stack. Any uncaught conditions thrown will be caught by the global
app error handler.

__The best way to handle errors in Wookie is to catch any possible conditions in
your code without letting them escape to top level.__ The app error handler is
capable of handling any errors that com its way, but you'll have very little
context as to where the error came from and how to fix it.

This can be mitigated somewhat by using [cl-async's futures](http://orthecreedence.github.com/cl-async/),
which provide [error handling abstraction](http://orthecreedence.github.com/cl-async/future#error-handling)
that make async programming a bit nicer, however it's not a perfect solution.

{{toc}}

### add-error-handler (function)
```lisp
(defun add-error-handler (error-type handler &key (error-table *wookie-error-handlers*)))
  => nil
```
Add an error handler (function) for a specific error type. Whenever the given
error type is encountered, Wookie fires the handler function.
