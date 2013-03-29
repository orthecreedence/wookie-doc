---
title: Wookie: An asynchronous web server for Common Lisp
layout: default
---

Wookie - An asynchronous HTTP server
====================================
Wookie is an asynchronous HTTP server written in Common Lisp. It is built on top
of [cl-async](http://orthecreedence.github.com/cl-async).

The principal behind Wookie is to have a very small core, with hooks in each
processing step that allow for expansion. For instance, Wookie doesn't handle
GET or POST variables natively. Instead, it has a set of [core plugins](/docs/core-plugins)
that provide this functionality.

*Wookie is very new and considered beta.*

