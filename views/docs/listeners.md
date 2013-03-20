---
title: Listeners | Documentation
layout: documentation
---

Listeners
=========
A listener is an object that describes to Wookie where/how it should listen for
incoming requests.

- [listener](#listener) _class_
  - [listener-bind](#listener-bind) _accessor_
  - [listener-port](#listener-port) _accessor_
  - [listener-backlog](#listener-backlog) _accessor_
- [ssl-listener](#ssl-listener) _class_
  - [listener-certificate](#listener-certificate) _accessor_
  - [listener-key](#listener-key) _accessor_
  - [listener-password](#listener-password) _accessor_
- [start-server](#start-server) _method_

<a id="listener"></a>
### listener (class)
The `listener` class is the basic way of telling Wookie where you want to accept
connections from. It tells Wookie what address/port to bind to and also the
connection backlog value.

The backlog is a number that tells the operating system how many connections
should be queued if they are unable to be accepted. The default (-1) tells the
operating system to just use the value it thinks is best.

<a id="listener-bind"></a>
##### listener-bind (accessor)
Accessor for the listener's `bind` value (what address to accept connections on).

<a id="listener-port"></a>
##### listener-port (accessor)
Accessor for the listener's `port` value (what port to accept connections on).

<a id="listener-backlog"></a>
##### listener-backlog
Accessor for the listener's `backlog` value.

<a id="ssl-listener"></a>
### ssl-listener (class)
_extends [listener](#listener)_

The `ssl-listener` class extends the [listener](#listener) class and tells
Wookie to listen over SSL using the given certificate/key (and optional key
password). It uses [cl-async's SSL](/cl-async/tcp-ssl). Loading of SSL can be
completely disabled by pushing `:wookie-no-ssl` onto `*features*` before loading
Wookie.

<a id="listener-certificate"></a>
##### listener-certificate (accessor)
Accessor for the SSL listener's certificate file.

<a id="listener-key"></a>
##### listener-key (accessor)
Accessor for the SSL listener's key file.

<a id="listener-password"></a>
##### listener-password (accessor)
Accessor for the SSL listener's key password (optional, only needed if the key
file is password-encrypted).

<a id="start-server"></a>
### start-server (method)
```lisp
(defgeneric start-server (listener))
  => tcp-server
```

This method takes a listener and uses it to create a [cl-async tcp-server](/cl-async/tcp#tcp-server)
object which is then returned.
