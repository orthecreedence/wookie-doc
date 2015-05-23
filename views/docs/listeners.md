---
title: Listeners | Documentation
layout: documentation
---

Listeners
=========
{{toc}}

A listener is an object that describes to Wookie where/how it should listen for
incoming requests.

### listener (class)
The `listener` class is the basic way of telling Wookie where you want to accept
connections from. It tells Wookie what address/port to bind to and also the
connection backlog value.

The backlog is a number that tells the operating system how many connections
should be queued if they are unable to be accepted. The default (-1) tells the
operating system to just use the value it thinks is best.

##### listener-bind (accessor)
Initarg `:bind`

Accessor for the listener's `bind` value (what address to accept connections on).

##### listener-port (accessor)
Initarg `:port`

Accessor for the listener's `port` value (what port to accept connections on).

##### listener-backlog (accessor)
Initarg `:backlog`

Accessor for the listener's `backlog` value.

##### listener-event-cb (accessor)
Initarg `:event-cb`

Accessor for the listener's `event-cb` value.

### ssl-listener (class)
_extends [listener](#listener)_

The `ssl-listener` class extends the [listener](#listener) class and tells
Wookie to listen over SSL using the given certificate/key (and optional key
password). It uses [cl-async's SSL](http://orthecreedence.github.com/cl-async/tcp-ssl). Loading of SSL can be
completely disabled by pushing `:wookie-no-ssl` onto `*features*` before loading
Wookie.

##### listener-certificate (accessor)
Accessor for the SSL listener's certificate file.

##### listener-key (accessor)
Accessor for the SSL listener's key file.

##### listener-password (accessor)
Accessor for the SSL listener's key password (optional, only needed if the key
file is password-encrypted).

### start-server (method)
```lisp
(defgeneric start-server (listener))
  => tcp-server
```

This method takes a listener and uses it to create a [cl-async tcp-server](http://orthecreedence.github.com/cl-async/tcp#tcp-server)
object which is then returned.

