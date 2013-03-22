---
title: Request handling | Documentation
layout: documentation
---

Request handling
================
Starting a server and setting up routes is kewl, but unless you know what to do
once an actual request comes in, it's all just an excersize in academia.

Wookie supports simple request/response handling, but also has the ability to
stream incoming/outgoing chunked HTTP content.

{{toc}}

<!--
- [request](#request) _class_
  - [request-socket](#request-socket) _accessor_
  - [request-method](#request-method) _accessor_
  - [request-resource](#request-resource) _accessor_
  - [request-headers](#request-headers) _accessor_
  - [request-uri](#request-uri) _accessor_
  - [request-http](#request-http) _accessor_
- [response](#response) _class_
  - [response-headers](#response-headers) _accessor_
  - [response-finished-p](#response-finished-p) _accessor_
- [send-response](#send-response) _function_
- [start-response](#start-response) _function_
- [finish-response](#finish-response) _function_
- [with-chunking](#with-chunking) _macro_
- [response-error](#response-error) _condition_
  - [response-error-response](#response-error-response) _accessor_
- [response-already-sent](#response-already-sent) _condition_
-->

### request (class)
The request class holds information about an incoming request: the socket the
request happened on, the HTTP method/resource/headers, etc.

It is passed to all routes defined by [defroute](/docs/routes#defroute).

##### request-socket (accessor)

##### request-method (accessor)

##### request-resource (accessor)

##### request-headers (accessor)

##### request-uri (accessor)

##### request-http (accessor)

### response (class)

##### response-headers (accessor)

##### response-finished-p (accessor)

### send-response (function)

### start-response (function)

### finish-response (function)

### with-chunking (macro)

### response-error (condition)

##### response-error-response (accessor)

### response-already-sent (condition)

