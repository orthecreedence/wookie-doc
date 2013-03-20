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

<a id="request"></a>
### request (class)

<a id="request-socket"></a>
##### request-socket (accessor)

<a id="request-method"></a>
##### request-method (accessor)

<a id="request-resource"></a>
##### request-resource (accessor)

<a id="request-headers"></a>
##### request-headers (accessor)

<a id="request-uri"></a>
##### request-uri (accessor)

<a id="request-http"></a>
##### request-http (accessor)

<a id="response"></a>
### response (class)

<a id="response-headers"></a>
##### response-headers (accessor)

<a id="response-finished-p"></a>
##### response-finished-p (accessor)

<a id="send-response"></a>
### send-response (function)

<a id="start-response"></a>
### start-response (function)

<a id="finish-response"></a>
### finish-response (function)

<a id="with-chunking"></a>
### with-chunking (macro)

<a id="response-error"></a>
### response-error (condition)

<a id="response-error-response"></a>
##### response-error-response (accessor)

<a id="response-already-sent"></a>
### response-already-sent (condition)

