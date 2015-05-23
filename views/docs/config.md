---
title: Configuration | Documentation
layout: documentation
---

Configuration
=============
{{toc}}

Wookie provides configuration options for some of its functionality.

### \*debug-on-error\*
See [error handling documentation](/docs/error-handling#debug-on-error). This
allows errors Wookie encounters (in your routes or hooks) to pass unfettered to
the debugger for further inspection. Great for debugging or when your app is
bdeing actively developed.

Default `nil` (the safe option for production use)

### \*max-body-size\*
When we are [storing the HTTP body in the request object](/docs/request-handling#request-store-body),
this determines the maximum data (in bytes) we can buffer before sending the
client an `HTTP 413` error and ending the request.

Default `1024 * 1024 * 2` (2mb)

### \*hide-version\*
If `t` will abstain from sending Wookie's version number in the `Server` header
on a response, otherwise if `nil` will pull and send current version from the
ASDF system.

Default `nil`

### \*enabled-plugins\*
This is a list of plugins that Wookie is allowed to load. Wookie may parse and
or be aware of *all* plugins in your plugin directories, but will only load ones
directly that are in this list. Note that if it's not in this list, but it *is*
a dependency of a plugin that is, it will be loaded automatically by ASDF.

Default `'(:get :post :multipart :http-var :cookie :directory-router)`

### \*tmp-file-store\*
This is a stupid variable that shouldn't be in Wookie's config, because it is
really only used by the [multipart plugin](/docs/core-plugins#multipart) for
temporarily storing file data passed via forms.

Until it is deprecated, use it to specify where your temporary uploaded files
are stored (pathname).

