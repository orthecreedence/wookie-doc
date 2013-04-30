---
title: Configuration | Documentation
layout: documentation
---

Configuration
=============
Wookie provides configuration options for much of its functionality.

{{toc}}

### \*log-level\*
This var determines what level of logging Wookie will use, and can be one of

- `:emerg`
- `:error`
- `:warning`
- `:notice`
- `:info`
- `:debug`

These follow \*nix syslog convention. Wookie normally logs to `\*standard-output\*`
but this can be changed by setting [\*log-output\*](#log-output).

### \*log-output\*
Holds a stream that Wookie will write to. This could be a TCP stream to a
logging server, a file stream, etc. If `nil` or `t`, will just log straight to
standard output (the default).

### \*error-handler\*
This is covered in the [error handling](/docs/error-handling) section, but worth
mentioning here as well.

This var is either `nil` (Wookie's default/dumb handler) or a function that that
takes one argument (an error/condition object), which is used to handle specific
errors your application may run into.

If you set this variable, it's a good idea to set the same function into
cl-async's `:default-event-cb` keyword while starting your event loop.

### \*hide-version\*
If `t` will abstain from sending Wookie's version number in the `Server` header
on a response, otherwise if `nil` will pull and send current version from the
ASDF system.

### \*enabled-plugins\*
This is a list of plugins that Wookie is allowed to load. Wookie may parse and
or be aware of *all* plugins in your plugin directories, but will only load ones
directly that are in this list. Note that if it's not in this list, but it *is*
a dependency of a plugin that is, it will be loaded automatically by ASDF.

### \*tmp-file-store\*
This is a stupid variable that shouldn't be in Wookie's config, because it is
really only used by the [multipart plugin](/docs/core-plugins#multipart) for
temporarily storing file data passed via forms.

Until it is deprecated, use it to specify where your temporary uploaded files
are stored (pathname).

