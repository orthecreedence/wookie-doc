---
title: Plugins | Documentation
layout: documentation
---

Plugins
=======
{{toc}}

Wookie's goal is to provide a fast HTTP server while keeping the core as minimal
as possible. Wookie does this by only providing the bare essentials for a web
server, and letting the rest be built using plugins.

Wookie's core plugins (found in the `/core-plugins/` folder) provide a lot of
functionality a normal HTTP/app server would. They can be disabled or overridden
by your own plugins if you desire different behavior.

The standard behavior for a plugin is to export its publicly available functions
to the `wookie-plugin-export` package. This makes it very easy for an
application to `:use` one package and have access to all of the exported
functions for the loaded plugins, without having to explicitely know which
package a plugin function is defined.

### \*enabled-plugins\*
This global variable holds a list of enabled plugins. The default:

```lisp
'(:get :post :multipart :cookie :directory-router)
```

Loads the [core Wookie plugins](/docs/core-plugins) when [load-plugins](#load-plugins)
is called.

### \*plugin-folders\*
This is a list of folders to search for plugins in. The first folders in the
list take precedence over later entries, meaning you could `push` your own
plugin directory onto this variable, and the plugins in your directory would
take precedence over any plugins in the core directory (any plugins with the
same name would load from your directory, not hte core directory).

The default `*plugin-folders*` value is a list with one entry, the `/core-plugins/`
directory.

### load-plugins (function)
```lisp
(defun load-plugins (&key ignore-loading-errors (use-quicklisp t)))
  => nil
```
This function loads looks through all folders in [\*plugin-folders\*](#plugin-folders)
and attempts to load any plugins it finds in [\*enabled-plugins\*](#enabled-plugins).
It does this by matching the directory name of a plugin with the keyword name of
that plugin in `*enabled-plugins*`.

So let's say `:session` was added to the `*enabled-plugins*` list. `load-plugins`
would look for a directory called `session/` in each of the folders specified in
`*plugin-folders*`. If it finds a folder called `session/`, it will attempt to
load `session/plugin.asd` which will be used to load that plugin (along with
any of its dependencies).

Plugins are loaded using ASDF, meaning they can use their own `:depends-on`
clause which will be honored.

This function has the ability to ignore any loading errors occuring loading (for
instance if a dependency can't be found, that plugin silently fails to load)
via `:ignore-loading-errors`.

`load-plugins` can also be told to use quicklisp for plugin loading (on by
default) which means dependencies can be downloaded automatically if needed.

`load-plugins` works by loading the ASDF files for each plugin, creating a new
system that depends on all the enabled plugins, and loads that system via ASDF.

### unload-plugin (function)
```lisp
(defun unload-plugin (plugin-name))
  => nil
```
Recursively unloads a plugin and all its dependencies. Note that this won't
actually remove the code from the lisp image, but will mark the plugin as
unloaded in Wookie's internal plugin registry. Also, unloaded plugins will
generally undo any hooks they have into the Wookie system, and will cease to
process incoming requests.

