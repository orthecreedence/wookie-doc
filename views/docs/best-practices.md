---
title: Best practices
layout: documentation
---

Wookie best practices
=====================
Here are some tips on making your Wookie experience as smooth as possible. Keep
in mind that this page may update frequently as issues come up and as Wookie is
used more in production.

{{toc}}

### When to use Wookie
Wookie is, by nature, asynchronous. This means that it really shines when your
app is doing a lot of network IO: grabbing results from APIs, calling databases,
and any other network-based communication.

If you're building an app that does a lot more *communication* than *processing*
and the drivers you need [are available](http://orthecreedence.github.io/cl-async/drivers),
Wookie is a good choice. If your app does a lot of CPU work, number crunching,
file IO, then it's probably best to stick to a threaded server like the
excellent [Hunchentoot](http://weitz.de/hunchentoot/).

Keep in mind that if your app would benefit by using Wookie and a driver for a
service you're using in your infrastructure isn't included, you can feel free to
[open an issue in the cl-async Github](https://github.com/orthecreedence/cl-async/issues)
for that driver and I'll try to build it (or convert an existing one) as fast as
I can...feel free to take a stab at it yourself as well =].

### Error handling
There is a [section covering error handling](/docs/error-handling) in the docs,
but I'm going to take this chance to talk about error handling *in general* when
using cl-async et al.

There are two basic setups 

Error handling in asynchronous systems is very important. A rogue condition
can completely halt the execution of your application and leave your clients
hanging indefinitely.

Familiarize yourself with [cl-async's error handling](http://orthecreedence.github.io/cl-async/event-handling)
and also [Wookie's error handling](/docs/error-handling) before taking your app
to production.

Here are some tips on checking your error handling:

- Try sending bogus data to your app
- Try accessing pages that don't exist
- Intentionally add in conditions that will cause an error. See how the app
responds.

If everything is set up properly, you should get no crashes in your app, even
in the event that errors escape. See [an example/simple error handler for a 
Wookie app](https://github.com/orthecreedence/wookie-doc/blob/master/init.lisp).

*NOTE:* A top-level error handler is no excuse to not catch errors at the
source. It is used as a last resort to ensure the well-being of the application
as a whole. If errors escape to the top-level, try to figure out what happened
and fix the situation.

### Application auth
As mentioned elsewhere, it makes sense to check your application authentication
in the [:pre-route hook](/docs/hooks#pre-route) by returning a
[future](/docs/hooks#hooks-and-futures) from the hook that finishes if auth was
successful (and has an error triggered on it if unsuccessful). This is useful
because you can check authentication via a database and immediately cancel the
request if the auth info is incorrect, saving your app from processing a request
it's just going to throw out.

However, this has some side effects when dealing with chunked data. Please
familiarize yourself with [defroute's options](/docs/routes#defroute)
(specifically `:suppress-100` and `:buffer-body`) and be sure to read the guide
on [successfully handling chunked data when using futures with :pre-route](/docs/hooks#pre-route).

### Large file uploads via XHR
It's becoming increasingly more popular and relevant to have your entire app
(logic, templating, etc) live entirely in javascript land. Because of this,
you'll find yourself dealing with XHR a lot.

Although XHR has gotten some nice improvements since the HTML5 frenzy started,
it still doesn't support chunking while uploading data. This means that while
you may *want* to stream upload data from one location to another through
Wookie, browsers will just send the entire thing as one big chunk with a
`Content-Length` header specified.

However, you can specify that the upload route use chunking:

```lisp
(defroute (:post "/uploads" :chunk t) (req res)
  (with-chunking (chunk lastp)
    ...))
```

Since Wookie switched from http-parse to [fast-http](https://github.com/fukamachi/fast-http),
[with-chunking](/docs/request-handling#with-chunking) will now receive the HTTP
data packet-by-packet (instead of chunk-by-chunk). This lets you stream incoming
data, chunked or not, as it arrives. `with-chunking` also tells Wookie to *not*
[store the incoming HTTP body](/docs/request-handling#request-store-body) in the
request object (so you don't waste 100mb of memory when processing an upload).

### Reverse Proxy
Wookie can be run by itself, but because it's new and hasn't been battle tested,
it's important to be weary of running it on its own in the wild. There may be 
uncaught security issues or bugs lurking around waiting to crash your app.
My suggestion is to use some sort of reverse proxy. This can provide a layer of
protection (less chance of malformed HTTP requests crashing your server).

[NginX](http://nginx.org/) is a great reverse proxy + web server. On top of
being really fast, it can serve your static content as well. Wookie's
[directory router](/docs/core-plugins#directory-router) allows an app to serve
static content and is fine for smaller websites or if you are developing an app
locally and don't want to set up some sort of reverse proxy. However I can
*guarantee* that NginX will always beat the directory router performance-wise,
and fully recommend using it in favor of the directory router in production.

#### Sample NginX config
This is a sample of the config used to run the Wookie doc site through NginX.

```
upstream wookiedoc {
    server 127.0.0.1:9061;
}

server {
    # set up the host/port
    listen 80;
    server_name wookie.beeets.com;

    # note we set the location to the static files here
    set $server_root /srv/www/wookie-doc/webroot;
    root $server_root;

    charset utf-8;

    access_log /var/log/www/wookie.beeets.com-access.log main;
    error_log /var/log/www/wookie.beeets.com-error.log;

    location / {
        rewrite ^(.*\.)v[0-9\.]+\.(css|js|gif|png|jpg)$ $1$2 last;
        try_files $uri @proxysite;

        add_header Cache-Control "public, max-age=315360000";
    }

    location @proxysite {
        proxy_pass http://wookiedoc;
        proxy_next_upstream error timeout invalid_header http_500 http_502 http_503 h
        proxy_redirect off;
        proxy_buffering off;
        proxy_set_header Host $host;
        proxy_set_header X-Forwarded-For $remote_addr;
    }

}
```
