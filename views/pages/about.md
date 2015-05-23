---
title: About Wookie
layout: default
---

About
=====
Wookie, named after [my dog Wookie](#wookie-dog), started out as an async port
of the excellent [Hunchentoot server](http://weitz.de/hunchentoot/). As I got
deeper in, I realized that Hunchentoot was, at its core, built to be threaded.
If converted, it would forever live out its days as the strange genetic freak
baby of synchronous and asynchronous.

Once I realized this, I started experimenting with doing HTTP in async. The most
difficult part is correctly parsing the protocol. I built [a separate library
for parsing HTTP](https://github.com/orthecreedence/http-parse) that can be used
both synchronously (say, with usocket) or asynchronously with something like
cl-async. After finishing http-parse, I started some very basic work to plug it
into cl-async.

Wookie was born!

Since then, http-parse has been replaced by the blazing fast [fast-http](https://github.com/fukamachi/fast-http),
and Wookie has matured quite a bit. It has managed to keep its core small and
support any needed functionality via its plugin system. This makes Wookie fast,
lean, and simple.

Wookie is now being used in production in many places, including the [Turtl project](https://turtl.it),
where it serves both the API server and the turtl.it website.

<a name="wookie-dog"></a>
The dog (Wookie)
----------------
<ul class="gallery clear">
    <li>
        <a href="/images/wookie/map.jpg" rel="modal">
            <img src="/images/wookie/map_sq.jpg" alt="wookie map" title="Wookie planning a trip"/>
        </a>
    </li>
    <li>
        <a href="/images/wookie/laser.jpg" rel="modal">
            <img src="/images/wookie/laser_sq.jpg" alt="laser dog" title="Wookie before a death beam rampage"/>
        </a>
    </li>
    <li>
        <a href="/images/wookie/couch1.jpg" rel="modal">
            <img src="/images/wookie/couch1_sq.jpg" alt="dog on couch" title="Wookie lounging"/>
        </a>
    </li>
    <li>
        <a href="/images/wookie/den.jpg" rel="modal">
            <img src="/images/wookie/den_sq.jpg" alt="dog denning" title="Wookie denning."/>
        </a>
    </li>
    <!--
    <li>
        <a href="/images/wookie/gaze.jpg" rel="modal">
            <img src="/images/wookie/gaze_sq.jpg" alt="dog stare" title="Wookie looking out over the horizon"/>
        </a>
    </li>
    -->
    <li>
        <a href="/images/wookie/look.jpg" rel="modal">
            <img src="/images/wookie/look_sq.jpg" alt="wookie look" title="Wookie doing an interview"/>
        </a>
    </li>
    <!--
    <li>
        <a href="/images/wookie/pose.jpg" rel="modal">
            <img src="/images/wookie/pose_sq.jpg" alt="dog posing" title="Wookie striking a pose"/>
        </a>
    </li>
    -->
    <li>
        <a href="/images/wookie/smile.jpg" rel="modal">
            <img src="/images/wookie/smile_sq.jpg" alt="dog smiling" title="Wookie is pleased. We shall prosper."/>
        </a>
    </li>
    <li>
        <a href="/images/wookie/tantrum.jpg" rel="modal">
            <img src="/images/wookie/tantrum_sq.jpg" alt="dog throwing a tantrum" title="Wookie is angered. We shall suffer."/>
        </a>
    </li>
</ul>

The webserver is named after my dog, Wookie. He's easily angered, image-obsessed,
and make sounds like a wookie when squeezed. This project is dedicated to him.


