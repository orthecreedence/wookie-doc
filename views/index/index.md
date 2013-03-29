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

The dog
-------
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

