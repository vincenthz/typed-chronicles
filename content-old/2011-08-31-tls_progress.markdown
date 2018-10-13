---
author: Vincent Hanquez
title: TLS progress report
tags: tls,ssl,progress
date: July 31, 2011
---

In an effort started just before CamHac and carried on during CamHac,
there's some new features supported by the haskell tls framework.

<!--more-->

Record cleanup
--------------

The record layer of the TLS protocol is now cleaner, and is using a
type safe approch with record data being tagged with their status in
the record process (plaintext, compressed or ciphertext).

Also it simplify the layer of functions doing the transformation.

Compression support
-------------------

Building on the record cleanup, i've added compression support. So
now you can compress your record data. Beware, if you're using
a protocol (HTTP) that may compressed data as well; data compressed
twice by the same algorithm is never good size wise.

Protocol version 1.2
--------------------

Now you can use the latest version of the TLS protocol, along with
previously supported versions SSL3, TLS1.0 and TLS1.1.

What's next
-----------

there's still some feature not supported, and hopefully will be part of
a later version:

* session support.
* client certificate.
* DH key algorithm.
* ECC support.
* AEAD bulk algorithm.

any contributions welcome :)

Release Status
--------------

The code is not yet available on hackage, but you can grab at the usual
place on github, and will be part of the incoming 0.8 release.

* on [github](http://github.com/vincenthz/hs-tls/)
