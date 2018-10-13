---
author: Vincent Hanquez
title: Haskell SipHash - fast and small MAC
tags: crypto,SipHash
date: June 25, 2012
---

While reading my twitter stream, following the release of the paper, many
people added support for their favorite language to SipHash.  I decided that
haskell shouldn't be left behind, and support this cute algorithm too.

<!--more-->

What is SipHash
---------------

SipHash is a pseudorandom function (PRF) optimized for short inputs.
This is useful when creating MAC (HMAC, PMAC) like tag on short input.
This is important to understand the tradeoff made to support short input,
before using it, as it's not a generic MAC algorithm.

It also have interesting property for hash table hashing, making it much harder
for being hash poisoned for example.

One can read more at the [original author website](http://131002.net/siphash/)

Using SipHash in haskell
------------------------

It's very easy to use and the github README should be more than enough to get
you started with Haskell's SipHash [README](https://github.com/vincenthz/hs-siphash/blob/master/README.md)
