---
author: Vincent Hanquez
title: December news for the haskell TLS framework
tags: haskell,tls,session
date: December 20, 2011
---

In an effort of publicize more what's happening with TLS, here is a textual
changelog of the two previous versions:

<!--more-->

TLS 0.8.3
---------

The first thing is the code reorganization that lead to make the record layer
of TLS more simple and readable; It also separate better the logic between the
handshake code and the record layer code. The improvement about this is, it
separates two critical pieces of code, one about establishing the security
between two parties (handshake) and the other keeping the data safe and secure
(record layer).

It also de-intertwine the RSA code from the record layer, which improve the ability
to support new key exchange in the future (DH, DHE, ECDHE, ECDHE-RSA, etc).

The second improvement is a callback for handshake renegociations, so a server
could check that a client is not trying to abuse the renegocitiation facility,
that cost a lot of cpu for a server. This is a simple implementation that just
count the number of bytes seen between handshakes, and ask the user to deny or
accept according to those number of bytes.

It goes hands in hands with others measure that need to be put in front of a
server to check that clients are not trying to abuse the TLS facility.

TLS 0.8.4
---------

Building on top of the improvements and cleanup of 0.8.3, i've finally added
session support. This is very exciting for everyone using the TLS stack, as
it does massively improve the performance of the TLS handshake.

The whole point of the session resumption in TLS, is to avoid exchange of
security parameters (the so-called master secret), between client and server
which is very expensive for the server.

The typical way that is done, is that the client contact the server once, and
they negociate the master secret through cpu intensive crypto. when the secret
has been negociated, the server and client will store the session data in a
safe place. Next time the client reconnect to the same server, it can use the
session data to shortcut the handshake.

What's next
-----------

It's always hard to predict for such a big piece of code and that it only
depends on my *free* time.

 * A deep look at the crypto currently used: improve the current implementations
   of some crypto, looking more about possible timing attacks (specially with the
   current RSA implementation) and flaws.

 * At the same time, make the crypto even more independant of the TLS stack.
   Currently all bulk ciphers (AES, RC4, etc) are pluggable and independant
   from the TLS stack, but the public key and the hashes algorithms are still
   not pluggable in the stack. The goal here is to be able to use someone else
   crypto implementation, possibly one designed for even higher security
   levels and having received more audits than my current implementations.

 * Implements the last remaining part of the handshake protocol missing:
   Certificate Request/Verify packet, and (client) Certificate.
     
 * New fancier exchange mechanism (for example ECDHE-RSA), and as a result support
   for perfect forward secrecy.
