---
author: Vincent Hanquez
title: "my haskell packages update"
tags: haskell,RSA,crypto,package
date: September 10, 2010
---

I haven't really blogged about single package update for a while, as such i'm
going to bundle multiple updates in this post.
<!--more-->

cryptohash 0.6.1
----------------

There's a bunch of new hash. tiger and skein256, skein512. Skein is a SHA-3
hash candidate, that is very flexible (multicore awareness, mac generation,
etc), and the internals are a new start from the traditional Merkle–Damgård
construction (MD5, SHA1, SHA-2). While i'm not a cryptographer, the internal
distinctions of packet and the bit meddling function, make it feels like quite
strong against someone trying to defeat the hashing.

I made some significant improvement over the incremental API, by using a
different marshall algorithm from using a byte per byte map loop to the
bytestring memcpy function, and as such the incremental API is quite close to
the one-go API. The only difference would be if using ridiculously small
bytestring per update.

Portability wise, i received patches to build on Windows and MacOsX, which is
awesome, since all major platforms are supported.

I also moved Data.CryptoHash. hierarchy into Crypto.Hash., however for
compatibility i left the previous hierarchy and add deprecated pragma on all
methods.

And last thing, is the package build with crypto-api. It can be disabled with a
cabal flags, for people not wanting any dependancies apart from bytestring.
crypto-api provides a crypto standardisation of crypto related things, with the
focus on efficiency (using bytestring for example) and providing better
cryptography "security". This is a great package, bundled together by Thomas
DuBuisson (Thanks !). That will help bring haskell crypto in the next century.

cryptocipher 0.2
----------------

I didn't really blog about it in the first place, so cryptocipher is a bundle
of my crypto cipher implementations (RC4, Camellia). Since the first update, I
added a safer RSA implementation than the RSA package, which do decryption
faster when all the cache value are available (see RSA on wikipedia)

certificate 0.3.2
-----------------

Nothing really worth reporting happened. fix a bug where RSA coefficient was
set as an int instead of an integer, and I also added some support to x509.v3
extensions, which is important for TLS.

TLS 0.3.1
---------

TLS is not reaching version 0.3.1, and have a hopefully much better random
generation system that use crypto-api CryptoRandomGen typeclass for its api.

The CRNG is made of a random key, a random IV, and a random counter start. it's using
AES in CBC mode to generate 16 bytes of pseudo random values. The RNG is passing
various tests and is really really close to /dev/urandom generated data.

The package now depends on my RSA implementation, and is fully using the
decrypted cache values for faster decryption. it also permitted to remove the
usage of spoon since i had to catch any possible exception if the decrypting
failed, which is not the case anymore, since the decrypt value returns an
Either value.

And finally, i've started abstracting the Key Exchange functions, so that
others type can be used (DSA, Diffie-Hellman, etc) in the future.
