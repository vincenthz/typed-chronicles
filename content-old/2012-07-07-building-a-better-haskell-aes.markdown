---
author: Vincent Hanquez
title: Building a better Haskell AES
tags: crypto,aes
date: June 27, 2012
---

[AES (Advanced Encryption Standard)](http://en.wikipedia.org/wiki/Advanced_Encryption_Standard/) is quite popular and is subject to numerous packages in Haskell:

<!--more-->

 * [AES](http://hackage.haskell.org/package/AES)
 * [Crypto](http://hackage.haskell.org/package/Crypto)
 * [cryptocipher](http://hackage.haskell.org/package/cryptocipher)
 * [intel-aes](http://hackage.haskell.org/package/intel-aes)

However each one of those packages have significant problems related to
performance, integration and completeness.

- Performance: It need to be close to state of art crypto, openssl and co.
  Without this, a lot of cpu time is wasted which would have better use being
  idle (save some energy, save the world), or in some other part of the program
  that use the crypto. I also found hard to convince people to use haskell if
  things run much slower than dynamically executed languages.
- Integration: It need to integrate with the haskell way, pure interfaces is important,
  but also to how block cipher crypto is usually done, i.e. on array of bytes, not integer.
- Completeness: It need to support cryptographic modes: CBC is the strict
  minimum, but some other modes are now widely used too. I don't want to recode
  my own chaining mode each time i use crypto.

Package: AES
------------

Using Gladman's C implementation, in a easy to use and pure interface bindings. But:

- no [AESNI]: only basic C software implementation, means it's not competitive despite
  the heavily optimised software implementation.
- Provides all the basic modes (CBC, CFB, OFB, CTR) but nothing more advanced.

Package: Crypto
---------------

This is probably the oldest package, and offers a comprehensive set of cryptographic
algorithm. the main problems of this package:

- pure haskell implementation of AES: 1 call to AES encryption or decryption is really slow.
- not practical to use: the actual calls take a Word128 and gives back a Word128, which
  means that the user need to do the convertion to and from bytestring on his own, and
  that the data is copied between the bytestring and the Word128.
- Only CBC is available as a crypto mode, although the signature doesn't look practical to use.

Package: cryptocipher
---------------------

This is my own package, and offers just like Crypto, a bunch of cryptographic algorithm. The main
problem is:

- pure haskell implementation with unsafe tricks: much better than Crypto, but still too slow to be useful.
- provides modes through crypto-api, resulting in slow modes implementation.

Package: intel-aes
------------------

This is theoretically the fastest AES package available, as it reuse intel's
hw assembly implementation and Gladman's AES code. however it does have
significant problems:

- build system problems: the package contains object code binaries and assembly source files,
  which is hard to integrate with cabal; The result is a harder (than it should) to build package.
- no portability: only x86 and amd64 architecture are supported; building on
  anything else will result in build failure and an unusable package.
- doesn't actually export crypto primitives (?): it seems to be just about
  providing a really fast RNG for now.

Can't we do better ?
--------------------

If you'ld ask me, the best package considering performance and portability as
the two most important items is, AES. cryptocipher is my experiment to push
haskell tricks to the limits, and learn things on the way. While it provides
some nice improvement upon the Crypto implementation, it is still lacking the
punch of a C SW implementation.

AES is really important nowadays, as everything crypto related does use it
(TLS, SSH, ..). This is so important that Intel and AMD added support in
hardware for optimising the operations, the [AESNI] instruction set. The hardware
acceleration offers stellar performance and also increased security (timing
attack relative to sboxes tables on software implementation).

As such a modern AES solution, need to support [AESNI] nowadays. I also think
it is important that other architectures or older cpu not having the hw
instructions, work transparently the same.

Last but not least, the cryptographic world added lots of cool stuff to the
field, that despite being already 10 years old, developers still do not know or
use. To name them, [Authenticated encryption](http://en.wikipedia.org/wiki/Authenticated_encryption) and [Disk encryption theory](http://en.wikipedia.org/wiki/Disk_encryption_theory) are 2 domains
where cool stuff can be found. I think it's important to provides some of those mode too.

One AES to rules them all
-------------------------

With this requirements list, I gathered piece of code i've written, and bundled
them in a consistant package. This is how
[cipher-aes](http://hackage.haskell.org/package/cipher-aes) was born.

First and foremost this is an AESNI accelerated package: Fortunate folks with the
necessary hardware will automatically make good use of it.

Second, a software implementation for less fortunate folks so that everything transparently
works on every platforms and older x86 cpus.

Third, the implementation supports basic mode ECB, CBC, CTR, but also new cool
modes, GCM and XTS. [GCM (Galois Counter mode)](http://en.wikipedia.org/wiki/Galois/Counter_Mode) is an authenticated
encryption scheme, and
[XTS](http://en.wikipedia.org/wiki/Disk_encryption_theory#XEX-based_tweaked-codebook_mode_with_ciphertext_stealing_.28XTS.29)
is a disk encryption scheme.

Few important things are missing for now, like [AESNI] support for AES 192 and
AES 256. Also the Galois field multiplication is unoptimised for now (not using
x86 hw acceleration, nor having an optimised sw implementation).

Performance
-----------

A simple benchmark of AES encrypting in ECB mode a 16k bytestring yield the following results:

- Crypto      : 56732.6 us
- cryptocipher: 1391.5 us
- AES         : 74.3 us
- cipher-aes  : 6.8 us

More benchmarks are available in the Benchmarks directory of the repository.

With those results, i can say that the goal behind cipher-aes has been
achieved. There is still room for further improvement, and the backup SW
implementation isn't the fastest, and would probably compare unfavorably to the
state of the art.

Bugs
----

This is a relatively big and complex package, and didn't have as much field
testing than other older solutions. Build system problems (specially related to
the C), portability problems, and runtime problems (on big endian for example)
could very well bite.

Let me know on the [github bugtracker](https://github.com/vincenthz/hs-cipher-aes/issues), or on emails of any problems.

[AESNI]: http://en.wikipedia.org/wiki/AES_instruction_set "X86 AES Instruction Set"
