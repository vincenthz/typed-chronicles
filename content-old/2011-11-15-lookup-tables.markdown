---
author: Vincent Hanquez
title: Fast lookup tables in haskell
tags: lookup,tables,array,performance,haskell
date: November 15, 2011
---

A lookup table in Wikipedia's words:

> In computer science, a lookup table is a data structure, usually an
> array or associative array, often used to replace a runtime computation
> with a simpler array indexing operation.

It used to be used extensively when cpu were short of power, but it's
still useful nowadays to squeeze performance for heavily used pieces of
code.

The usual structure behind lookup tables is a tightly packed array, which
is not necessarily a very haskelly structure.

Another thing you're looking for with lookup table is, a limited values that
the indexes can take.  Either through the type directly (for example Word8) or
through a fast mathematic operations (for example mod). You can't resonably
create lookup tables over a Word32 type for example.

First we need something that we want to convert to lookup table. So as an
example, i'll take the simple function that converts an hexadecimal
Character to an Int.

The following piece of code represent this convertion, expressed naively:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
    module N (hex) where
    hex c
            | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A'
            | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a'
            | otherwise            = 0xff
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Benchmarks
----------

Secondly let's put in place some benchmarks using
[Criterion](http://hackage.haskell.org/package/criterion).  The naive piece of
code is going to be the reference implementation, and the benchmarks are going
to be executed with differents levels of optimisations (no, O, and O2)

The Benchmark itself is using Char for conveniency, where it should use Word8
to be accurate and *safe*. Some of the code below would be unsafe to run if run
with the Char type and not having any validation function in front, or a casting to
a Word8.

This is the benchmark:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
import Criterion.Main
import qualified N -- naive computing

import Data.List (foldl')

-- simple benchmark function
dataList = take 100000 $ cycle ['\x1'..'\xf0']
process hexF = foldl' (\acc c -> acc + hexF c) 0
    
main = defaultMain [
        bgroup "lookup"
                [ bench "naive" $ whnf (process N.hex) dataList
                ]
        ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It yields the following result for the naive implemetation:

optimisations   report
-------------   ---------------------------------------------------
no              mean: 17.29674 ms, lb 17.16636 ms, ub 17.47595 ms
O               mean: 1.595945 ms, lb 1.569064 ms, ub 1.628741 ms
O2              mean: 1.470591 ms, lb 1.452113 ms, ub 1.495270 ms

Vector
------

Ok, now let's try improving this number. First we're going to try
to mimics more low level language by using an array through the
[Vector](http://hackage.haskell.org/package/vector)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
module V (hex) where
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

hex c = vtable ! (fromEnum c)
vtable = V.fromList table

\_\_ = 0xff
table :: [Int]
table = [
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
         0, 1, 2, 3, 4, 5, 6, 7,  8, 9,__,__,__,__,__,__,
        10,11,12,13,14,15,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,10,11,12,13,14,15,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,

        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__ ]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It gives:

optimisations   report
-------------   ---------------------------------------------------
no              mean: 9.199355 ms, lb 9.072765 ms, ub 9.364172 ms
O               mean: 1.337789 ms, lb 1.319601 ms, ub 1.362063 ms
O2              mean: 1.398385 ms, lb 1.383462 ms, ub 1.423932 ms

This is a bit disappointing, you would expect slighly better performance here,
but since we know we are doing safe indexing we can replace the indexing operator (!)
by the unsafeIndex operation. doing so lead to:

optimisations   report
-------------   ---------------------------------------------------
no              mean: 7.738883 ms, lb 7.665292 ms, ub 7.845997 ms
O               mean: 972.9515 us, lb 959.3614 us, ub 991.1686 us
O2              mean: 517.0818 us, lb 510.2732 us, ub 526.1961 us

We're back in business, but one thing that you could notice while looking at
core (or the assembly), is that the list is stored as a list, and thus the
vector is created at runtime. I'm not sure if there's any way to do better
through the vector interface to create more efficently the table.

Matching
--------

Instead of taking a lowlevel approch, maybe a high level approch will do
better.  Pattern matching is one of the very nice feature of advanced languages
like OCaml and Haskell, and it usually have good performance result.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
module M (hex) where

hex :: Char -> Int
hex '\x30' = 0
hex '\x31' = 1
hex '\x32' = 2
hex '\x33' = 3
hex '\x34' = 4
hex '\x35' = 5
hex '\x36' = 6
hex '\x37' = 7
hex '\x38' = 8
hex '\x39' = 9
hex '\x40' = 10
hex '\x41' = 11
hex '\x42' = 12
hex '\x43' = 13
hex '\x44' = 14
hex '\x45' = 15
hex '\x61' = 10
hex '\x62' = 11
hex '\x63' = 12
hex '\x64' = 13
hex '\x65' = 14
hex '\x66' = 15
hex _      = 255
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is really simple and give really good result. This is somewhat surprising
and also show the amount of optimisation GHC is putting in the pattern match.

optimisations   report
-------------   ---------------------------------------------------
no              mean: 3.389348 ms, lb 3.333397 ms, ub 3.475168 ms
O               mean: 909.5452 us, lb 894.9783 us, ub 928.9299 us
O2              mean: 834.9938 us, lb 820.8469 us, ub 854.5966 us

with no, and default O optimisation, it beats the vector-unsafe interface.

As a note of warning: the pattern matching approch works very well when the
number of case stays low, however when there's too many cases, the compilation
slow down massively and the runtime slow down too.

ByteString
----------

The high level approch works quite well, however it really feel that something
more could be done:

 * Pattern matches performance degenerates if the number of cases increases.
 * Vectors are not generated at compile time.

We can try to leverage the OverloadedStrings extension, to create an array of
bytes through the ByteString interface.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
{-# LANGUAGE OverloadedStrings #-}
module B (hex) where

import Data.ByteString (ByteString, index)
import Data.ByteString.Char8 ()
import Data.ByteString.Unsafe (unsafeIndex)

hex :: Char -> Int
hex c = fromIntegral (table `index` fromEnum c)

table :: ByteString
table =
	"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
	\\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It yields:

optimisations   report
-------------   ---------------------------------------------------
no               mean: 8.543990 ms, lb 8.434344 ms, ub 8.708120 ms
O                mean: 1.090147 ms, lb 1.076412 ms, ub 1.110581 ms
O2               mean: 1.091043 ms, lb 1.077365 ms, ub 1.111452 ms

The test is also repeated replacing the index by an unsafeIndex:

optimisations   report
-------------   ---------------------------------------------------
no              mean: 8.270395 ms, lb 8.186708 ms, ub 8.399394 ms
O               mean: 856.9175 us, lb 841.1300 us, ub 879.6006 us
O2              mean: 505.5361 us, lb 498.7377 us, ub 514.6283 us

While the vector and bytestring should in theory yield the same performance
since they're both arrays, the bytestring get a slight boost for O and O2.
This might be due only to the overloadedStrings extension which allow
compilation times representation of the packed array.

Magic Hash
----------

MagicHash is the name of the GHC extension to be able to use GHC primitives
directly. The primitive approch, is building on top of the bytestring approch.
The only major difference is the usage of a raw Addr#, which represent an address
in memory (just like a void \* in C), instead of the more classical ByteString.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
{-# LANGUAGE MagicHash, BangPatterns #-}
module F (hex) where

import Data.List
import GHC.Prim
import GHC.Word
import GHC.Types

data Table = Table !Addr#

hex :: Char -> Int
hex (C# i) = I# (word2Int# $ indexWord8OffAddr# addr (ord# i))
	where !(Table addr) = table

table = Table
	"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
	\\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
	\\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It yields the most impressive performance so far:

optimisations   report
-------------   ---------------------------------------------------
no              mean: 5.318712 ms, lb 5.255720 ms, ub 5.410266 ms
O               mean: 725.3248 us, lb 712.3446 us, ub 748.3496 us
O2              mean: 502.6127 us, lb 495.5847 us, ub 512.6601 us

While it doesn't beat the pattern matching without optimisation (which is odd),
it's consistantly the fastest for O (default optimisation level with cabal) and
O2.

 method \\ optim      none          O            O2
 ---------------  -----------  -----------  -----------
           naive  17.29674 ms  1.595945 ms  1.470591 ms
          vector  9.199355 ms  1.337789 ms  1.398385 ms
   vector-unsafe  7.738883 ms  972.9515 us  517.0818 us
        matching  3.389348 ms  909.5452 us  834.9938 us
         bytestr  8.543990 ms  1.090147 ms  1.091043 ms
  bytestr-unsafe  8.270395 ms  856.9175 us  505.5361 us
       magichash  5.318712 ms  725.3248 us  505.6127 us

![All levels](/pictures/posts/2011-11-15-all.png)

![O and O2 levels](/pictures/posts/2011-11-15-optim.png)

Wrapping up
-----------

The lookup table are at the time of writing far best represented through the
primitives, but unfortunately it does comes at the expense of portability to
different haskell compiler, and also doesn't necessarily feels like very nice
haskell code.

As such it should probably be reserved to cases where you really need to
squeeze the last drop of performance and where the lookup is key to
differenciate between a slow and fast piece of code.

As relatively close 2nd, the bytestring and vector interface are both good
candidates as long as the unsafeIndex function is used. unsafe indexing is
perfectly safe in static cases where you know what's your input range.

Pattern matching is quite interesting, since it's not really in the same
category as the others, but yet manage to offer really good performance.
With pattern matching you can also more easily covers a sparse range of input,
which is harder and more time consuming to do with typical lookup tables.

In any case, the most important thing is: benchmarks ! Do not blindly
optimise simple functions; the big performance picture is the most important
place where you should start.

You can find some real world usage of primitive lookup tables:

* [Cryptocipher's AES](http://github.com/vincenthz/hs-cryptocipher/blob/master/Crypto/Cipher/AES.hs)
* [base16-bytestring](http://github.com/bos/base16-bytestring/blob/master/Data/ByteString/Base16.hs) (and the [pull request](http://github.com/bos/base16-bytestring/pull/1) containing the performance improvement report)
