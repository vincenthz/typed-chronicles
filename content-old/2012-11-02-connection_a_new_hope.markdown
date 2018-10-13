---
author: Vincent Hanquez
title: "Connection: A new hope for client network connection."
tags: tls,connection,network,socks,haskell
date: November 02, 2012
---

Opening connection is nowadays a complicated business:
One of the leading complication provider is TLS. In itself,
there's nothing complicated about it, however the sheer amount
of configurability make it too fiddly for casual programmer
that want to open a simple connection to send bytes back and forth.
<!--more-->

For example, you want to have to good default behavior for TLS sessions,
certificate caching, certificate exceptions to name a few.

To solve this problem, i've create the connection package.
It bundles simple default with easy settings, and will hopefully
reduce the amount of general code that a typical client connection
requires by bundling together:

- network: open raw sockets.
- socks: for all SOCKS proxy needs.
- tls  : for all SSL/TLS need, session, STARTTLS like command.

Here is the [Connection package](http://hackage.haskell.org/package/connection) and its [API](http://hackage.haskell.org/packages/archive/connection/0.1.0/doc/html/Network-Connection.html)

You can also found the code and more example in [connection repository](https://github.com/vincenthz/hs-connection)

Examples
--------

Let's start with a simplest example: i want to connect to "www.example.com"
, on port 80, and close the connection straight away. It's as simple as using
the network package directly, and doesn't add any overhead.

~~~~~~~~~~~~ {.haskell .numberLines}
import Network.Connection
import Data.Default

main = do
    ctx <- initConnectionContext
    con <- connectTo ctx $ ConnectionParams
                              { connectionHostname  = "www.example.com"
                              , connectionPort      = fromIntegral 80
                              , connectionUseSecure = Nothing
                              , connectionUseSocks  = Nothing
                              }
    connectionClose con
~~~~~~~~~~~~

Now what if i want to connect to the same host on the same port, but
using the SOCKS\_PROXY environment variable if it exists to connect
to a SOCKS proxy. Turns out it's very simple, and simply tweaking the
connectionUseSocks setting:

~~~~~~~~~~~~ {.haskell .numberLines}
            , connectionUseSocks  = Just def
~~~~~~~~~~~~

socks isn't hard to use in general anyway, but what if we want to use
a SSL connection on port 443 (https). Turns out this is just as simple, and
the only change required is to change connectionUseSecure to 'Just def', as such:

~~~~~~~~~~~~ {.haskell .numberLines}
    con <- connectTo ctx $ ConnectionParams
                              { connectionHostname  = "www.example.com"
                              , connectionPort      = fromIntegral 443
                              , connectionUseSecure = Just def
                              , connectionUseSocks  = Nothing
                              }
~~~~~~~~~~~~

What's next
-----------
* provide the ability to turn on/off TLS session
* provide certificate verification caching capability (just like http-conduit) when session is not convenient enough.
* provide client certificate.
* provide the ability to store certificate exceptions, so that non valid certificates (expired, hostname non matching, etc) can be overriden, or to add extra verifications for valid certificate through TOFU (Trust on first use) akin to ssh known\_hosts

