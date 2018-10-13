---
author: Vincent Hanquez
title: A little guide of date and time in Haskell
tags: blog,date,time,calendar,guide
date: December 16, 2011
---

Every now and then, i have to use date and time in haskell program or
library, and always end up being confused on how to do simple things with
it. It doesn't looks like I'm alone, and same topic as been covered by [Magnus' blog](http://therning.org/magnus/archives/389)

<!--more-->

It's mostly written for my own (later) benefits, but it might be useful in
general for anyone using the time modules.

This guide assume you're leaving on earth, and use the time package
version 1.4, although i think most of it works for older time package too.

In the beginning there was seconds
----------------------------------

Counting seconds is the most usual way to track time for computer.  This
is exactly what DiffTime is for in the [Data.Time.Clock](http://hackage.haskell.org/packages/archive/time/1.4/doc/html/Data-Time-Clock.html)
module, and as a bonus DiffTime can track time up to the picosecond.

let's create a 10 seconds value:

    > let t = fromIntegral 10 :: DiffTime

or 2253 milliseconds:

    > let t = fromRational 2.253 :: DiffTime

You can also use dedicated conversation function that may required less
type annotation by using secondsToDiffTime and picosecondsToDiffTime:

    > let t = secondsToDiffTime 10 -- 10 seconds
    > let t = picosecondsToDiffTime 2253000000000 -- 2253 milliseconds

Seconds to time
---------------

A number of seconds need to have a referencial to make sense as time.
In the POSIX world, it's common to track time as the number of seconds
since 1st of january 1970 (EPOCH). Everything to use Posix time is
available in the [Data.Time.Clock.POSIX](http://hackage.haskell.org/packages/archive/time/1.4/doc/html/Data-Time-Clock-POSIX.html) module.

First the POSIXTime, like DiffTime is easily converted using the standard
fromIntegral or fromRational. POSIXTime is just a type alias from
NominalDiffTime, which is suppose to represent a time with a implicit starting
point.

     -- 10 seconds after Unix EPOCH.
     > let unixTime = fromIntegral 10 :: POSIXTime

The main type for tracking time in Haskell, is UTCTime.

Converting POSIXTime to and from UTCTime is very easy, and is available
through the posixSecondsToUTCTime and utcTimeToPOSIXSeconds.

     -- 10 seconds after unix epoch.
     > let utctime = posixSecondsToUTCTime $ fromIntegral 10

You can use utcTimeToPOSIXSeconds to converts back from UTCTime.

Localization
------------

Time is often not handled through UTC when dealing with humans, but
localized with timezones. Time localization is provided through the
[Data.Time.LocalTime](http://hackage.haskell.org/packages/archive/time/1.4/doc/html/Data-Time-LocalTime.html) module.

First the TimeZone type is self explanatory and can be easily created
either with minutesToTimeZone or hoursToTimeZone.

     > let tz = hoursToTimeZone 2 -- create a +0200 timezone

Then we can convert UTCTime back and forth to a ZonedTime using the
utcToZonedTime and zonedTimeToUTC functions.

     > let time = posixSecondsToUTCTime $ fromIntegral 10
     > let ztime = utcToZonedTime tz utcTime

Current time
------------

It's very easy to get the current time through either UTCTime or ZonedTime:

     getCurrentTime :: IO UTCTime
     getCurrentTimeZone :: IO TimeZone
     getZonedTime :: IO ZonedTime

or as POSIX Time:

     getPOSIXTime :: IO POSIXTime

Formatting time
---------------

Time can be converted from and to their english representations using
the [Data.Time.Format](http://hackage.haskell.org/packages/archive/time/1.4/doc/html/Data-Time-Format.html) module, with the parseTime and formatTime functions.

parseTime and formatTime are able to handle multiple types that represent time.
Every type covered by the ParseTime and FormatTime typeclasses respectively can
be used.

You need to import System.Locale from the old-locale package to get the
defaultTimeLocale or create a locale type yourself.

First starting with formatTime which transform a time type, into a string
that is parametrized through a format string.

The format string (printf-style) is quite versatile and allow all sorts of
various formats. [Docs](http://www.haskell.org/ghc/docs/7.2.2/html/libraries/time-1.2.0.5/Data-Time-Format.html)

     > let time = posixSecondsToUTCTime $ fromIntegral 10
     > formatTime defaultTimeLocale "%c" time 
     "Thu Jan  1 00:00:10 UTC 1970"

The opposite of this operation, transforming a string into a time type, is
handled by parseTime and have similar parameters:

     > parseTime defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970" :: Maybe UTCTime
     Just 1970-01-01 00:00:10 UTC


Organizing time
---------------

We usually organize time with calendar, and haskell is well covered
for calendars through the [Data.Time.Calendar](http://hackage.haskell.org/packages/archive/time/1.4/doc/html/Data-Time-Calendar.html) hierarchy.

The usual calendar we use is the gregorian calendar, also known as the
Western calendar.

First the Day type is very easy to marshall between Integral types.

    > let day = fromGregorian 1970 1 1 -- year 1970, month 1, day 1

And to get the content of a Day back:

    > let (year, month, dayOfMonth) = toGregorian day

Finally, we can now create UTCTime directly using Day and DiffTime:

    > let utctime = UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)

And to get parts of a UTCTime:

    > let day = utctDay utctime
    > let difftime = utctDayTime utctime

At the end of time
------------------

That's it, i think i covered every piece that I usually use, and that are
the most useful. Once you understand the basic types and how they are
related to each others, you can explore the modules in more depth, to find
types and functions that may be useful.
