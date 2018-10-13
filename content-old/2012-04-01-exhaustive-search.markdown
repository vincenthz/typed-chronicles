---
author: Vincent Hanquez
title: "Exhaustive search: fun with multithreads and haskell."
tags: haskell,threads,threadmanager,bruteforce,exhaustive
date: April 1, 2012
---

I recently had to make a couple of exhaustive search, and i'm going to share
the solution i've crafted really quickly. This is for my own future benefits
later so that i don't have to search my disk, but this could be useful and a
good starting point for anyone attempting something similar.
<!--more-->

First exhaustive search is a search into a set of data for one or multiple
values that validate a specific proposition.

An algorithm to search exhaustively is quite easy:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
proposition :: a -> Bool
proposition i = -- True is i validate the proposition, False otherwise

loop i = if proposition i then return i else loop (next i)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A Parallel solution
-------------------

Exhaustive search is usually a massively parallel problem: you can throw lots of
processing unit by splitting the search space into smaller units. This is the main
feature i want to focus on.

To complete this, i'm going to introduce 2 differents objects.

* The first one is a ComputationUnit: Takes a data set to iterate, and test the proposition.

* The second one is a Result type, that represent the value of a terminated ComputationUnit.

Last we need a ComputationManager: It orchestrates ComputationUnits, making
sure ComputationUnit are spawned, and that in case a ComputationUnit find a
candidate terminate the search.

Starting up we need traditional module definition and imports:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
module Main where
import Control.Monad
import Control.Concurrent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


The result type is really simple, either your ComputationUnit terminates
without finding any candidate (Terminated), or the ComputationUnit found a candidate that
validate the proposition.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
data Result a = Terminated | Found a
    deriving (Show,Eq)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is exactly similar to a Maybe type, but for code self-documentation I
chose to create an ad-hoc type.

The Computation type is just a closure that takes a data set "a" and gives back a
Result type. The IO monad is just there for maximum flexibility, but in many case,
propositions are pure and doesn't need any monad to run.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
type ComputationUnit a b = a -> IO (Result b)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Now the only thing missing is the ComputationManager code, first the signature
is self explanatory:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
runManager :: ComputationUnit a b
           -> Int              -- ^ number of simultaneous threads
           -> [a]              -- ^ a list data sets
           -> IO (Result b)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The manager is constantly going to wait for result from ComputationUnit. For communication,
the manager is going to share a Chan with the computation units. It's going to loop over each
data sets by starting computation units and going to wait for result from the computation units.
As soon as one computation get a Found result to the manager, the manager will abort all remaining
threads and return the result.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
runManager computation nbThreads allSets = newChan >>= \chan -> loop chan [] allSets
    where loop :: Chan (Result b)
               -> [ThreadId]    -- this is the ids of every computation units running
               -> [a]           -- this is the remaining data sets to search.
               -> IO (Result b)

	  -- there's no computation units running and we got no data sets left.
          -- simply terminate the algorithm.
          loop _    [] [] = return Terminated

          loop chan threads  dataSets
	      -- fork a new thread running the computation if:
	      -- * the number of threads actually running is less than nbThreads
	      -- * there's at least one data set left.
              | length threads < nbThreads && not (null dataSets) = do
                      let (x:xs) = dataSets
                      tid <- forkIO $ do
                          myTid <- myThreadId
                          ret <- computation x
                          writeChan chan (myTid,ret)
                      loop chan (tid:threads) xs
              -- otherwise we wait for a thread to terminate with a Result:
	      -- on success, we just abort all remaining threads and returns the value found.
	      -- on failure, we just loop again.
              | otherwise = do
                  (tid, res) <- readChan chan
                  let newThreads = filter (/= tid) threads
                  case res of
                      Terminated -> loop chan newThreads dataSets
                      (Found x)  -> mapM_ killThread newThreads >> return $ Found x
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using the code with all your machine cpu cores is very easy and just requires you to
compile the code using -rtsopt and -threaded.

    ghc --make -O2 -threaded -rtsopts <module-name>

And, then you can just run the program:

    ./Computation +RTS -N<#cpu> -RTS

Have fun, and with any luck that will inspire someone to write an even better blog post, or a library.
