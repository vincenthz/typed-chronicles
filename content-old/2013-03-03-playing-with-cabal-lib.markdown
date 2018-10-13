---
author: Vincent Hanquez
title: "Playing with Cabal lib: generating dependencies graph"
tags: cabal,graph,API,library,dependencies,dot
date: March 03, 2013
---

Cabal is a tool for package care and aftercare. Most of cabal
functionalities are available in the library itself, and I've been looking at the
library to write some little tool to solve queries i'ld like to run on the
state of the haskell world.

<!--more-->

I'm not the first one wrapping some Cabal stuff to do some visualization or
random queries. Cool stuff can be found (albeit out-of-date) on the amazing Magnus' blog:

* [Simple cabal parsing](http://therning.org/magnus/archives/514)
* [More fun with cabal, visualising dependencies](http://therning.org/magnus/archives/534)
* [Even more Cabal fun, visualising in 3d](http://therning.org/magnus/archives/543)

The state of the haskell world (as often known as hackage) is represented by
every single cabal files for each package in a big tar file, usually called
00-index.tar, and is retrieved and store by cabal update.

In my case, i want to generate graphs of dependencies for my packages or compute all
the dependencies needed for said package or set of packages. The same tool could be
tweaked to do lot of various queries.

Making of
---------

First we need to open the tar file and enumerate the packages list.  Sadly i
didn't find any way to do that through the cabal lib (dependencies problem on
tar ?), but it's really easy to do.

The tar files list of entries that looks "package name"/"package version"/"cabal file"
and contains the cabal file. First, we need to wrap all the entries available from
this database into a friendly format. we put everything into a simple Map.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}

-- create a type to represent all the packages available in the index.
-- The type is a simple map of package name to [ (version,cabal file content) ]
newtype AvailablePackages = AvailablePackages (M.Map PackageName [(String,GenericPackageDescription)])

-- caveat: we hardcode hackage.haskell.org as the provider, and we could get this
-- from the config file.
loadAvailablePackages :: IO AvailablePackages
loadAvailablePackages = do
    cabalAppDir <- getAppUserDataDirectory "cabal"
    let tarFile = cabalAppDir </> "packages" </> "hackage.haskell.org" </> "00-index.tar"
    foldl' mkMap (AvailablePackages M.empty) . listTar . Tar.read <$> L.readFile tarFile

    where listTar :: Show e => Tar.Entries e -> [([FilePath],L.ByteString)]
          listTar (Tar.Next ent nents) =
                case Tar.entryContent ent of
                    Tar.NormalFile bs _ -> (splitPath $ Tar.entryPath ent, packageDesc bs) : listTar nents
                    _                   -> listTar nents
          listTar Tar.Done             = []
          listTar (Tar.Fail err)       = error ("failed: " ++ show err)

          packageDesc bs = case parsePackageDescription $ UTF8.toString bs of
                                     ParseFailed perror -> Nothing
                                     ParseOk _ a        -> Just a

          mkMap :: AvailablePackages -> ([FilePath], L.ByteString) -> AvailablePackages
          mkMap (AvailablePackages acc) ([(dropTrailingPathSeparator -> packageName),packageVer,_],entBS)
                | packageName == "." = AvailablePackages acc
                | otherwise          = AvailablePackages $ tweak (PackageName packageName)
                                                                 (dropTrailingPathSeparator packageVer)
                                                                 entBS acc
                          where tweak !pname !pver !cfile !m = M.alter alterF pname m
                                  where alterF Nothing  = Just [(pver,cfile)]
                                        alterF (Just z) = Just ((pver,cfile) : z)
          mkMap nacc _ = nacc

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we keep the cabal file content in bytestring format, as parsing them is expensive,
and some operations don't need to iterate through all the packages.

we need a parsed [PackageDescription](http://hackage.haskell.org/packages/archive/Cabal/1.16.0.2/doc/html/Distribution-PackageDescription.html#t:PackageDescription) to be able to figure out the dependencies of a package:

* we first parse the string into a [GenericPackageDescription](http://hackage.haskell.org/packages/archive/Cabal/1.16.0.2/doc/html/Distribution-PackageDescription.html#t:GenericPackageDescription) using [parsePackageDescription](http://hackage.haskell.org/packages/archive/Cabal/1.16.0.2/doc/html/Distribution-PackageDescription-Parse.html#v:parsePackageDescription).
* then we resolve the GenericPackageDescription into a PackageDescription using [finalizePackageDescription](http://hackage.haskell.org/packages/archive/Cabal/1.16.0.2/doc/html/Distribution-PackageDescription-Configuration.html#v:finalizePackageDescription).

GenericPackageDescription represent the parsed file without accounting the
state of the world, and we need to apply our state through
[finalizePackageDescription] to have a useful package description.

Our state of the world include the configuration flags, the platform to use,
and additional constraints like this.

In this case, we don't any generic solution, and we're just considering the
library part and their dependency, we don't want to look at benchmarks, tests
or executables. hence, we hardcode most of the finalization state.

For other specific needs, flattenPackageDescription could be use for a more
thorough dependency generation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
finPkgDesc :: GenericPackageDescription -> Either [Dependency] (PackageDescription, FlagAssignment)
finPkgDesc = finalizePackageDescription [] (const True)
                                        buildPlatform
                                        (CompilerId buildCompilerFlavor (Version [] [])) []
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Outputing the graph
-------------------

Using the excellent dot utility to produce the image, i can just focus on generating the
dot format, which is very easy.

An example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	digraph graph {
	    1 [label="package1"];
	    2 [label="package2"];
	    1 -> 2;
	}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The format is:

* an enumeration of node index with a label
* then a simple "index -> index" of every dependency edges.

To keep track of it we have a simple state monad that keep a PackageName to index table,
and also an Index -> \[Index\] dependency edge table.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
  data St = St
      { nextIndex  :: !Int
      , indexTable :: M.Map PackageName Int
      , depsTable  :: M.Map Int [Int]
      } deriving (Show,Eq)
  
  -- resolve a package name into its dot index. create it if doesn't exists
  resolve pn = get >>= addOrGet
      where addOrGet st = maybe (add st) return $ M.lookup pn (indexTable st)
            add st = put (st { nextIndex = ni st+1, indexTable = M.insert pn ni (indexTable st) }) >> return ni
                      where ni = nextIndex st
  
  isProcessed pn = M.member pn <$> gets depsTable
  modifyDepsTable k f = modify (\st -> st { depsTable = M.alter f k (depsTable st) })
  insertDep i j = modifyDepsTable i f
      where f Nothing  = Just [j]
            f (Just z) = Just (j:z)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we run a recursive loop on every packages specified and their dependencies,
inserting the node and all its dependencies at each step of the loop:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
  run apkgs specifiedPackages = generateDotM $ mapM_ loop specifiedPackages
      where loop pn = do
            pni       <- resolve pn
            processed <- isProcessed pni
            unless processed $ do
                case finPkgDesc <$> getPackageDescription apkgs pn Nothing of
                    Just (Right (d,_)) -> do let depNames = buildDepends d
                                             mapM_ loop depNames
                                             mapM_ (resolve >=> insertDep pni) depNames
                    _ -> return ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And then we output our state into the dot format after running the state monad with our function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell .numberLines}
  generateDotM f = do
      st <- execStateT f (St 1 M.empty M.empty)
      -- generate the format
      putStrLn "digraph projects {"
      forM_ (M.toList $ indexTable st) $ \((PackageName pn), i) -> do
          putStrLn (show i ++ " [label=\"" ++ pn ++ "\"];")
      forM_ (M.toList $ depsTable st) $ \(src,dsts) ->
          mapM_ (\dst -> putStrLn (show src ++ " -> " ++ show dst ++ ";")) dsts
      putStrLn "}"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Displaying the graph
--------------------

Running on cryptohash will produce the following graph:

![Graph for cryptohash](/pictures/posts/2013-03-03-graph-cryptohash.png)

Running with all my packages uploaded on hackage, give the following really big graph:

[Graph for all my packages](/pictures/posts/2013-03-03-graph-all.png)

This was surpringly easy to generate and it gives some visual clues on how
everything fits together, which is what exactly i was after.
