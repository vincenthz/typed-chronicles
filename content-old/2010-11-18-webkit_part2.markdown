---
author: Vincent Hanquez
title: "using webkit in haskell for fun and profit #2"
tags: haskell,webkit,tutorial
date: November 18, 2010
---

Following from the previous article, where I demonstrated how easy it is to
instanciate your own webkit. I'll try now to go in more details, and create
a simple hackage browser.

<!--more-->

Organizing the interface
------------------------

First we need to organize the window. It's something that can be done with
glade, specially when your project start to be complex.

In this windows, we wants a adjustable paned with on the left a list of
packages we have, and on the right a webkit window with the haddock web page of
a package.

~~~~~~~~~~~~ {.haskell .numberLines}
    	window <- windowNew
    	hpaned <- hPanedNew
    
    	-- the widgets on the left
    	swView <- scrolledWindowNew Nothing Nothing
    	view   <- treeViewNew
    
    	-- the widgets on the right
    	swWv   <- scrolledWindowNew Nothing Nothing
    	wv     <- webViewNew
    
    	set window
    		[ containerChild       := hpaned
    		, windowDefaultWidth   := 500
    		, windowDefaultHeight  := 400
    		, containerBorderWidth := 2
    		]
    	set swWv [ containerChild := wv ]
    	set swView [ containerChild := view ]
    
    	panedAdd1 hpaned swView
    	panedAdd2 hpaned swWv
~~~~~~~~~~~~


Populating the package list
---------------------------

The package list is a treeview. a working treeview is composed of two parts:

* a model where you store your data
* and a view where you describe how you want your column linked with the data
  in the model

First we get the list of package we have already installed from ghc-pkg list.
We create the following procedure to do so:

~~~~~~~~~~~~ {.haskell .numberLines}
    import System.Process
    
    listAllPackages :: IO [String]
    listAllPackages = do
    	(_, out, _) <- readProcessWithExitCode "ghc-pkg"
    	                                       [ "list", "--simple-output" ] ""
    	return $ words out
~~~~~~~~~~~~

then we set the model, and then the view.

~~~~~~~~~~~~ {.haskell .numberLines}
    	pkg   <- listAllPackages
    
    	store <- listStoreNew (sort pkgs)
    	treeViewSetModel view store
    
    	-- we set the view by turning off the headers of the column,
    	-- then we create a new column with a text renderer, since we want
    	-- to render a string of character. and finally we set the function
    	-- that's making the link between the store model and the displayed
    	-- text and append the new column to the treeview
    
    	treeViewSetHeadersVisible view False
    
    	col      <- treeViewColumnNew
    	renderer <- cellRendererTextNew
    	treeViewColumnPackStart col renderer True
    
    	cellLayoutSetAttributes col renderer store $ \row -> [ cellText := row ]
    	treeViewColumnSetTitle col "Package"
    	treeViewAppendColumn view col
~~~~~~~~~~~~

Connecting everything together
------------------------------

The last step if connecting actions that the user can takes.

As such we only have 2 actions, the standard event when the window is close
and what happens when the user clicks on a package on the left side.

The first event was already seen in previous article:

~~~~~~~~~~~~ {.haskell .numberLines}
    	onDestroy window mainQuit
~~~~~~~~~~~~

The second one is new; we want to connect the onRowActivated event, which
happens when the user is pressing enter with the treeview on focus, or when
double clicking on item. This event callbacks with a treepath and treeviewcolumn.
We don't need the later since we only have one column as our data.

The treepath is a list of indexes from parent. for example [2,3] would mean
the 3rd entry on the 2 second entry node. Because we're using a flat list,
compared to a tree, we only have one index in the list, which represent
the row on the store that we have just clicked.

Now that we have the package that we clicked, we can just load the uri in
the webkit widget.

~~~~~~~~~~~~ {.haskell .numberLines}
    	onRowActivated view (\treepath _ -> do
    		v <- listStoreGetValue store (head treepath)
    		webViewLoadUri wv ("http://hackage.haskell.org/package/" ++ v)
    		)
~~~~~~~~~~~~

For practical purpose, here the full program source with all the necessary
initializations:

~~~~~~~~~~~~ {.haskell .numberLines}
    import System.Process
    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.WebKit.WebView
    import Data.List
    
    listAllPackages :: IO [String]
    listAllPackages = do
    	(_, out, _) <- readProcessWithExitCode "ghc-pkg" [ "list", "--simple-output" ] ""
    	return $ words out
    
    main = do
    	pkgs <- listAllPackages
    	_      <- initGUI
    
    	window <- windowNew
    	hpaned <- hPanedNew
    
    	-- the widgets on the left
    	swView <- scrolledWindowNew Nothing Nothing
    	view   <- treeViewNew
    
    	-- the widgets on the right
    	swWv   <- scrolledWindowNew Nothing Nothing
    	wv     <- webViewNew
    
    	set window
    		[ containerChild       := hpaned
    		, windowDefaultWidth   := 500
    		, windowDefaultHeight  := 400
    		, containerBorderWidth := 2
    		]
    	set swWv [ containerChild := wv ]
    	set swView [ containerChild := view ]
    
    	panedAdd1 hpaned swView
    	panedAdd2 hpaned swWv
    
    	store <- listStoreNew (sort pkgs)
    	treeViewSetModel view store
    
    	treeViewSetHeadersVisible view False
    	col      <- treeViewColumnNew
    	renderer <- cellRendererTextNew
    	treeViewColumnPackStart col renderer True
    	cellLayoutSetAttributes col renderer store $ \row -> [ cellText := row ]
    	treeViewColumnSetTitle col "Package"
    	treeViewAppendColumn view col
    
    	onDestroy window mainQuit
    	onRowActivated view (\treepath _ -> do
    		v <- listStoreGetValue store (head treepath)
    		webViewLoadUri wv ("http://hackage.haskell.org/package/" ++ v)
    		)
    
    	widgetShowAll window
    	mainGUI
~~~~~~~~~~~~

Which after compiling and running, gives you the following interface:

[image|example1.jpg]

That's all for now. in part3, I'll describe more about webkit internals.
