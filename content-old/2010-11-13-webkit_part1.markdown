---
author: Vincent Hanquez
title: "using webkit in haskell for fun and profit #1"
tags: haskell,webkit,tutorial
date: November 11, 2010
---

In this serie of articles, I'll give a tutorial to use the webkit library.
<!--more-->

First, webkit is a library to render web pages, that is very
easy to use, fast and compliant with most standards out there
(CSS1, CSS2, CSS3, etc).  It's in use in Safari, Google
Chrome, and in multiples smaller browser on unix (midori,
uzbl, epiphany, etc).

A commented example is worth a thousand words, so:

~~~~~~~~~~~~ {.haskell .numberLines}
    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.WebKit.WebView
    
    main = do
    	-- initialize Gtk
    	_ <- initGUI
    
    	-- create a new window, a scrolled window, and a new webview
    	w  <- windowNew
    	sw <- scrolledWindowNew Nothing Nothing
    	wv <- webViewNew
    
    	-- set the child of the parent to the scrolled window,
    	-- and set some others attributes
    	set w
    		[ containerChild       := sw
    		, windowDefaultWidth   := 500
    		, windowDefaultHeight  := 400
    		, containerBorderWidth := 2
    		]
    	-- set the child of the scrolled windows to the webview.
    	set sw [ containerChild := wv ]
    
    	-- load slashdot.org on the webview.
    	webViewLoadUri wv "http://slashdot.org"
    
    	-- on destroying event, we quit the mainloop
    	onDestroy w mainQuit
    	-- show all widgets starting from the root window
    	widgetShowAll w
    	-- start GTK main loop
    	mainGUI
~~~~~~~~~~~~

Compiling and running the previous example, gives you this:

[image|example1.jpg]

Now let's add an entry URL bar to load website on demand.
first we need to reorganize the layout, by adding a vertical
box, that will contains first an entry, and then the scrolling
window.

Then we remove the default uri we load, and instead add a
signal callback, when the entry is activated (by pressing
enter). when the signal is triggered, we get the text
back from the widget, and then load into the webview the
url.

Here's the full code to do that:

~~~~~~~~~~~~ {.haskell .numberLines}
    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.WebKit.WebView
    
    main = do
    	-- initialize Gtk
    	_ <- initGUI
    
    	-- create a new window, a vertical box, to hold an
    	-- entry bar, and a scrolled window, and a webview
    	w   <- windowNew
    	v   <- vBoxNew False 2
    	bar <- entryNew
    	sw  <- scrolledWindowNew Nothing Nothing
    	wv  <- webViewNew
    
    	-- set the child of the parent to the vbox,
    	-- and set some others attributes
    	set w
    		[ containerChild       := v
    		, windowDefaultWidth   := 500
    		, windowDefaultHeight  := 400
    		, containerBorderWidth := 2
    		]
    	-- now pack the bar and then the scrolled window
    	boxPackStart v bar PackNatural 0
    	boxPackStart v sw PackGrow 0
    
    	-- set the child of the scrolled windows to the webview.
    	set sw [ containerChild := wv ]
    
    	-- when we activate the entry bar (typically pressing enter),
    	-- we execute the following code: get the text from the entry and
    	-- load the uri from the entry.
    
    	onEntryActivate bar (entryGetText bar >>= webViewLoadUri wv)
    	-- on destroying event, we quit the mainloop
    	onDestroy w mainQuit
    	-- show all widgets starting from the root window
    	widgetShowAll w
    	-- start GTK main loop
    	mainGUI
~~~~~~~~~~~~

that gives you the following after entering "http://slashdot.org" and pressing
enter in the url bar (notice the entry bar at the top):

[image|example2.jpg]

That's all for now. In the next part I'll make a small hackage
browser to show how easy it is to make a specific dedicated
browser.
