---
author: Vincent Hanquez
title: "darcs to git mirror script"
tags: haskell,darcs,git,mirror
date: September 17, 2010
---

The following script permits to mirror a darcs repository into a git
repository; It's also working in incremental mode since darcs-fastconvert
added support for marksfile (>= 0.2.0) and it's really fast.

<!--more-->

it requires darcs, darcs-fastconvert, and git. it need to be run in the
darcs repository to be converted. the script does try to pull darcs
change, and then will run the darcs fast-export and git fast import in a
pipe.

    #!/bin/sh
    
    DARCS=darcs
    DARCSCONV=darcs-fastconvert
    GIT=git
    FIRST=0
    
    if [ ! -d "_darcs" ]; then
    	echo "not a darcs repository"
    	exit 1
    fi
    
    if [ ! -d ".git" ]; then
    	echo "initializing mirror"
    	${GIT} init
    	FIRST=1
    fi
    
    ${DARCS} pull
    
    DARCSMARK=.git/darcs.mark
    GITMARK=.git/git.mark
    
    READDARCSMARK=""
    if [ -f ${DARCSMARK} ]; then
    	READDARCSMARK="--read-marks=${DARCSMARK}"
    fi
    READGITMARK=""
    if [ -f ${GITMARK} ]; then
    	READGITMARK="--import-marks=${GITMARK}"
    fi
    
    ${DARCSCONV} export $READDARCSMARK --write-marks=${DARCSMARK} | \
    	${GIT} fast-import $READGITMARK --export-marks=${GITMARK}
    
    if [ $FIRST -eq 1 ]; then
    	${GIT} gc --aggressive
    fi
    
    exit 0

Note that mirroring the other way around is easy too; You just need to
swap git by darcs, and change the pipe direction from **darcs export** to
**git export** and **git import** to **darcs export**

Drop that in your cronjob with reasonable wakeup values, and do enjoy the
fact that you can work with your favorite DVCS system with a foreign
origin.
