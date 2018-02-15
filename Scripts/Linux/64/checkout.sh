#!/bin/sh

# checkout.sh
# 
#
# Created by Andrew Brunner on 9/5/2010.
# Modified by Andrew Brunner on 9/21/2010.
# Copyright 2010 Aurawin LLc. All rights reserved.

NFS_SUBVERSION="phoenix:/media/raid/Developer/Subversion/Aurawin"
VOL_SUBVERSION="/Volumes/Subversion/";

if [ -d $VOL_SUBVERSION/conf ] ; then
  UNMOUNT=false
else
  UNMOUNT=true
  ./mount.sh
fi

if [ ! -d /Developer/Internet ] ; then
  echo Making Internet Source Folder
  mkdir /Developer/Internet
fi
if [ ! -d /Developer/Internet/Aurawin ]; then
  echo Making Aurawin folder
  mkdir /Developer/Internet/Aurawin
fi

echo Checking out aurawin.com from svn...
svn checkout file://$VOL_SUBVERSION/Portal /Developer/Internet/Aurawin/portal

echo Checking out Aurawin Core from svn...
svn checkout file://$VOL_SUBVERSION/Daily /Developer/Internet/Aurawin/daily

echo Checking out Source from Subversion...
svn checkout file://$VOL_SUBVERSION/Pascal /Developer/Source

if [ ! -d /Developer/External/ ]; then
  echo Making Compilers Folder...
  mkdir /Developer/External
fi

if [ ! -d /Developer/External/FPC/.svn ]; then
  echo Checking out Free Pascal Compiler from SVN...
  svn checkout http://svn.freepascal.org/svn/fpc/trunk /Developer/External/FPC
fi
if [ ! -d /Developer/Source/External/Lazarus/.svn ]; then
  echo Checking out Lazarus from SVN...
  svn checkout http://svn.freepascal.org/svn/lazarus/trunk /Developer/External/Lazarus
fi
echo Setting File Permissions...
sudo chgrp -Rf developer /Developer
sudo chmod -Rf ug+rw /Developer 
echo Done.
