#!/bin/sh

# commit.sh
# 
#
# Created by Andrew Brunner on 9/5/10.
# Modified by Andrew Brunner on 9/21/2010.
# Copyright 2010 Aurawin LLc. All rights reserved.
VOL_SUBVERSION="/Volumes/Subversion"

echo "Searching for mounted volumes..."
if [ ! -d $VOL_SUBVERSION/conf ]; then
  UNMOUNT=true
  echo "Source code Volume not  mounted."
  ./mount.sh
else
  UNMOUNT=false
  echo "Source code Volume already mounted."
fi
./perms.sh
echo "Commiting Aurawin Source to SVN..."
svn commit /Developer/Source
echo "Commiting Aurawin Daily Source to SVN..."
svn commit /Developer/Internet/Aurawin/daily
echo "Commiting Aurawin Portal Source to svn..."
svn commit /Developer/Internet/Aurawin/portal

if [ $UNMOUNT = true ]; then
  ./unmount.sh
fi
