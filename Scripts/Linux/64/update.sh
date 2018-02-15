#!/bin/sh

# update.sh

NFS_SUBVERSION="phoenix:/media/raid/Developer/Subversion/Aurawin"
VOL_SUBVERSION="/Volumes/Subversion/";

if [ -d $VOL_SUBVERSION/conf ] ; then
  UNMOUNT=false
else
  UNMOUNT=true
  ./mount.sh
fi
echo Updating Source from Subversion...
svn update /Developer/Source

if [ ! -d /Developer/Internet ] ; then
  echo Making Internet Source Folder
  mkdir /Developer/Internet
fi

echo Updating out aurawin.com from svn...
svn update /Developer/Internet/Aurawin/portal

echo Checking out Aurawin Core from svn...
svn update /Developer/Internet/Aurawin/daily

if [ $UNMOUNT = true ]; then
  ./unmount.sh
fi

svn update /Developer/External/FPC
svn update /Developer/External/Lazarus

/Developer/perms.sh

