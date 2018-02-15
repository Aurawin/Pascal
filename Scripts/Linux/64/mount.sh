#!/bin/sh

# mount.sh
# Copyright 2010-2012 Aurawin LLC. All rights reserved.

OS_NAME=$(uname)

NFS_SUBVERSION="phoenix:/media/raid/Developer/Subversion/Aurawin"
VOL_SUBVERSION="/Volumes/Subversion/";

OS_NAME=$(uname)
MOUNT_OPTIONS="-o -P"
echo "Checking Operating system ($OS_NAME)"
if [ $OS_NAME = "Linux" ]; then
  echo "Setting Linux NFS options"
  MOUNT_OPTIONS="-o vers=4"
fi

if [ ! -d /Volumes ]; then
  echo "Creating Volumes Folder..."
  sudo mkdir /Volumes
fi
if [ ! -d $VOL_SUBVERSION ]; then
  echo "Creating Subversion Folder $VOL_SUBVERSION..."
  sudo mkdir $VOL_SUBVERSION
fi

if [ ! -d "$VOL_SUBVERSION/conf" ]; then
  echo "Mounting Subversion Volume for ($OS_NAME)..."
  sudo mount -t nfs $MOUNT_OPTIONS $NFS_SUBVERSION $VOL_SUBVERSION
fi  

