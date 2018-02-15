#!/bin/sh

# unmount.sh
# Copyright 2010-2012 Aurawin LLC. All rights reserved.

NFS_SUBVERSION="phoenix:/media/raid/Developer/Subversion/Aurawin";
VOL_SUBVERSION="/Volumes/Subversion/";

if [ -d $VOL_SUBVERSION ]; then
  echo "Unmounting Aurawin Subversion volume..."
  sudo umount $NFS_SUBVERSION
fi
