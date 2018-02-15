#!/bin/sh

# build-fpc.sh
# 
OS_NAME=$(uname)
DEV="/Developer/"
DEV_FPC=$DEV"FPC/"
DEV_SOURCE=$DEV"Source/"
FPC_VERS="3.1.1"
FPC_START_VER="2.6.4";
CPU_64="x86_64";

PP_64="ppcx64";
SRC_64="x86_64";

OP_SYS="linux";
TARGET="/usr/local"
FPC_DIR=$TARGET"/lib/fpc/"$FPC_VERS
START_CMP="/usr/local/lib/fpc/$FPC_START_VER/$PP_64";

BUILD_STAMP_64=$DEV_FPC"build-stamp."$CPU_64"-"$OP_SYS;

echo "Going to inspect 64bit build for "$OP_SYS" "$BUILD_STAMP_64;

if [ ! -d $DEV_SOURCE ]; then
  echo Making Source Code Folder...
  mkdir $DEV_SOURCE
fi
echo Setting File Permissions...
sudo chgrp -Rf developer $DEV_FPC
sudo chmod -Rf ug+rw $DEV_FPC

if [ ! -f $BUILD_STAMP_64 ]; then
  cd $DEV
  echo Resetting to Default System Free Pascal Compiler...
  ./reset-fpc.sh
  echo Compiling 64bit Free Pascal Compiler...
  cd $DEV"FPC"
  make clean all OPT="-O-" DEBUG=1 OS_TARGET=$OP_SYS CPU_TARGET=$CPU_64 CPU_SOURCE=$SRC_64
fi

if [ -f $BUILD_STAMP_64 ]; then
  cd $DEV_FPC
  sudo make install PREFIX=$TARGET CPU_TARGET=$CPU_64 CPU_SOURCE=$SRC_64
  sudo make sourceinstall PREFIX=$TARGET FPC=$FPC_CMP CPU_TARGET=$CPU_64 CPU_SOURCE=$SRC_64
  echo Done.
fi
