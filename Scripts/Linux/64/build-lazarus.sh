#!/bin/sh

# build.sh

if [ ! -d /Developer/Source/ ]; then
  echo Making Source Code Folder...
  mkdir /Developer/Source
fi
echo Setting File Permissions...
sudo chown -Rf root:developer /Developer/Lazarus
sudo chmod -Rf ug+rw /Developer/Lazarus
echo Compiling Lazarus
cd /Developer/Lazarus
# make clean all DEBUG=1
sudo make clean all
sudo chown -Rf root:developer /Developer/Lazarus
sudo chmod -Rf ug+rw /Developer/Lazarus
echo Done.
