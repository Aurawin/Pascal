sudo chown root:developer /Developer -Rf
sudo chmod ug+rw /Developer -Rf
cd /Developer/Source/Compilers/FPC

make clean all
cd /Developer/Source/Compilers/Lazarus
rm startlazarus
make clean all
if [ -f /Developer/Source/Compilers/FPC/build-stamp.x86_64-linux ]; then
  echo "FPC compiler test [OK]"
else
  echo "FPC compiler test [FAIL]"
fi
if [ -f /Developer/Source/Compilers/Lazarus/startlazarus ]; then
  echo "Lazarus compiler test [OK]"
else
  echo "Lazarus compiler test [FAIL]"
fi
