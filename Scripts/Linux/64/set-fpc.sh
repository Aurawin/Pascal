#!/bin/sh

# set-fpc.sh
# 
#
FPC_BUILD="3.1.1";
OS_NAME=$(uname);
if [ $OS_NAME = Darwin ]; then
  TARGET="/usr/local";
  FPC_CMP="ppc386";
else
  TARGET="/usr/local";
  FPC_CMP="ppcx64";
fi
FPC_DIR=$TARGET"/lib/fpc/"$FPC_BUILD;
FPC_CMP=$FPC_DIR"/$FPC_CMP";
FPC_CONFIG="fpcmkcfg";
FPC_UTILS="/Developer/FPC/compiler/utils/";
FPC_BIN="fpc";
FPC_EXE="$FPC_UTILS$FPC_BIN";

CFG_OUT="/etc"
LN_FPC_32_TARGET1="/usr/ppc386";
LN_FPC_32_TARGET2="/usr/bin/ppc386";

LN_FPC_64_TARGET1="/usr/bin/ppcx64";
LN_FPC_64_TARGET2="/usr/local/bin/ppcx64";

CP_FPC_BIN="/usr/local/bin/$FPC_BIN";

echo "linking $FPC_CMP $LN_FPC_64_TARGET1";
sudo ln -sf $FPC_CMP $LN_FPC_64_TARGET1;

echo "linking $FPC_CMP $LN_FPC_64_TARGET2";
sudo ln -sf $FPC_CMP $LN_FPC_64_TARGET2;


# echo "copying $FPC_EXE $CP_FPC_BIN";
# sudo cp -f $FPC_EXE $CP_FPC_BIN;

echo sample config $FPC_CONFIG $FPC_DIR /etc
sudo $FPC_CONFIG -d basepath=$FPC_DIR -o /etc/fpc.cfg
echo sample config $FPC_CONFIG $FPC_DIR $FPC_DIR
sudo $FPC_CONFIG -d basepath=$FPC_DIR -o $FPC_DIR/fpc.cfg

sudo chown -Rf root:developer $TARGET/lib/fpc
sudo chmod -Rf ug+rw $TARGET/lib/fpc
echo Done.

