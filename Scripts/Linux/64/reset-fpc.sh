#!/bin/sh

# reset-fpc.sh
# 
#
FPC_BUILD="3.0.2"
OS_NAME=$(uname)

  TARGET="/usr/local"
  FPC_DIR="/usr/local/lib/fpc/"$FPC_BUILD
  FPC_CMP=$FPC_DIR"/ppcx64"
  LN_FPC_TARGET1="/usr/ppcx64"
  LN_FPC_TARGET2="/usr/bin/ppcx64"
  LN_FPC_TARGET3="/usr/local/bin/ppcx64"
  FPC_CONFIG=$FPC_DIR"/samplecfg"
  CFG_OUT="/etc"

  FPC_BS_PTH="/Developer/Source/Bootstrap/FPC/Linux/64";
  FPC_BS_BIN=$FPC_BS_PTH"/fpc";
  FPC_BS_DST="/usr/local/bin/fpc";


  echo "Restoring $FPC_BS_DST from $FPC_BS_BIN";
  sudo cp -f $FPC_BS_BIN $FPC_BS_DST;

  echo "linking $FPC_CMP $LN_FPC_TARGET1";
  sudo ln -sf $FPC_CMP $LN_FPC_TARGET1

  echo "linking $FPC_CMP $LN_FPC_TARGET2";
  sudo ln -sf $FPC_CMP $LN_FPC_TARGET2

  echo "linking $FPC_CMP $LN_FPC_TARGET3";
  sudo ln -sf $FPC_CMP $LN_FPC_TARGET3



echo sample config $FPC_DIR/samplecfg $FPC_DIR /etc
sudo $FPC_DIR/samplecfg $FPC_DIR /etc

sudo chown -Rf root:developer $TARGET/lib/fpc
sudo chmod -Rf ug+rw $TARGET/lib/fpc
echo Done.

