App="AuSCS";
aurawin_script="aurawin";
aurawin_process="AuProcess";
aurawin_service="AuService";

aurawin_base="./";
aurawin_etc="/etc/scs/";
aurawin_initd="/etc/init.d/";
aurawin_logs="/var/log/scs/";
aurawin_log=$aurawin_logs"rsr.log";
target="/usr/local/AuSCS/";
aurawin_ini=$aurawin_etc"scs.ini";

echo "Shutting down Aurawin Social Computing Server";
sudo service aurawin stop

if [ -d $aurawin_logs ]; then
  echo "Deleting $aurawin_logs"
  sudo rm -rf $aurawin_logs
fi

if [ -d $target ]; then
  echo "Deleting $target";
  sudo rm -rf $target
fi

if [ -d $aurawin_etc ]; then
  echo "Deleting $aurawin_etc";
  sudo rm -rf $aurawin_etc
fi
sudo rm $aurawin_initd$aurawin_script

echo "Aurawin Social Computing Server was removed from this server";

