aurawin_console="AuConsole";
aurawin_etc="/etc/scs/";
aurawin_logs="/var/log/scs/";
aurawin_install="/usr/bin/AuConsole/";
aurawin_link="/usr/bin/$aurawin_console";

if [ -d $aurawin_logs ]; then
  echo "Deleting $aurawin_logs"
  sudo rm -rf $aurawin_logs
fi

if [ -d $aurawin_install ]; then
  echo "Deleting $aurawin_install"
  sudo rm -rf $aurawin_install
fi

if [ -d $aurawin_etc ]; then
  echo "Deleting $aurawin_etc"  
  sudo rm -rf $aurawin_etc
fi

if [ -f aurawin_link ]; then
  echo "Deleting $aurawin_link"
  sudo rm -f $aurawin_link
fi

echo "Aurawin Social Computing Server Console was removed"

