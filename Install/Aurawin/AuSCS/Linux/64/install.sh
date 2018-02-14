App="AuSCS";
group_id="79";
user_id="79";

raid_group_name="raid";
group_name="aurawin";
user_name="aurawin";

aurawin_script="aurawin";
aurawin_systemd="aurawin.service";
aurawin_process="AuProcess";
aurawin_service="AuService";
aurawin_license="License.txt";
target="/usr/local/$App";

aurawin_base="./";
aurawin_etc="/etc/scs/";
aurawin_initd="/etc/init.d/";
aurawin_logs="/var/log/scs/";
aurawin_log=$aurawin_logs"rsr.log";
aurawin_ini=$aurawin_etc"scs.ini";
aurawin_system_service="";
if [ -d /etc/systemd/system ] ; then
  aurawin_system_service="/etc/systemd/system/"$aurawin_systemd;
fi;

SCS_GROUP_ID=$(id -g $group_name)

if [ "$SCS_GROUP_ID" = "" ]; then
	echo "Adding group $group_name ($group_id) to system"
	sudo groupadd -r -g $group_id $group_name
	SCS_GROUP_ID=$(id -g $group_name)
	SCS_USER_ID=$(id -u $user_name)
	if [ "${SCS_USER_ID}" = "" ]; then
		echo "Adding user $user_name ($user_id) to group $group_name($group_id)"
		sudo useradd -r -M -G $group_name","$raid_group_name -u $user_id -g $group_name $user_name
		SCS_USER_ID=$(id -u $user_name)
  	fi

else
	SCS_USER_ID=$(id -u $user_name)
	if [ "$SCS_USER_ID" = "" ]; then
		echo "Adding user $user_name ($user_id) to group $group_name($group_id)"
		sudo useradd -r -M -G $group_name","$raid_group_name -u $user_id -g $group_name $user_name
		SCS_USER_ID=$(id -u $user_name)
	fi
fi

echo "Inspecting $aurawin_logs"

if [ ! -d $aurawin_logs ]; then
  echo "Creating log folder $aurawin_logs"
  sudo mkdir $aurawin_logs
fi
sudo chmod ug+rwx $aurawin_logs

if [ ! -f $aurawin_log ]; then
  sudo touch $aurawin_log
fi
sudo chown -Rf $user_name":"$group_name $aurawin_logs
sudo chmod ug+rw $aurawin_log
sudo chmod a-x $aurawin_log


echo "Inspecting $target"
if [ ! -d $target ]; then
  sudo mkdir $target
fi
sudo chmod ug+rwx $target

echo "Inspecting $aurawin_etc"
if [ ! -d $aurawin_etc ]; then
  sudo mkdir $aurawin_etc
fi
sudo chmod ug+rwx $aurawin_etc

echo "Inspecting $aurawin_ini"
if [ ! -f $aurawin_ini ]; then
  sudo touch $aurawin_ini
fi
sudo chown $user_name:$user_group $aurawin_ini
sudo chmod ug+rw $aurawin_ini
sudo chmod a-x $aurawin_ini

SCS_SOURCE="$aurawin_base$aurawin_script"
SCS_DESTINATION="$aurawin_initd$aurawin_script"
echo "Copying $SCS_SOURCE to $SCS_DESTINATION"		
sudo cp -f $SCS_SOURCE $SCS_DESTINATION

SCS_SOURCE="$aurawin_base$aurawin_process";
SCS_DESTINATION="$target/$aurawin_process";
echo "Copying $SCS_SOURCE to $SCS_DESTINATION";
sudo cp -f $SCS_SOURCE $SCS_DESTINATION

SCS_SOURCE="$aurawin_base$aurawin_service"
SCS_DESTINATION="$target/$aurawin_service"
echo "Copying $SCS_SOURCE to $SCS_DESTINATION";
sudo cp -f $SCS_SOURCE $SCS_DESTINATION

SCS_SOURCE="$aurawin_base$aurawin_license"
SCS_DESTINATION="$target/$aurawin_license"
echo "Copying $SCS_SOURCE to $SCS_DESTINATION";
sudo cp -f $SCS_SOURCE $SCS_DESTINATION

if [ $aurawin_system_service != "" ]; then
  SCS_SOURCE=$aurawin_base$aurawin_systemd;
  SCS_DESTINATION=$aurawin_system_service;
  echo "Copying $SCS_SOURCE to $SCS_DESTINATION";
  sudo cp -f $SCS_SOURCE $SCS_DESTINATION
fi;

echo "Setting permissions";
sudo chown -Rf $user_name:$user_group $aurawin_ini
sudo chmod 775 $aurawin_ini
sudo chown -Rf $user_name:$user_group $target
sudo chmod -Rf 755 $target

echo "Aurawin Social Computing Server was installed to this server"
echo "You will need to install and run AuConsole to configure"
