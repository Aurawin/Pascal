group_id="79"
user_id="79"

raid_group_name="raid";
group_name="aurawin"
user_name="aurawin"

aurawin_console="AuConsole"

App="AuConsole";
License="License.txt";
Readme="Readme.txt";
remove="remove.sh";

icon="$App.png";
desktop="$App.desktop";
user="/usr";
share="$user/share";
bin="$user/bin";
base="./";
target="$user/$App";

aurawin_etc="/etc/scs/";
aurawin_logs="/var/log/scs/";
aurawin_log=$aurawin_logs"rsr.log";


aurawin_ini=$aurawin_etc"scs.ini";

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


echo "Inspecting $aurawin_etc"
if [ ! -d $aurawin_etc ]; then
  sudo mkdir $aurawin_etc
fi
sudo chown -Rf $user_name":"$group_name $aurawin_etc
sudo chmod ug+rwx $aurawin_etc

echo "Inspecting $aurawin_ini"
if [ ! -f $aurawin_ini ]; then
  sudo touch $aurawin_ini
fi

echo "Inspecting $target"
if [ ! -d $target ]; then
  sudo mkdir $target
fi

SOURCE="$base$License";
DESTINATION="$target/$License";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$Readme";
DESTINATION="$target/$Readme";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$remove";
DESTINATION="$target/$remove";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$icon";
DESTINATION="$share/icons/";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$desktop";
DESTINATION="$share/applications/";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$App";
DESTINATION="$user/$App/";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$target/$App";
DESTINATION="$bin/$App";
echo "Creating Link to $SOURCE as $DESTINATION";
sudo ln -sf $SOURCE $DESTINATION;

echo "Setting permissions";
sudo chown -Rf $user_name:$user_group $aurawin_ini;
sudo chmod 775 $aurawin_ini;

sudo chown -Rf $user_name:$user_group $target;
sudo chmod -Rf 755 $target;

echo "Aurawin Social Computing Server (SCS) Console was installed";
