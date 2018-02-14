App="AuCloud";
License="License.txt";
Readme="Readme.txt";
remove="remove.sh";
icon="$App.png";
desktop="$App.desktop";
Share="/usr/share";
DesktopApps="$Share/applications/";
DesktopIcons="$Share/icons";

target="/usr/local/$App";
user="/usr/bin";
base="./"
link="$user/$App";
Runable="$target/$App";
Removal="$target/$remove";

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
DESTINATION="$Removal";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$App";
DESTINATION="$target/$App";

echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$desktop";
DESTINATION="$DesktopApps";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

SOURCE="$base$icon";
DESTINATION="$DesktopIcons";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION


echo "Setting permissions"
sudo chmod a+rx $Runable;
sudo chmod a+rx $Removal;

SOURCE=$Runable;
DESTINATION=$link;
echo "Creating a link from $SOURCE to $DESTINATION"
sudo ln -s -f $SOURCE $DESTINATION

