App="AuCloud";
License="License.txt";
Readme="Readme.txt";
remove="remove.sh";

target="/usr/local/$App";
user="/usr/bin";
base="./"
link="$user/$App";

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

SOURCE="$base$App";
DESTINATION="$target/$App";
echo "Copying $SOURCE to $DESTINATION"
sudo cp -f $SOURCE $DESTINATION

echo "Setting permissions"
sudo chmod -Rf a+rx $target

SOURCE=$DESTINATION;
DESTINATION=$link;

echo "Creating a link from $SOURCE to $DESTINATION"
sudo ln -s -f $SOURCE $DESTINATION

