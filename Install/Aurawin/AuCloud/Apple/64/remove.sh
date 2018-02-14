App="AuCloud";
target="/usr/local/$App";
user="/usr/bin";
base="./";
link="$user/$App";
User=$(id -un);
data="/home/$User/.config/$App";

DESTINATION=$target;
echo "Removing $DESTINATION";
sudo rm -Rf $DESTINATION;

DESTINATION=$data;
echo "Removing $DESTINATION";
sudo rm -Rf $DESTINATION;

DESTINATION=$link;
echo "Removing $DESTINATION";
sudo rm $DESTINATION;

echo "Aurawin Cloud synchronization was removed";
