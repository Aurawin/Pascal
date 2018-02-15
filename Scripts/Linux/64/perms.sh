echo "Setting file permissions..."
sudo chgrp -Rf developer /Developer
sudo chown -Rf atbrunner /Developer
sudo chmod -Rf ug+rw /Developer
#sudo find /Developer -type f -print -exec chmod ug+rw {} \;
#sudo find /Developer -type d -print -exec chmod ug+rwx {} \;
#sudo find "/Developer" -type d -print0 | xargs -0 -I {} chmod ug+rwx {}
echo "File permissions set."
