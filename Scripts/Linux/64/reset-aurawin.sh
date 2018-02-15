sudo service socialserver stop
svn update ./Source
./build-aurawin.sh
sudo ./clear-logs.sh
sudo service socialserver start
