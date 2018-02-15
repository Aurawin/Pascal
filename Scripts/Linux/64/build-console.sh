OS_NAME=$(uname)
if [ $OS_NAME = "Linux" ]; then
  /Developer/Internal/Pascal/Install/Aurawin/AuConsole/Linux/64/build.sh
  /Developer/Internal/Pascal/Install/Aurawin/AuConsole/Linux/64/assemble.sh
elif [ $OS_NAME = "Darwin" ]; then
  /Developer/Lazarus/lazbuild -B /Developer/Internal/Pascal/Build/Aurawin/SCS/Apple/32/AuConsole.lpr
fi
