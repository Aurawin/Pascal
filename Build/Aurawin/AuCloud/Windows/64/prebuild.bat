SET BUILD_SRC = C:\Developer\Internal\Pascal\Build\AppBuild\Windows
echo Updating Application Build Stamp
%BUILD_SRC%\64\AppBuild.exe
echo Updating RSR Build Stamp
%BUILD_SRC\64\RSRBuilder.exe