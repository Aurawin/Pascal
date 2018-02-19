SET BUILD_SRC=C:\Developer\Internal\Pascal\Build
echo Updating Application Build Stamp
%BUILD_SRC%\AppBuild\Windows\64\AppBuild.exe
echo Updating RSR Build Stamp
%BUILD_SRC%\RSRBuild\Windows\64\RSRBuilder.exe