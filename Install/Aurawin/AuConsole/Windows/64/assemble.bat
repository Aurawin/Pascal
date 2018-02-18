@echo off
cls
SET SRC_VERSION=C:\Developer\Internal\Pascal\Build\AppBuild\Windows\64\AppBuild.inc

setlocal EnableDelayedExpansion
set /a counter=0

for /f ^"usebackq^ eol^=^

^ delims^=^" %%a in (%SRC_VERSION%) do (
        if "!counter!"=="1" goto :stop_counting
	SET VERSION=%%a
	SET VERSION=!VERSION:~1,-1!
        set /a counter+=1
)
:stop_counting

SET SUFFIX=-windows-x64.exe
SET APP=AuConsole
SET FILE=%APP%-%VERSION%%SUFFIX%

echo Building %FILE%

SET SRC_EXE=%APP%.exe
SET SRC_MFST=Manifest.xml
SET SRC_RME=Readme.txt
SET SRC_LIC=License.txt
SET SRC_ICON=%APP%.ico
SET SRC_IMAGE=%APP%.jpg
SET FILE=%APP%-%VERSION%-windows-x64.exe
SET SRC_BIN=C:\Developer\Internal\Pascal\Builds\Aurawin\%APP%\Windows\64\%SRC_EXE%
SET SRC_BASE=C:\Developer\Internal\Pascal\Install\Aurawin\%APP%
SET SRC_LIC=%SRC_BASE%\%SRC_LIC%

SET SRC_PATH=C:\Developer\Internal\Pascal\Install\Aurawin\%APP%\Windows\64
SET SRC_PACK=C:\Developer\Internal\Pascal\Build\Aurawin\AuPackage\Windows\64
SET SRC_BUILD_BASE=C:\Developer\Internal\Pascal\Build\Aurawin\%APP%
SET SRC_BUILD=%SRC_BUILD_BASE%\Windows\64
SET PTH_UL=C:\Developer\Internal\Pascal\Install\Aurawin\Uploads

cd SRC_PATH

echo Deleting %SRC_PATH%\%SRC_EXE%
del /F /Q %SRC_PATH%\%SRC_EXE%

echo Copying %SRC_BIN%
copy /Y %SRC_BIN% %SRC_PATH%\

echo Adding Files to %FILE%
%SRC_PACK%\AuPack.exe -P=%SRC_PATH%\%FILE% -A=%SRC_PATH%\%SRC_EXE% -A=%SRC_PATH%\%SRC_MFST% -A=%SRC_PATH%\%SRC_RME% -A=%SRC_LIC% -A=%SRC_BUILD_BASE%\%SRC_ICON% -A=%SRC_BUILD_BASE%\%SRC_IMAGE%
echo %FILE% is now assembled.


echo Assembling uploads...
del /F /Q %PTH_UL%\%APP%-*%SUFFIX%
copy /Y %FILE% %PTH_UL%\%FILE%

echo Build %FILE% successful