@echo off
cls
SET SRC_BASE=C:\Developer\Internal\Pascal
SET SRC_VERSION=%SRC_BASE%\Build\AppBuild\Windows\64\AppBuild.inc

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
SET APP=AuCloud
SET FILE=%APP%-%VERSION%%SUFFIX%
SET SRC_EXE=%APP%.exe
SET SRC_MFST=Manifest.xml
SET SRC_RME=Readme.txt
SET SRC_LIC=License.txt
SET SRC_ICON=%APP%.ico
SET SRC_IMAGE=%APP%.jpg

SET SRC_BIN=%SRC_BASE%\Build\Aurawin\%APP%\Windows\64\%SRC_EXE%
SET SRC_LIC=%$SRC_BASE%\Install\Aurawin\%APP%\%SRC_LIC%
SET PTH_UL=%$SRC_BASE%\Install\Aurawin\Uploads


echo %SRC_LIC%
SET SRC_PATH=%SRC_BASE%\Install\Aurawin\%APP%\Windows\64
SET SRC_PACK=%SRC_BASE%\Build\Aurawin\AuPackage\Windows\64
SET SRC_BUILD_BASE=%SRC_BASE%\Build\Aurawin\%APP%
SET SRC_BUILD=%SRC_BUILD_BASE%\Windows\64
echo Setting directory to %SRC_PATH%
cd %SRC_PATH%
echo Building %FILE%
echo Deleting %SRC_PATH%\%SRC_EXE%
del /F /Q %SRC_PATH%\%SRC_EXE%
echo Copying %SRC_BIN%
copy /Y %SRC_BIN% %SRC_PATH%\
echo Adding Files to %FILE%
%SRC_PACK%\AuPack.exe -P=%SRC_PATH%\%FILE% -A=%SRC_PATH%\%SRC_EXE% -A=%SRC_PATH%\%SRC_MFST% -A=%SRC_PATH%\%SRC_RME% -A=%SRC_LIC% -A=%SRC_BUILD_BASE%\%SRC_ICON% -A=%SRC_BUILD_BASE%\%SRC_IMAGE%

echo Assembling uploads...
echo Deleting %PTH_UL%\%APP%-*%SUFFIX%
del /F /Q %PTH_UL%\%APP%-*%SUFFIX%
echo Copying %FILE% %PTH_UL%\%FILE%
copy /Y %FILE% %PTH_UL%\%FILE%
echo Svn Adding %PTH_UL%\%FILE%

echo Build %FILE% successful.