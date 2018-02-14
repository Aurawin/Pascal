@echo off
cd C:\Developer\Source\Install\Aurawin\AuSCS\Windows\64
cls
SET SRC_VERSION=C:\Developer\Source\Builds\AppBuild\Windows\64\AppBuild.inc

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
SET APP=AuSCS
SET FILE=%APP%-%VERSION%%SUFFIX%
echo Building %FILE%

SET SRC_EXE_1=AuProcess.exe
SET SRC_EXE_2=AuService.exe
SET SRC_MFST=Manifest.xml
SET SRC_RME=Readme.txt
SET SRC_LIC=License.txt
SET SRC_ICON=AuSCS.ico
SET SRC_IMAGE=AuSCS.jpg
SET SRC_BIN_1=C:\Developer\Source\Builds\Aurawin\AuSCS\Windows\64\%SRC_EXE_1%
SET SRC_BIN_2=C:\Developer\Source\Builds\Aurawin\AuSCS\Windows\64\%SRC_EXE_2%
SET SRC_LIC=C:\Developer\Source\Install\Aurawin\AuSCS\%SRC_LIC%

SET SRC_MAGICK=C:\Developer\Source\Install\External\Windows\64\ImageMagick
SET SRC_PATH=C:\Developer\Source\Install\Aurawin\AuSCS\Windows\64
SET SRC_PACK=C:\Developer\Source\Builds\Aurawin\AuPackage\Windows\64
SET SRC_BUILD_BASE=C:\Developer\Source\Builds\Aurawin\AuSCS
SET SRC_BUILD=%SRC_BUILD_BASE%\Windows\64
SET PTH_UL=C:\Developer\Source\Install\Aurawin\Uploads

cd SRC_PATH

echo Deleting %SRC_PATH%\%SRC_EXE_1%
del /F /Q %SRC_PATH%\%SRC_EXE_1%
echo Deleting %SRC_PATH%\%SRC_EXE_2%
del /F /Q %SRC_PATH%\%SRC_EXE_2%

echo Copying %SRC_BIN_1%
copy /Y %SRC_BIN_1% %SRC_PATH%\
echo Copying %SRC_BIN_2%
copy /Y %SRC_BIN_2% %SRC_PATH%\

echo Adding Files to %FILE%
%SRC_PACK%\AuPack.exe -P=%SRC_PATH%\%FILE% -A=%SRC_PATH%\%SRC_EXE_1% -A=%SRC_PATH%\%SRC_EXE_2% -A=%SRC_PATH%\%SRC_MFST% -A=%SRC_PATH%\%SRC_RME% -A=%SRC_LIC% -A=%SRC_BUILD_BASE%\%SRC_ICON% -A=%SRC_BUILD_BASE%\%SRC_IMAGE% -A=%SRC_MAGICK%\CORE_RL_bzlib_.dll -A=%SRC_MAGICK%\CORE_RL_jbig_.dll -A=%SRC_MAGICK%\CORE_RL_jp2_.dll -A=%SRC_MAGICK%\CORE_RL_jpeg_.dll -A=%SRC_MAGICK%\CORE_RL_lcms_.dll -A=%SRC_MAGICK%\CORE_RL_libxml_.dll -A=%SRC_MAGICK%\CORE_RL_magick_.dll -A=%SRC_MAGICK%\CORE_RL_png_.dll -A=%SRC_MAGICK%\CORE_RL_tiff_.dll -A=%SRC_MAGICK%\CORE_RL_ttf_.dll -A=%SRC_MAGICK%\CORE_RL_wand_.dll -A=%SRC_MAGICK%\CORE_RL_xlib_.dll -A=%SRC_MAGICK%\CORE_RL_zlib_.dll
svn add -q --force %SRC_PATH%\%FILE%
echo %FILE% is complete.

echo Assembling uploads...
del /F /Q %PTH_UL%\%APP%-*%SUFFIX%
copy /Y %FILE% %PTH_UL%\%FILE%
svn add -q --force %PTH_UL%\%FILE%


svn commit -m "Added %FILE%"
svn commit -m "Added %FILE%" %PTH_UL%


echo Build %FILE% successful