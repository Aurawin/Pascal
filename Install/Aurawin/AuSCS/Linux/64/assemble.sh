App="AuSCS";
Version=$(sed -n 1p /Developer/Internal/Pascal/Build/AppBuild/Linux/64/AppBuild.inc);
Version="${Version%\'}";
Version="${Version#\'}";
echo "Assembling $App-$Version ...";
Opts="-9 -j";
Root="/Developer/Internal/Pascal/Install/Aurawin/$App";
Base="$Root/Linux/64";
Suffix="linux-x64.zip";
ZipFile="$App-$Version-$Suffix";
Zip="$Base/$ZipFile";
Build="/Developer/Internal/Pascal/Build/Aurawin/$App/Linux/64";
PTH_UL="/Developer/Internal/Pascal/Install/Aurawin/Uploads";
AuProcess="$Base/AuProcess";
AuService="$Base/AuService";
aurawin="$Base/aurawin";
systemd="$Base/aurawin.service";
Readme="$Base/Readme.txt";
License="$Root/License.txt";
Install="$Base/install.sh";
Remove="$Base/remove.sh";
cp -f $Build/AuProcess $AuProcess
cp -f $Build/AuService $AuService
zip $Opts $Zip $AuService
zip $Opts $Zip $AuProcess
zip $Opts $Zip $aurawin
zip $Opts $Zip $Readme
zip $Opts $Zip $License
zip $Opts $Zip $Install
zip $Opts $Zip $Remove
zip $Opts $Zip $systemd

echo "Removing file pattern " $PTH_UL/$App"-*"$Suffix;
rm -f $PTH_UL/$App-*$Suffix;

echo "Moving " $ZipFile "to Uploads...";
mv -f $Zip $PTH_UL/$ZipFile;

echo "Assembly of $App-$Version compelete!";
echo "Check Upload Folder "$PTH_UL" for assembly.";
