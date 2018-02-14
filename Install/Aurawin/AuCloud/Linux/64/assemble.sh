App="AuCloud";
Version=$(sed -n 1p /Developer/Source/Builds/AppBuild/Linux/64/AppBuild.inc);
Version="${Version%\'}";
Version="${Version#\'}";
echo "Assembling $App-$Version ...";
Opts="-9 -j";
Root="/Developer/Source/Install/Aurawin/$App";
Base="$Root/Linux/64";
Suffix="linux-x64.zip";
ZipFile="$App-$Version-$Suffix";
Zip="$Base/$ZipFile";
Build_Base="/Developer/Source/Builds/Aurawin/$App";
Build="$Build_Base/Linux/64";
PTH_UL="/Developer/Source/Install/Aurawin/Uploads";
target="$Base/$App";
Readme="$Base/Readme.txt";
License="$Root/License.txt";
Install="$Base/install.sh";
Remove="$Base/remove.sh";
Icon="$Build_Base/$App.png";
DesktopInf="$Build/$App.desktop";

cp -f $Build/$App $target
zip $Opts $Zip $target
zip $Opts $Zip $Readme
zip $Opts $Zip $License
zip $Opts $Zip $Install
zip $Opts $Zip $Remove
zip $Opts $Zip $Icon
zip $Opts $Zip $DesktopInf
svn -q --force add $Zip

echo "Assembling Uploads...";

rm -f $PTH_UL/$App-*$Suffix;
cp -f $Zip $PTH_UL/$ZipFile;
svn -q --force add $PTH_UL/$ZipFile;

svn commit $Base -m "$App-$Version"
svn commit $PTH_UL -m "$App-$Version"

echo "Assembly of $App-$Version compelete!";
