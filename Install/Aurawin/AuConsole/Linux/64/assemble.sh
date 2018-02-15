App="AuConsole";
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
target="$Base/$App";
Readme="$Base/Readme.txt";
License="$Root/License.txt";
Install="$Base/install.sh";
Remove="$Base/remove.sh";
Icon="$Base/AuConsole.png";
Desktop="$Base/AuConsole.desktop";

cp -f $Build/$App $target;
zip $Opts $Zip $target;
zip $Opts $Zip $Readme;
zip $Opts $Zip $License;
zip $Opts $Zip $Install;
zip $Opts $Zip $Remove;
zip $Opts $Zip $Icon;
zip $Opts $Zip $Desktop;


echo "Removing file pattern " $PTH_UL/$App"-*"$Suffix;
rm -f $PTH_UL/$App-*$Suffix;
echo "Moving " $ZipFile " to " $PTH_UL;
mv -f $Zip $PTH_UL/$ZipFile;

echo "Assembly of $App-$Version compelete!";

echo "Please check " $PTH_UL " for assembled package."
