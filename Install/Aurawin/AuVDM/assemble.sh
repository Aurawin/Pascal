App="AuVDM";
OldDir=$(pwd);
Version=$(sed -n 1p /Developer/Source/Builds/AppBuild/Linux/64/AppBuild.inc);
Version="${Version%\'}";
Version="${Version#\'}";
echo "Assembling $App-$Version ...";
Opts="-9 -j";
FolderOpts="-9 -r";
Suffix=".zip";
SVN_DIR="/Developer/Internet/Aurawin/portal/";
IN_DIR="/Developer/Source/Builds/Aurawin/$App";
OUT_DIR="/Developer/Source/Install/Aurawin/$App";
PTH_UL="/Developer/Source/Install/Aurawin/Uploads";
rm -Rf $IN_DIR;
svn export $SVN_DIR $IN_DIR;

rm -Rf $IN_DIR/http/aup;
rm -Rf $IN_DIR/http/browser;
rm -Rf $IN_DIR/http/cloud;
rm -Rf $IN_DIR/http/dev;
rm -Rf $IN_DIR/http/download;
rm -Rf $IN_DIR/http/downloads;
rm -Rf $IN_DIR/http/features;
rm -Rf $IN_DIR/http/fp;
rm -Rf $IN_DIR/http/landing;
rm -Rf $IN_DIR/http/lazarus;
rm -Rf $IN_DIR/http/pricing;
rm -Rf $IN_DIR/http/privacy;
rm -Rf $IN_DIR/http/search;
rm -Rf $IN_DIR/http/social;
rm -Rf $IN_DIR/http/tos;


Base="/Developer/Source/Install/Aurawin/$App";
ZipFile="$App-$Version$Suffix";
Zip="$Base/$ZipFile";

rm -f $Zip;

Readme="$Base/Readme.txt";
License="$Base/License.txt";
Keywords="$IN_DIR/Keywords.kwz";
Purchases="$IN_DIR/Purchases.plz";


zip $Opts $Zip $Purchases
zip $Opts $Zip $Keywords
zip $Opts $Zip $Readme
zip $Opts $Zip $License
cd "$IN_DIR"
zip $FolderOpts $Zip "./core"
zip $FolderOpts $Zip "./http"

svn -q --force add $Zip

echo "Assembling Uploads...";

rm -f $PTH_UL/$App-*$Suffix;
cp -f $Zip $PTH_UL;
svn -q --force add $PTH_UL/$ZipFile;

svn commit $Base -m "$App-$Version"
svn commit $PTH_UL -m "$App-$Version"

echo "Assembly of $App-$Version compelete!";
cd $OldDir
