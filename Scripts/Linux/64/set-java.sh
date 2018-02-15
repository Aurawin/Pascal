jdk="jdk1.8.0_92"
jdk_file="jdk-8u92-linux-x64"
jdk_install="/media/raid/Install/Developer/java/$jdk_file.tar.gz"
jdk_target="/usr/lib/jvm"


android="android-studio"
android_file="android-studio-ide-143.2915827-linux.zip"
android_install=/media/raid/Install/Developer/java/$android_file
android_target="/usr"

idea="idea-IC-145.1617.8"
idea_file="ideaIC-2016.1.3"
idea_install="/media/raid/Install/Developer/java/$idea_file.tar.gz"
idea_target="/usr"


echo "Extracting $jdk ($jdk_install to $jdk_target)"
tar -xzf $jdk_install -C $jdk_target

chown root:root -Rf $jdk_target/$jdk
chmod -R a+xr $jdk_target/$jdk
ln -sf $jdk_target/$jdk $jdk_target/default-java

echo "Exracting $android ($android_install to $android_target)"
unzip -oq $android_install -d $android_target
chmod a+x $android_target/$android/bin/studio.sh

echo "Extracting $idea ($idea_install to $idea_target)"
tar -xzf $idea_install -C $idea_target


echo "Setting Java Virtual Machine Links"

ln -sf /usr/lib/jvm/default-java/bin/java /etc/alternatives/java
ln -sf /usr/lib/jvm/default-java/bin/javac /etc/alternatives/javac
ln -sf /usr/lib/jvm/default-java/bin/javah /etc/alternatives/javah
ln -sf /usr/lib/jvm/default-java/bin/javap /etc/alternatives/javap
ln -sf /usr/lib/jvm/default-java/bin/jar /etc/alternatives/jar
ln -sf /usr/lib/jvm/default-java/bin/jarsigner /etc/alternatives/jarsigner
ln -sf /usr/lib/jvm/default-java/bin/javadoc /etc/alternatives/javadoc
ln -sf /usr/lib/jvm/default-java/lib/jexec /etc/alternatives/jexec
ln -sf /usr/lib/jvm/default-java/bin/jcmd /etc/alternatives/jcmd
ln -sf /usr/lib/jvm/default-java/bin/jdeps /etc/alternatives/jdeps
ln -sf /usr/lib/jvm/default-java/bin/jdb /etc/alternatives/jdb
ln -sf /usr/lib/jvm/default-java/bin/jstat /etc/alternatives/jstat
ln -sf /usr/lib/jvm/default-java/bin/jstack /etc/alternatives/jstack
ln -sf /usr/lib/jvm/default-java/bin/jrunscript /etc/alternatives/jrunscript
ln -sf /usr/lib/jvm/default-java/bin/jps /etc/alternatives/jps
ln -sf /usr/lib/jvm/default-java/bin/jmap /etc/alternatives/jmap
ln -sf /usr/lib/jvm/default-java/bin/jjs /etc/alternatives/jjs
ln -sf /usr/lib/jvm/default-java/bin/jinfo /etc/alternatives/jinfo
ln -sf /usr/lib/jvm/default-java/bin/jhat /etc/alternatives/jhat

echo "Java $jdk installation is complete."
