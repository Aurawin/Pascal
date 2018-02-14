*********************************************************************
Aurawin Social Computing Server Console (Aurawin SCS)
Version 2014 compiled for Windows 32
Email: support@aurawin.com
Suggested Aurawin Networks: Aurawin SCS, Aurawin VDM.
*********************************************************************
WARNING: 
    While you can compile Aurawin Server on a 32bit platform, you must 
    not run Aurawin Server in a 32bit environment.  Identifiers used for
    social computing methods require a 64bit computing environment.  
   
WINDOWS XP CAUTIONARY NOTE:
    Windows XP is not officially supported.  AuraDisks require that each
    networknode have access *ALL* nodes which host AuraDisks.  Since 
    XP does not support linking network drives to folders, AuraDisks
    cannot be maintained.

    While it is possible to mount 1 physical device to a folder, you will
    not be able to run Aurawin in a clustered environment.

    Windows 7+ includes the ability to mount and maintain such mounts.

   
*********************************************************************
MANIFEST:
   
   AuConsole.exe - process application for the RSR Engine.
   Readme.txt    - this file. 
   License.txt   - the official license agreement. 
   remove.bat    - shell script for removing SCS.

*********************************************************************
PREQUISITES:

1.) The necessary downloads listed in this section are available from
    our Downloads Application.  Or by visiting 
    https://aurawin.com/downloads

2.) Aurawin requires a database managment system.  Presently, only
    three types are supported:  MySQL 5.1, Postgresql, and Oracle.

    NOTICE: You must install BOTH client and developer library packages
            or you will get an error when trying to connect to your DBMS.

    Postgresql:
        Copy all dlls (*.dll) from C:\Program Files\PostgreSQL\9.2\bin to
        C:\Windows\System32

3.) Aurawin requires OpenSSL.  
    This enables the socket security for many of the implemented 
    protocols.  Some protocol services like HTTP, SMTP, POP3 are
    available to SSL. Domain certificates, requests, and management
    also require the OpenSSL.

    The path variable must be set to enable use of all OpenSSL features.
    After installing, add C:\OpenSSL-Win64\bin to your path.

    To edit your path:
      1.)  Right Click My Computer icon.
      2.)  Select Properties.
      3.)  Select Advancted Tab.
      4.)  Click Environment Variables button.
      5.)  Select the "Path" variable in System Variables list.
      6.)  Click the Edit button.
      7.)  Add the path of your Open SSL bin path to the path variable.
           i.e.) ";C:\OpenSLL-Win64\bin"
      8.)  Click Ok.
      9.)  Click Ok.
      10.) Click Ok.
      11.) Log-out of Windows and Log back in.
	

4.) At least one AuraDisk is required.
    AuraDisks automatically mount using NFS (unix). Under Windows, 
    however, you must make sure you MAP network drives manually.
    See AURADISKS below.

    NOTICE:  Having Aurdisks on remote nodes will require the use of 
      tunneling to inter-connect nodes.  Please ensure propper 
      Firewall config.


*********************************************************************
GETTING STARTED:
1.) Use AuConsole to configure server instance(s).  
2.) Use at least one Node as an AuraDisk as per the requirement.


*********************************************************************
NODAL CONFIGURATION (scs.ini):

The installation folder (target C:\Program Files\Aurawin\AuConsole) contains
an scs.ini file.  Inside this file contains everything that is needed 
to allow Aurawin SCS to communicate with the DBMS.  While, only essential
information is stored, AuConsole, AuService, and AuProcess executables
all need access to this file.

After configuring Aurawin, the scs.ini file should be placed in the 
.\AuSCS folder.  Each node's .\AuSCS\ folder should contain a scs.ini
file with matching cluder/resource/node values.

See Also. Clustering.


*********************************************************************
CLUSTERING:

Aurawin SCS supports clusting of computer nodes into resources and 
resources into clusters.  

	Clusters - groups of resources located at a single locale.
	Resources - physical computers with at least 1 MAC Address.
	Node	- represention of each MAC Address by associated IPv4.

Use AuConsole create Clusters, then add each Resource, finally each node.

Typical Example:

Cluster Site1
	Resource BladeBox1
		Node Blade1_LAN 192.168.0.1
		Node Blade1_WAN 123.45.67.1
		Node Blade2_LAN 192.168.0.2
		Node Blade2_WAN 123.45.67.2
		Node Blade3_LAN 192.168.0.3
		Node Blade3_WAN 123.45.67.3
	Resource BladeBox2
		Node Blade1_LAN 192.168.0.4
		Node Blade1_WAN 123.45.67.4
		Node Blade2_LAN 192.168.0.5
		Node Blade2_WAN 123.45.67.5
		Node Blade3_LAN 192.168.0.6
		Node Blade3_WAN 123.45.67.6

Cluster Site2
	Resource BladeBox1
		Node Blade1_LAN 192.168.1.1
		Node Blade1_WAN 123.45.68.1
		Node Blade2_LAN 192.168.1.2
		Node Blade2_WAN 123.45.68.2
		Node Blade3_LAN 192.168.1.3
		Node Blade3_WAN 123.45.68.3

*********************************************************************
AURADISKS:

AuraDisks are typically raid devices connected to the LAN side of your
network.  As long as they have a mount point under Linux, Aurawin will
ensure propper usage.  Under Windows however, NFS is not widely adopted
and the dos commands may differ.  You should just ensure that the root
folder contains the AuDisks folder.  Ie.) c:\AuDisks.  

AuraDisks store all forms of personal and social content.  Emails, 
Texts, Blogs, Pictures, Movies, and Documents are just a few purposes.

Requirements: 
     1.) There must be a AuDisk file located at the root of the file
         system of each device you want to be included.

     2.) Since Unix file system commands e.g.) mount is not available
         under Windows, you must MAP the network drive or mount the
         actual disk to the AuraDisks folder.

         Using the Tips below you can easily assertain a mount point
         once you create a Cluster/Resource/Node identifiers with a 
         corresponding AuraDisk with (native) path.

         Example:
            Cluster Site2(12), Resource BladeBox1(23), and Node Blade1_LAN(45) with
            and IP of 192.168.1.1 now has a raid drive d:\
	    
            Share that drive and map the network drive to each Node's
            C:\AuraDisks.
            
	    While not every node will be an AuraDisk, you can add as many
            as you need one AuraDisk per node.
         
	    Ensure network drive mapping on each node 
            \\192.168.1.1\D C:\AuraDisks\12\23\45

Warnings:
     * If the system goes down or the disk is un-reachable, the system
       should detect such a failure but all users having storage allocation
       there will not be able to access any content.

Tips
     * You can backup/restore raw data from the "\" folder of the actual device.
     * Or you can backup from any node with the following Directory Key
       All information is stored with identifiers generated and maintained by the
       database system. 

       C:\AuDisks\ClusterID\ResourceID\NodeID\Kind\DomainID\UserID\FolderID\FileID

       C:\AuDisks - Pivot folder for mounting
       \ClusterID - Actual ID of the desired cluster (of AuraDisk).
       \ResourceID - Actual ID of the desired resource (of AuraDisk)
       \NodeID - Actual ID of the desired node (of AuraDisk)
       \Kind -  Values include: 0=User, 1=Social, 2=CoreData, 3=AppData
       \DomainID - Actual ID of Domain.
       \UserID - Actual ID of User.
       \FolderID - Actual ID of Folder.
       \FileID - Actual ID of File.

     * It is not advisible to access these files other for purposes other than 
       system administration.

     * When writing back-end core objects, please see dbmAuraDisks.pas


*********************************************************************
OPTIONAL FIREWALL CONFIGURATION:
Please remember to check your ports before troubleshooting first time connection
problems.

You must allow an open port for each service you intend Aurawin to host.
Without opening each service specific port, users will be unable to connect 
with corresponding Aurawin service.
