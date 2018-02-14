*********************************************************************
Aurawin Social Computing Server  (Aurawin SCS)
Version 2013 compiled for Linux
Email: support@aurawin.com
Suggested Aurawin Networks: Aurawin SCS, Aurawin VDM.
*********************************************************************
WARNING: 
    While you can compile Aurawin Server on a 32bit platform, you must 
    not run Aurawin Server in a 32bit environment.  Identifiers used for
    social computing methods require a 64bit computing environment.  
*********************************************************************
MANIFEST (AuSCS.zip):
   
   aurawin    - shell script to start/stop the RSR Engine.
   AuService  - service application that controls the RSR Engine.
   AuProcess  - process application for the RSR Engine.
   Readme.txt - this file. 
   install.sh - shell script for installation.
   remove.sh  - shell script to removal. 
*********************************************************************
PREQUISITES:

1.) Aurawin requires that your system have a user and group named "aurawin".
    The install script will create those defaults for you and will use ID 79.
    If your system is using id 79 for either a group or user you will neeed
    edit the install.sh script.
    Notice:
      Aurawin requires at least one disk device.  The variable "raid_group_name"
      located in the install.sh script is defaulted to "raid".

2.) Aurawin install script will create a folder "/etc/scs".
    If you need to change that path edit the install.sh and aurawin scripts.

2.) By default, Aurawin install script will copy the files over to "/usr/bin/scs".
    If you need to change that path edit the install.sh and aurawin scripts.

3.) Aurawin install script will create /var/log/scs folder for
    server logging. If you need to change that path edit the install.sh script.

4.) Aurawin requires a database managment system.  Presently, only three types
    are supported:  MySQL 5.1, Postgresql, and Oracle.

    NOTICE: You must install BOTH client and developer library packages or you
            will get an error when trying to connect to your DBMS.

5.) Aurawin requires OpenSSL.  
        This enables the socket security for many of the implemented protocols.  
	Some protocol services like HTTP, SMTP, POP3 are available to SSL.
        Domain certificates, requests, and management also require the OpenSSL.

6.) ImageMagick is required.
	This enables very fast image manipulation for social computing methods
	that require image data and conversion.

7.) At least one AuraDisk is required.
	AuraDisks automatically mount using NFS. 
	NOTICE: Having Aurdisks on remote nodes will require the use of tunneling
		to inter-connect nodes.  Please ensure propper Firewall config.


*********************************************************************
GETTING STARTED:

1.) To install Aurawin, extract the contents of this zip file to a working folder
2.) Review ./install.sh and adjust paths, group id or user id as mentioned above.
3.) Type sudo ./install.sh
4.) Use AuConsole to configure server instance(s).  
    NOTICE: AuConsole is offered as a seperate download and is the
            official administration tool for Aurawin SCS.
5.) Use at least one Node as an AuraDisk as per the requirement.


*********************************************************************
NODAL CONFIGURATION (scs.ini):

The installation folder (default /etc/scs/) contains an scs.ini file.
Inside this file contains everything that is needed to allow Aurawin SCS
to communicate with the DBMS.  While, only essential information is stored,
AuConsole, AuService, and AuProcess executables all need access to this file.

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
network.  As long as the have a mount point under Linux, Aurawin will
ensure propper usage.

AuraDisks store all forms of personal and social content.  Emails, 
Texts, Blogs, Pictures, Movies, and Documents are just a few purposes.

Requirements: 
     1.) There must be a /AuDisk file located at the root of the file
         system of each device you want to be included.
     2.) Make sure that the user and group of the AuDisk file is "aurawin".

Warnings:
     * If the system goes down or the disk is un-reachable, the system
       should detect such a failure but all users having storage allocation
       there will not be able to access any content.

Tips
     * You can backup/restore raw data from the "/" folder of the actual device.
     * Or you can backup from any node with the following Directory Key
       All information is stored with identifiers generated and maintained by the
       database system. 

       /AuDisks/ClusterID/ResourceID/NodeID/Kind/DomainID/UserID/FolderID/FileID

       /AuDisks - Pivot folder for mounting
       /ClusterID - Actual ID of the desired cluster (of AuraDisk).
       /ResourceID - Actual ID of the desired resource (of AuraDisk)
       /NodeID - Actual ID of the desired node (of AuraDisk)
       /Kind -  Values include: 0=User, 1=Social, 2=CoreData, 3=AppData
       /DomainID - Actual ID of Domain.
       /UserID - Actual ID of User.
       /FolderID - Actual ID of Folder.
       /FileID - Actual ID of File.

     * It is not advisible to access these files other for purposes other than 
       system administration.

     * When writing back-end core objects, please see dbmAuraDisks.pas


*********************************************************************
OPTIONAL FIREWALL CONFIGURATION FOR UFW:

You must allow an open port for each service you intend Aurawin to host.
Without opening each service specific port, users will be unable to connect 
with corresponding Aurawin service.

If you have UFW, opening up your POP3 mail port 110 is accomplished by:
sudo ufw allow from any to 123.123.123.123 port 110

