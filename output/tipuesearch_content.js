var tipuesearch = {"pages":[{"title":"About","text":"Starting this blog, I want to accomplish some goals. The most important goal is to share my knowledge and experience with others. It's my contribution back to the community of technology gurus who helped me be where I am now. I haven't done anything special. I've just learnt information from different sources, validated, refined, added my research, and published back. I appreciate everybody who makes information open and available to public. We are growing together helping each other. The second goal is to create my personal repository as a quick way to refresh my memory in case of forgetting something. In many cases, we are doing a lot of staff but we can't keep it in our heads. When we need it again, we try to reinvent the wheel because we can't recall what it was done before. The last goal is to express myself. This is when I can be creative without any limits. It's my way to go and I'm happy to be successful. If you want to contact me, please drop me a line here . You can find me on git as well.","tags":"pages","url":"/pages/about.html","loc":"/pages/about.html"},{"title":"Enable gzip Compression in Synology NAS in Nginx","text":"Enabling gzip compression for your website can be done in Synology DiskStation Manager (DSM). HTTP traffic is already compressed with gzip as default. HTTPS protocol needs to be enabled explicitly. DSM manager is doing it in an easy step without rebooting Synology NAS. Also, DSM manager includes text/javascript and text/css MIME types additional to text/html one. 1. Validate Compression 1.1 Firefox Browses Open your website in Firefox browser Click Shift-F12 to open Developer Tools panels. Go to Network panel Click Reload button in the panel or Ctrl-F5 buttons to initiate your website data load. Filter resources which you are interested in. Those resources are: HTML , CSS , and JS , for example, HTML website root page. Click on a resource to see compression and other resource properties, for example, HTML resource. Clear cache. Gzip compression is set up. 1.2 Chrome Browser The steps to get information are very similar to Firefox. To open Chrome DevTools, press Command+Option+C (Mac) or Ctrl+Shift+C (Windows, Linux, Chrome OS). 2. Enable gzip Compression in DSM Open Control Panel in Synology DiskStation Manager. Go to Security settings. Select Advanced tab and flag Enable HTTP Compression . Apply the setting pressing Save button. 3. Include Different MIME Types Compression is applied to HTML MIME type as default. Also, compression is applied to CSS and JS in Synology DSM. If compression is missing for CSS and JS resources, it can be set up. The sample is based on DSM 6.2.2 operation system. Use ssh client to access your Synology NAS with a user which has administrative permission. It can be PuTTY tool in Windows or terminal with ssh command in Unix. Switch to root user. sudo su - Back up the current moustache template. cp /usr/syno/share/nginx/WWWService.mustache /usr/syno/share/nginx/WWWService.mustache.bak Open the moustache template for editing. vi /usr/syno/share/nginx/WWWService.mustache Add a line after gzip on; command to both HTTP and HTTPS server sections. gzip_types text/javascript text/css; The final content should be. server { listen 80 default_server{{#reuseport}} reuseport{{/reuseport}}; listen [::]:80 default_server{{#reuseport}} reuseport{{/reuseport}}; gzip on; gzip_types text/javascript text/css; {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / http://$host:{{DSM.port}}/ redirect; } } server { listen 443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; listen [::]:443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; {{#DSM.https.compression}} gzip on; gzip_types text/javascript text/css; {{/DSM.https.compression}} {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / https://$host:{{DSM.ssl.port}}/ redirect; } } Restart the Nginx web server to apply the changes. synoservicecfg --restart nginx","tags":"Synology NAS","url":"/enable-gzip-compression-in-synology-nas-in-nginx.html","loc":"/enable-gzip-compression-in-synology-nas-in-nginx.html"},{"title":"Create keytab File for Kerberos Authentication in Windows","text":"Windows has a limited set of tools to create a keytab file. There are a couple of tools for this purpose. One tool is the Windows Server built-in utility ktpass . It can be only run on a Windows Server. Another tool is ktab which can be used on any Windows computer. ktab tool is a part of Java installation. 1. ktpass There are some restrict requirements to run the tool. It must be run on either a member server or a domain controller of the Active Directory domain. Windows Server operating system such as Windows Server 2008, 2012, or 2016 are supported. When running ktpass.exe, Windows Command Prompt must be run with Run as administrator option. ktpass -princ [ Windows user name ] @ [ Realm name ] -pass [ Password ] -crypto [ Encryption type ] -ptype [ Principle type ] -kvno [ Key version number ] -out [ Keytab file path ] [Windows user name] - mywindowsname. [Real name] - SAMPLE.COM. [Password] - mywindowsname user password. [Encryption type] - RC4-HMAC-NT. See RFC 3961, section 8 . [Principle type] - KRB5_NT_PRINCIPAL which is Kerberos protocol 5. [Key version number] - 0. [Keytab file path] - c:\\kerberos\\keytabname.keytab. 2. ktab It requests to install Java JRE or SDK or open source equivalent, for example, OpenJDK. The tool has a limited set of options. It can't be defined encryption and principle types. It will be used Kerberos protocol 5 and it will be created multiple encryption types. ktab -a [ Windows user name ] @ [ Realm name ] [ Password ] -n [ Key version number ] -k [ Keytab file path ] List all encryption types stored in a keytab file ktab -l -e -k [ Keytab file path ] If multiple encryption types are not accepted in authentication process, it can be left one encryption type and the rest can be deleted. ktab -d [ Windows user name ] @ [ Realm name ] -f -e [ Number of encryption type ] -k [ Keytab file path ] [Number of encryption type] - 16. As per RFC 3961, section 8. 3. Usage Samples 3.1. DBeaver connection to Hive with Kerberos Authentication It can be created multiple encryption types in a keytab file. Create a keytab file ktab -a mywindowsname@SAMPLE.COM mypassword -n 0 -k c: \\k erberos \\m ywindowsname.keytab Done! Service key for mywindowsname@SAMPLE.COM is saved in c: \\k erberos \\m ywindowsname.keytab List content of the keytab file ktab -l -e -k c: \\k erberos \\m ywindowsname.keytab Keytab name: c: \\k erberos \\m ywindowsname.keytab KVNO Principal ---- --------------------------------------------------------------- 0 mywindowsname@SAMPLE.COM ( 18 :AES256 CTS mode with HMAC SHA1-96 ) 0 mywindowsname@SAMPLE.COM ( 17 :AES128 CTS mode with HMAC SHA1-96 ) 0 mywindowsname@SAMPLE.COM ( 16 :DES3 CBC mode with SHA1-KD ) 0 mywindowsname@SAMPLE.COM ( 23 :RC4 with HMAC ) 3.2. Talend tHDFSConnection Component with Kerberos Authentication It should be one encryption type in a keytab file, for example, 23. Create a keytab file ktab -a mywindowsname@SAMPLE.COM mypassword -n 0 -k c: \\k erberos \\m ywindowsname.keytab Done! Service key for mywindowsname@SAMPLE.COM is saved in c: \\k erberos \\m ywindowsname.keytab List content of the keytab file ktab -l -e -k c: \\k erberos \\m ywindowsname.keytab Keytab name: c: \\k erberos \\m ywindowsname.keytab KVNO Principal ---- --------------------------------------------------------------- 0 mywindowsname@SAMPLE.COM ( 18 :AES256 CTS mode with HMAC SHA1-96 ) 0 mywindowsname@SAMPLE.COM ( 17 :AES128 CTS mode with HMAC SHA1-96 ) 0 mywindowsname@SAMPLE.COM ( 16 :DES3 CBC mode with SHA1-KD ) 0 mywindowsname@SAMPLE.COM ( 23 :RC4 with HMAC ) Delete unused encryption types ## 16-18 ktab -d mywindowsname@SAMPLE.COM -f -e 16 -k c: \\k erberos \\m ywindowsname.keytab Done! 1 entries removed. ktab -d mywindowsname@SAMPLE.COM -f -e 17 -k c: \\k erberos \\m ywindowsname.keytab Done! 1 entries removed. ktab -d mywindowsname@SAMPLE.COM -f -e 18 -k c: \\k erberos \\m ywindowsname.keytab Done! 1 entries removed. List content of the keytab file again ktab -l -e -k c: \\k erberos \\m ywindowsname.keytab Keytab name: c: \\k erberos \\m ywindowsname.keytab KVNO Principal ---- --------------------------------------------------------------- 0 mywindowsname@SAMPLE.COM ( 23 :RC4 with HMAC ) 3.3. Windows It depends on Windows account settings how many encryption types and what types can be used. Windows account properties dialog contains the next options for Kerberos authentication. 4. Encryption types As per RFC 3961, section 8 . Encryption Type Code Section or Comment des-cbc-crc 1 6.2.3 des-cbc-md4 2 6.2.2 des-cbc-md5 3 6.2.1 [reserved] 4 des3-cbc-md5 5 [reserved] 6 des3-cbc-sha1 7 dsaWithSHA1-CmsOID 9 (pkinit) md5WithRSAEncryption-CmsOID 10 (pkinit) sha1WithRSAEncryption-CmsOID 11 (pkinit) rc2CBC-EnvOID 12 (pkinit) rsaEncryption-EnvOID 13 (pkinit from PKCS#1 v1.5) rsaES-OAEP-ENV-OID 14 (pkinit from PKCS#1 v2.0) des-ede3-cbc-Env-OID 15 (pkinit) des3-cbc-sha1-kd 16 6.3 aes128-cts-hmac-sha1-96 17 [KRB5-AES] aes256-cts-hmac-sha1-96 18 [KRB5-AES] rc4-hmac 23 (Microsoft) rc4-hmac-exp 24 (Microsoft) subkey-keymaterial 65 (opaque; PacketCable)","tags":"Kerberos","url":"/create-keytab-file-for-kerberos-authentication-in-windows.html","loc":"/create-keytab-file-for-kerberos-authentication-in-windows.html"},{"title":"Build DBeaver Installation Package to Access Hive with JAAS Configuration Using Kerberos Authentication in Windows","text":"When your organization has decided to start using DBeaver, you need to plan how to deploy and setup DBeaver for users. It might not be an issue if you are going to utilize databases/drivers which included in the standard configuration. But let's imagine case when you need to connect to Hive with Kerberos authentication. This task request advanced skills and time for troubleshooting in case of encountering any issues. Setup The following sample based on Cloudera Hive JDBC driver v. 2.6.5.1007 and DBeaver EE v. 6.3.0 64 bit or DBeaver CE v. 7.0.0 64 bit edition. Download Windows 64 bit (zip) DBeaver installation and unzip it, for example, to C: drive. Your root folder is C:\\dbeaver-ce-7.0.0-win32.win32.x86_64 . Download and unzip Cloudera Hive JDBC driver. Place it in C:\\dbeaver-ce-7.0.0-win32.win32.x86_64\\DBeaverData\\drivers folder. C:\\dbeaver-ce-7.0.0-win32.win32.x86_64\\DBeaverData └───drivers └───hive_jdbc_2.6.5.1007 ├───ClouderaHiveJDBC41-2.6.5.1007 │ HiveJDBC41.jar │ └───docs Cloudera-JDBC-Driver-for-Apache-Hive-Install-Guide.pdf Cloudera-JDBC-Driver-for-Apache-Hive-Release-Notes.txt third-party-licenses.txt Copy or modify the following configuration files in C:\\dbeaver-ce-7.0.0-win32.win32.x86_64 DBeaver root folder. You can figure out how make them from Connect DBeaver to Cloudera Hive with JAAS Configuration using Kerberos Authentication in Windows article. 1) dbeaver.ini 2) jaas.conf 3) krb5.ini 4) organization-service-account.keytab Create dbeaver.vbs file and placed in C:\\dbeaver-ce-7.0.0-win32.win32.x86_64 DBeaver root folder as well. This file is a DBeaver launcher and our goal is to redirect DBeaver workspace from Windows User Home folder to DBeaver root one. We could create Windows batch file but it would show Command Prompt window during DBeaver start. Set WshShell = CreateObject ( \"WScript.Shell\" ) WshShell . Run \"dbeaver.exe -data ./DBeaverData/workspace6\" , 0 Set WshShell = Nothing Run DBeaver with dbeaver.vbs launcher and create a Hive driver and a new Hive connection as per Connect DBeaver to Cloudera Hive with JAAS Configuration using Kerberos Authentication in Windows article. After this step, it has to be created workspace folders by DBeaver. C:\\dbeaver-ce-7.0.0-win32.win32.x86_64\\DBeaverData └───workspace6 ├───.metadata │ ├───.plugins │ │ ├───org.eclipse.core.resources │ │ │ ├───.history │ │ │ ├───.projects │ │ │ │ └───General │ │ │ ├───.root │ │ │ │ └───.indexes │ │ │ └───.safetable │ │ ├───org.eclipse.core.runtime │ │ │ └───.settings │ │ ├───org.eclipse.e4.ui.workbench.swt │ │ ├───org.eclipse.e4.workbench │ │ ├───org.eclipse.ui.workbench │ │ ├───org.jkiss.dbeaver.core │ │ │ └───security │ │ ├───org.jkiss.dbeaver.model │ │ └───org.jkiss.dbeaver.ui │ └───qmdb └───General ├───.dbeaver └───Scripts The final step is to zip C:\\dbeaver-ce-7.0.0-win32.win32.x86_64 DBeaver root folder. Your zipped file name is dbeaver-ce-7.0.0-win32.win32.x86_64.zip . Deployment and Setup Users get dbeaver-ce-7.0.0-win32.win32.x86_64.zip file and they need to unzip it in a desired folder. If DBeaver has been set up in a network folder, it will be one limitation. You need to use mapped driver to run DBeaver, for example, Y:\\myuser\\desktop\\dbeaver-ce-7.0.0-win32.win32.x86_64 . It will not work if you run from \\\\networkserver\\UserHome\\myuser\\desktop\\dbeaver-ce-7.0.0-win32.win32.x86_64 folder. This limitation is caused by a reference to HiveJDBC41.jar library. You can run it from a network folder with UNC path but you need to make \\\\networkserver\\UserHome\\myuser\\desktop\\dbeaver-ce-7.0.0-win32.win32.x86_64 folder as your DBeaver root one and complete steps ## 1-6 in that folder.","tags":"DBeaver","url":"/build-dbeaver-installation-package-to-access-hive-with-jaas-configuration-using-kerberos-authentication-in-windows.html","loc":"/build-dbeaver-installation-package-to-access-hive-with-jaas-configuration-using-kerberos-authentication-in-windows.html"},{"title":"Connect DBeaver to Cloudera Hive with JAAS Configuration using Kerberos Authentication in Windows","text":"DBeaver allows connecting to a wide range of databases including Cloudera Hive. Hive driver is part of DBeaver installation but it uses basic authentication with user name and password. Kerberos authentication is another option to connect to Hive. It can be accomplished by adding a new driver to DBeaver. The DBeaver driver is based on Cloudera JDBC Driver for Hive and JAAS configuration file. Legend Before going forward, let's get agreed with the initial information used in configuration files. You can easily replace it with configurations applicable to your cases. It will be a kind of a template. Windows user name: mywindowsuser. Hive name host: hivehost. Kerberos realm if your organization supports multiple regions: REGION.SAMPLE.COM. Hive port: 10000. Prerequisites Kerberos configuration file: krb5.conf. It can be obtained from your Kerberos administrator or from the /etc/krb5.conf folder on the machine that is hosting the Hive Server 2 instance. One of the files below. It depends on which method is chosen. a) Kerberos credential cache: krb5cc_mywindowsuser. This file contains your Windows kerberized credentials. Using this file will request to renew it. b) Kerberos keytab file: mywindowsuser.keytab. This file stores your Windows kerberized credentials. It doesn't request renewal. See Create keytab File for Kerberos Authentication in Windows article. Setup The following sample based on DBeaver EE v. 6.3.0 64 bit and Cloudera Hive JDBC driver v. 2.6.5.1007 . Download and unzip Cloudera Hive JDBC driver. Place the unzipped folder to the permanent location. DBeaver reads it from that location every time when the driver is used. It's advisable to read the driver documentation from ./hive_jdbc_2.6.5.1007/docs folder. Registering the Driver Class section tells the class name used in DBeaver driver setup and Using Kerberos section explains each parameter utilized in the driver setup. Append those 4 lines to dbeaver.ini file. The location of the ini file can be different. If you have installed DBeaver, it will be in C:\\Program Files\\DBeaverEE folder. If you use zipped installation, it will be a root folder where it has been unzipped. -Djavax.security.auth.useSubjectCredsOnly = false -Dsun.security.krb5.debug = true -Djava.security.krb5.conf = C:/Users/mywindowsuser/DBeaver/krb5.ini -Djava.security.auth.login.config = C:/Users/mywindowsuser/DBeaver/jaas.conf It's very critical to use correct path separator. It has to be \"/\" forward slash character or you have to use \"\\\\\" double back slash characters. After successful completion of the setup, you need to remove the line. -Dsun.security.krb5.debug = true The final beaver.ini file is -startup plugins/org.eclipse.equinox.launcher_1.5.600.v20191014-2022.jar --launcher.library plugins/org.eclipse.equinox.launcher.win32.win32.x86_64_1.1.1100.v20190907-0426 -vmargs -XX:+IgnoreUnrecognizedVMOptions --add-modules = ALL-SYSTEM -Xms128m -Xmx2048m -Djavax.security.auth.useSubjectCredsOnly = false -Dsun.security.krb5.debug = true -Djava.security.krb5.conf = C:/Users/mywindowsuser/DBeaver/krb5.ini -Djava.security.auth.login.config = C:/Users/mywindowsuser/DBeaver/jaas.conf Move krb5.conf file with new krb5.ini name to C:/Users/mywindowsuser/DBeaver folder. You don't need to do any changes to the file. Create JAAS configuration file Option #1. Kerberos credential cache. Client { com.sun.security.auth.module.Krb5LoginModule required debug = true doNotPrompt = true useKeyTab = true keyTab = \"C:/Users/mywindowsuser/krb5cc_mywindowsuser\" useTicketCache = true renewTGT = true principal = \"mywindowsuser@REGION.SAMPLE.COM\"; }; Option #2. Kerberos keytab file. Client { com.sun.security.auth.module.Krb5LoginModule required debug = true doNotPrompt = true useKeyTab = true keyTab = \"mywindowsuser.keytab\" useTicketCache = false renewTGT = false principal = \"mywindowsuser@REGION.SAMPLE.COM\"; }; After successful completion of the setup, you need to remove the line. debug = true Create Kerberos authentication file The Kerberos tools are part of java installation and you need to figure out the location of your jre installation. It's assumed java 8 is installed in C:\\Program Files\\Java\\jre1.8.0_192 folder. Option #1. Kerberos credential cache. Run Windows Command Prompt with the command to create a cache file with your credentials. You can play with settings to accommodate your requirements. \"C:\\Program Files\\Java\\jre1.8.0_192\\bin\\kinit\" The successful completion is. Password for mywindowsuser@REGION.SAMPLE.COM: New ticket is stored in cache file C:\\Users\\mywindowsuser\\krb5cc_mywindowsuser Option #2. Kerberos keytab file. Run Windows Command Prompt with the command create a new keytab file with your credentials. \"C:\\Program Files\\Java\\jre1.8.0_192\\bin\\ktab\" -a mywindowsuser -k mywindowsuser.keytab The successful completion is. Password for mywindowsuser@REGION.SAMPLE.COM: Done! Service key for mywindowsuser is saved in myserviceuser.keytab Create a new driver in DBeaver Option #1. Kerberos credential cache. Driver Name: Hive-Cloudera Class Name: com.cloudera.hive.jdbc41.HS2Driver URL Template: jdbc:hive2://hivehost:10000/{database};AuthMech=1;KrbRealm=REGION.SAMPLE.COM;KrbServiceName=hive;KrbHostFQDN=hivehost.region.sample.com;KrbAuthType=2; Default Port: 10000 Category: Hadoop Add jar file to the driver: ./hive_jdbc_2.6.5.1007/ClouderaHiveJDBC41-2.6.5.1007/HiveJDBC41.jar Option #2. Kerberos keytab file. Driver Name: Hive-Cloudera Class Name: com.cloudera.hive.jdbc41.HS2Driver URL Template: jdbc:hive2://hivehost:10000/{database};AuthMech=1;KrbRealm=REGION.SAMPLE.COM;KrbServiceName=hive;KrbHostFQDN=hivehost.region.sample.com;KrbAuthType=1; Default Port: 10000 Category: Hadoop Add jar file to the driver: ./hive_jdbc_2.6.5.1007/ClouderaHiveJDBC41-2.6.5.1007/HiveJDBC41.jar Create a new connection in DBeaver Utilize the driver created on step #6. The name is Hive-Cloudera . Add default to Database/Schema field. Flag Save password locally . Test your new connection clicking Test Connection button. Troubleshooting Check the debug log in C:\\Users\\mywindowsuser\\AppData\\Roaming\\DBeaverData\\workspace6\\.metadata\\dbeaver-debug.log folder.","tags":"DBeaver","url":"/connect-dbeaver-to-cloudera-hive-with-jaas-configuration-using-kerberos-authentication-in-windows.html","loc":"/connect-dbeaver-to-cloudera-hive-with-jaas-configuration-using-kerberos-authentication-in-windows.html"},{"title":"Cron Scheduler with Docker Container in CentOS/RHEL 7","text":"Using cron with the official CentOS Docker image requests activating systemd, keeping container running, and opening a Docker container in privileged mode. CentOS Docker Hub image includes the description of the setup which should be done to activate systemd and keep a container going. The missing is information how to install and set up cron. This article provides a summary of steps and a functioning sample of Dockerfile. Some customization is needed to implement your cron project. Dockerfile The Dockerfile can be used as a template to design your file. After line #15, you can add your commands to install any packages. You need to replace line #20 with your time zone. Finally, line #22 shows how to add a scheduled job to crontab file. ROM centos:7 ENV container docker RUN ( cd /lib/systemd/system/sysinit.target.wants/ ; for i in * ; do [ $i == \\ systemd-tmpfiles-setup.service ] || rm -f $i ; done ) ; \\ rm -f /lib/systemd/system/multi-user.target.wants/* ; \\ rm -f /etc/systemd/system/*.wants/* ; \\ rm -f /lib/systemd/system/local-fs.target.wants/* ; \\ rm -f /lib/systemd/system/sockets.target.wants/*udev* ; \\ rm -f /lib/systemd/system/sockets.target.wants/*initctl* ; \\ rm -f /lib/systemd/system/basic.target.wants/* ; \\ rm -f /lib/systemd/system/anaconda.target.wants/* ; VOLUME [ \"/sys/fs/cgroup\" ] RUN yum install -y cronie && yum clean all RUN rm -rf /etc/localtime RUN ln -s /usr/share/zoneinfo/America/Toronto /etc/localtime RUN crontab -l | { cat ; echo \"25 04 * * sun,mon,tue python3 /app/do_maintenance.py\" ; } | crontab - CMD [ \"/usr/sbin/init\" ] Dockerfile logical parts Activating systemd. RUN ( cd /lib/systemd/system/sysinit.target.wants/ ; for i in * ; do [ $i == \\ systemd-tmpfiles-setup.service ] || rm -f $i ; done ) ; \\ rm -f /lib/systemd/system/multi-user.target.wants/* ; \\ rm -f /etc/systemd/system/*.wants/* ; \\ rm -f /lib/systemd/system/local-fs.target.wants/* ; \\ rm -f /lib/systemd/system/sockets.target.wants/*udev* ; \\ rm -f /lib/systemd/system/sockets.target.wants/*initctl* ; \\ rm -f /lib/systemd/system/basic.target.wants/* ; \\ rm -f /lib/systemd/system/anaconda.target.wants/* ; VOLUME [ \"/sys/fs/cgroup\" ] Install cron. RUN yum install -y cronie && yum clean all Set up your time zone. RUN rm -rf /etc/localtime RUN ln -s /usr/share/zoneinfo/America/Toronto /etc/localtime Add a job in crontab file. RUN crontab -l | { cat ; echo \"25 04 * * sun,mon,tue python3 /app/do_maintenance.py\" ; } | crontab - Keep container running. CMD [ \"/usr/sbin/init\" ] Build conatiner The image name is c7-cron and it's designated as local one. docker build --rm -t local/c7-cron . Run container To create and start a container, use the command. docker run --privileged --name = parking –v /sys/fs/cgroup:/sys/fs/cgroup:ro -d local/c7-cron To access the container in a terminal, run the command. docker exec -it parking /bin/bash","tags":"Docker","url":"/cron-scheduler-with-docker-container-in-centosrhel-7.html","loc":"/cron-scheduler-with-docker-container-in-centosrhel-7.html"},{"title":"Resize Disk in Oracle VM VirtualBox","text":"Virtualization is flexible in terms of using resources. A virtual machine can be built with minimal assigned resources and later, they can be added when it's needed. Oracle VM VirtualBox allows allocation more space to an existing disk. This procedure is common for all virtualized operation systems. It's similar to replacing your old hard drive with new one which is larger size. Run Oracle VM VirtualBox Manager and open settings of a virtual machine. It can be observed that the disk of the virtual machine is run out of space. Open Virtual Media Manager Select disk to add more space After applying the change, open settings of the virtual machine again to validate it Next steps depend on your operation system. You need to run your virtual machine and consume added space. Follow article to set it up for CentOS/RHEL.","tags":"Virtualization","url":"/resize-disk-in-oracle-vm-virtualbox.html","loc":"/resize-disk-in-oracle-vm-virtualbox.html"},{"title":"Expand Logical Volume in CentOS/RHEL 7","text":"As Linux systems have root file systems on a logical volume, it can be used Logical Volume Management (LVM) to resize the volume. The exercise of logical volume expanding is completed in case of adding an extra disk to a physical system or having a pool of storage in a virtual environment. Run fdisk or gdisk partition tool. gdisk is used if the partition layout is GPT otherwise fdisk has to be used. gdisk will make your system unbootable if you don't have GPT partition. Both tools work identically when a new partiton is created. How to figure out if GPT partition is present in your system? Assiming that sda is device with available space, run gdisk /dev/sda command. This screen shows that your partition is GPT. $ gdisk /dev/sda GPT fdisk (gdisk) version 0.8.10 Partition table scan: MBR: protective BSD: not present APM: not present GPT: present Found valid GPT with protective MBR; using GPT. In case of MBR partition your screen is. $ gdisk /dev/sda GPT fdisk (gdisk) version 0.8.10 Partition table scan: MBR: MBR only BSD: not present APM: not present GPT: not present *************************************************************** Found invalid GPT and valid MBR; converting MBR to GPT format in memory. THIS OPERATION IS POTENTIALLY DESTRUCTIVE! Exit by typing 'q' if you don't want to convert your MBR partitions to GPT format! *************************************************************** Create a new logical volume partition. Enter 8E00 partition code. Command (? for help): n Partition number (6-128, default 6): First sector (31457280-52428766, default = 31457280) or {+-}size{KMGTP}: Last sector (31457280-52428766, default = 52428766) or {+-}size{KMGTP}: Current type is 'Linux filesystem' Hex code or GUID (L to show codes, Enter = 8300): 8E00 Changed type of partition to 'Linux LVM' Apply changes. Command (? for help): w Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING PARTITIONS!! Do you want to proceed? (Y/N): Y OK; writing new GUID partition table (GPT) to /dev/sda. Warning: The kernel is still using the old partition table. The new table will be used at the next reboot. The operation has completed successfully. Notify the operation system about changes in the partition tables. $ partprobe Validate the new created partition. It can be used either fdisk or gdisk partition tool $ fdisk -l /dev/sda WARNING: fdisk GPT support is currently new, and therefore in an experimental phase. Use at your own discretion. Disk /dev/sda: 26.8 GB, 26843545600 bytes, 52428800 sectors Units = sectors of 1 * 512 = 512 bytes Sector size (logical/physical): 512 bytes / 512 bytes I/O size (minimum/optimal): 512 bytes / 512 bytes Disk label type: gpt Disk identifier: 71DD2E79-BD1C-4713-9880-22664C87E57B # Start End Size Type Name 1 2048 2099199 1G Linux filesyste Linux filesystem 2 2099200 16777215 7G Linux LVM Linux LVM 3 16777216 20971519 2G Linux LVM Linux LVM 4 20971520 31457279 5G Linux LVM Linux LVM 5 34 2047 1007K BIOS boot BIOS boot partition 6 31457280 52428766 10G Linux LVM Linux LVM Find out what logical groups/volumes are available. $ lvs LV VG Attr LSize Pool Origin Data% Meta% Move Log Cpy%Sync Convert root centos -wi-ao---- <13.19g swap centos -wi-ao---- 820.00m Our intertest is to add space to the root file system. The logical volume path is centos/root . In case of RHEL, it might be rhel/root . Create a physical volume. $ pvcreate /dev/sda6 WARNING: ext4 signature detected on /dev/sda6 at offset 1080. Wipe it? [y/n]: y Wiping ext4 signature on /dev/sda6. Physical volume \"/dev/sda6\" successfully created. Extend centos volume group. $ vgextend centos /dev/sda6 Volume group \"centos\" successfully extended Figure out exact free space in PE. The field name is Free PE / Size and the value in the sample is 2559 $ vgdisplay --- Volume group --- VG Name centos System ID Format lvm2 Metadata Areas 4 Metadata Sequence No 8 VG Access read/write VG Status resizable MAX LV 0 Cur LV 2 Open LV 2 Max PV 0 Cur PV 4 Act PV 4 VG Size 23.98 GiB PE Size 4.00 MiB Total PE 6140 Alloc PE / Size 3581 / <13.99 GiB Free PE / Size 2559 / <10.00 GiB VG UUID ZPaYGz-7hbZ-2H6y-RS9W-x13x-2K81-pXCsA3 Extend centos/root logical volume $ lvextend -l+2559 centos/root Size of logical volume centos/root changed from <13.19 GiB (3376 extents) to 23.18 GiB (5935 extents). Logical volume centos/root successfully resized. XFS file system may be grown while mounted using the xfs_growfs command. $ xfs_growfs /dev/centos/root meta-data=/dev/mapper/centos-root isize=512 agcount=9, agsize=406016 blks = sectsz=512 attr=2, projid32bit=1 = crc=1 finobt=0 spinodes=0 data = bsize=4096 blocks=3457024, imaxpct=25 = sunit=0 swidth=0 blks naming =version 2 bsize=4096 ascii-ci=0 ftype=1 log =internal bsize=4096 blocks=2560, version=2 = sectsz=512 sunit=0 blks, lazy-count=1 realtime =none extsz=4096 blocks=0, rtextents=0 data blocks changed from 3457024 to 6077440","tags":"Linux","url":"/expand-logical-volume-in-centosrhel-7.html","loc":"/expand-logical-volume-in-centosrhel-7.html"},{"title":"Convert MBR Partition into GPT in CentOS/RHEL 7","text":"Master Boot Record (MBR) partitioned disks are replaced with newer GUID Partition Table (GPT) standard but MBR is still used widely as a default format. GPT layout for the partition tables has a lot of benefits comparing with MBR one. Along with supporting significantly larger size of disks, it introduces faster and more stable booting. GPT requests to support Unified Extensible Firmware Interface (UEFI) boot. Switch to root user. $ sudo su - Run GPT partition tool. Assuming that sda disk is bootable and will be converted into GPT. $ gdisk /dev/sda GPT fdisk (gdisk) version 0.8.10 Partition table scan: MBR: MBR only BSD: not present APM: not present GPT: not present *************************************************************** Found invalid GPT and valid MBR; converting MBR to GPT format in memory. THIS OPERATION IS POTENTIALLY DESTRUCTIVE! Exit by typing 'q' if you don't want to convert your MBR partitions to GPT format! *************************************************************** Make sure that there is enough space before the first partition to support a boot partition. 2048 value for the first sector confirms that GPT can be applied to MBR. Command (? for help): p Disk /dev/sda: 52428800 sectors, 25.0 GiB Logical sector size: 512 bytes Disk identifier (GUID): 71DD2E79-BD1C-4713-9880-22664C87E57B Partition table holds up to 128 entries First usable sector is 34, last usable sector is 52428766 Partitions will be aligned on 2048-sector boundaries Total free space is 20973501 sectors (10.0 GiB) Number Start (sector) End (sector) Size Code Name 1 2048 2099199 1024.0 MiB 8300 Linux filesystem 2 2099200 16777215 7.0 GiB 8E00 Linux LVM 3 16777216 20971519 2.0 GiB 8E00 Linux LVM 4 20971520 31457279 5.0 GiB 8E00 Linux LVM Create a new bootable partition. As the first sector, enter 34 and the last sector is 2047 . Partition code is ef02 . Command (? for help): n Partition number (5-128, default 5): First sector (34-52428766, default = 31457280) or {+-}size{KMGTP}: 34 Last sector (34-2047, default = 2047) or {+-}size{KMGTP}: Current type is 'Linux filesystem' Hex code or GUID (L to show codes, Enter = 8300): ef02 Changed type of partition to 'BIOS boot partition' Save changes. Command (? for help): w Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING PARTITIONS!! Do you want to proceed? (Y/N): Y OK; writing new GUID partition table (GPT) to /dev/sda. Warning: The kernel is still using the old partition table. The new table will be used at the next reboot. The operation has completed successfully. Notify the operation system about changes. It eliminates rebooting of the system. $ partprobe Install GRUB on the new bootable partition. $ grub2-install /dev/sda Installing for i386-pc platform. Installation finished. No error reported. Validate the conversion. $ gdisk /dev/sda GPT fdisk (gdisk) version 0.8.10 Partition table scan: MBR: protective BSD: not present APM: not present GPT: present Found valid GPT with protective MBR; using GPT.","tags":"Linux","url":"/convert-mbr-partition-into-gpt-in-centosrhel-7.html","loc":"/convert-mbr-partition-into-gpt-in-centosrhel-7.html"},{"title":"Redirect HTTP to HTTPS in Synology NAS Nginx","text":"Synology DiskStation Manager (DSM) doesn't include GUI based functionality to set up a redirect HTTP web traffic to secured HTTPS version of your web site. The default web server in DSM 6 is Nginx and the configuration of the web server should be adjusted. It can be accomplished making manual changes to the Nginx web server moustache template. Prerequisites SSL certificate is added to Synology NAS. SSH service is enabled. Web Station is installed. Web server is Nginx. Environment Document is based on DSM 6.2.2 operation system Original moustache template server { listen 80 default_server{{#reuseport}} reuseport{{/reuseport}}; listen [::]:80 default_server{{#reuseport}} reuseport{{/reuseport}}; gzip on; {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / http://$host:{{DSM.port}}/ redirect; } } server { listen 443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; listen [::]:443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; {{#DSM.https.compression}} gzip on; {{/DSM.https.compression}} {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / https://$host:{{DSM.ssl.port}}/ redirect; } } Setup Use ssh client to access your Synology NAS with a user which has administrative permission. It can be PuTTY tool in Windows or terminal with ssh command in Unix. Switch to root user sudo su - Back up the current moustache template cp /usr/syno/share/nginx/WWWService.mustache /usr/syno/share/nginx/WWWService.mustache.bak Open the moustache template for editing vi /usr/syno/share/nginx/WWWService.mustache Replace 4 lines in port 80 section {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / http://$host:{{DSM.port}}/ redirect; } with those 2 lines server_name _; return 301 https://$host$request_uri; The final content should be server { listen 80 default_server{{#reuseport}} reuseport{{/reuseport}}; listen [::]:80 default_server{{#reuseport}} reuseport{{/reuseport}}; gzip on; server_name _; return 301 https://$host$request_uri; } server { listen 443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; listen [::]:443 default_server ssl{{#reuseport}} reuseport{{/reuseport}}; {{#DSM.https.compression}} gzip on; {{/DSM.https.compression}} {{> /usr/syno/share/nginx/WWW_Main}} location ~ &#94;/$ { rewrite / https://$host:{{DSM.ssl.port}}/ redirect; } } Restart the Nginx web server to apply the changes synoservicecfg --restart nginx The last important step is to refresh your browser. When you open your web site with http , it's still showing as http and don't redirect to https . Just click Ctrl-F5 .","tags":"Synology NAS","url":"/redirect-http-to-https-in-synology-nas-nginx.html","loc":"/redirect-http-to-https-in-synology-nas-nginx.html"},{"title":"Extract Number in Microsoft Access/Excel","text":"Microsoft Access and Excel don't include any string functions to extract a number from a string. It should be created a custom function to complete this task. Regular Expressions are a good option to deal with manipulation of text data. Microsoft Access and Excel are lacking of support of regular expressions but they allow to utilize third party libraries. A library of our interest is Microsoft VBScript Regular Expressions 5.5 one. Open your Microsoft Access database or Excel spreadsheet, then go to VBA Editor pressing Alt-F11 combination. Microsoft Access Microsoft Excel Create a new module calling context menu on the root node of the project tree. Microsoft Access Microsoft Excel Copy and paste the function below to the new created module. Function ExtractNumber ( textValue ) Dim re As Object Set re = CreateObject ( \"vbscript.RegExp\" ) re . Pattern = \"[&#94;\\d]\" re . Global = True ExtractNumber = re . Replace ( textValue , \"\" ) End Function This code uses late binding to the library. This method is not preferable but it reduces number of steps to implement the solution. Close VBA Editor. Create a table and a query in Microsoft Access and a column of values in Microsoft Excel to test the function. Microsoft Access Microsoft Excel","tags":"Microsoft Access","url":"/extract-number-in-microsoft-accessexcel.html","loc":"/extract-number-in-microsoft-accessexcel.html"},{"title":"Set Up Scanner in CentOS 7","text":"As a scanner software can be used Paperwork . It's open source software which is available in both Windows and Unix. CentOS 7 includes Paperwork scanner software as a part of the distribution repository. Make sure that your scanner driver installed in CentOS. This is a sample of Brother printer/scanner driver installation instructions from the official website. Install Scanner Access Now Easy (SANE) application programming interface that provides standardized access to any raster image scanner hardware. sudo yum install sane-backends Enable SANE connection required for scanning. sudo sh -c \"echo 127.0.0.1 >> /etc/sane.d/saned.conf\" Set up SANE service to start automatically and run the service. sudo systemctl enable saned.socket sudo systemctl start saned.socket Open Application Installer from System Tools menu and search for paperwork in search bar. Install Paperwork. After completion it will be available in Office menu. Run Paperwork and open Setting from menu Validate that Device , Default source , and Resolution populated properly as per your scanner.","tags":"Linux","url":"/set-up-scanner-in-centos-7.html","loc":"/set-up-scanner-in-centos-7.html"},{"title":"Clean Up USB Flash Drive","text":"When a USB flash drive is used in Linux or as an ISO image is recorded to it, USB drive might be is not usable in Windows. diskpart Windows tool can be used to clean up and re-partition USB flash drive. The tool can be run from Windows Command Prompt typing diskpart The first step is to identify an index of our USB drive. Run LIST DISK command Based on size, our flash drive index is 2 and the next step is to select our drive. Run SELECT DISK Now it's time to remove all content from the drive including any partitions. Run CLEAN command After making our flash drive empty, a new primary partition is created. Run CREATE PRIMARY PARTITION command Next step is to format flash drive. A list of file systems is FAT, FAT32, NTFS, exFAT. quick option allows to complete it very fast. Run FORMAT command Final step is to assign a letter to access the flash drive in Windows. Run ASSIGN command Close diskpart tool. Run EXIT","tags":"Windows","url":"/clean-up-usb-flash-drive.html","loc":"/clean-up-usb-flash-drive.html"}]};