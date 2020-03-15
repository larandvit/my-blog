Title: Connect DBeaver to Cloudera Hive with JAAS Configuration using Kerberos Authentication in Windows
Date: 2020-03-07
Category: DBeaver, Hive, Windows, JAAS, Kerberos, Authentication, Cloudera

DBeaver allows connecting to a wide range of databases including Cloudera Hive. Hive driver is part of DBeaver installation but it uses basic authentication with user name and password. Kerberos authentication is another option to connect to Hive. It can be accomplished by adding a new driver to DBeaver. The DBeaver driver is based on Cloudera JDBC Driver for Hive and JAAS configuration file.

## Legend

Before going forward, let's get agreed with the initial information used in configuration files. You can easily replace it with configurations applicable to your cases. It will be a kind of a template.

1. Windows user name: mywindowsuser.

2. Hive name host: hivehost.

3. Kerberos realm if your organization supports multiple regions: REGION.SAMPLE.COM.

4. Hive port: 10000.


## Prerequisites

1. Kerberos configuration file: krb5.conf. It can be obtained from your Kerberos administrator or from the **/etc/krb5.conf** folder on the machine that is hosting the Hive Server 2 instance.

2. One of the files below. It depends on which method is chosen.

    a) Kerberos credential cache: krb5cc_mywindowsuser. This file contains your Windows kerberized credentials. Using this file will request to renew it.

    b) Kerberos keytab file: mywindowsuser.keytab. This file stores your Windows kerberized credentials. It doesn't request renewal. See [Create keytab File for Kerberos Authentication in Windows]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md) article.

## Setup

The following sample based on **DBeaver EE v. 6.3.0 64 bit** and **Cloudera Hive JDBC driver v. 2.6.5.1007**.

1. [Download](https://www.cloudera.com/downloads/connectors/hive/jdbc/2-6-5.html) and unzip Cloudera Hive JDBC driver. 

    Place the unzipped folder to the permanent location. DBeaver reads it from that location every time when the driver is used.

    It's advisable to read the driver documentation from **./hive_jdbc_2.6.5.1007/docs** folder. **Registering the Driver Class** section tells the class name used in DBeaver driver setup and **Using Kerberos** section explains each parameter utilized in the driver setup.

2. Append those 4 lines to **dbeaver.ini** file. The location of the **ini** file can be different. If you have installed DBeaver, it will be in **C:\Program Files\DBeaverEE** folder. If you use zipped installation, it will be a root folder where it has been unzipped.

        :::ini
        -Djavax.security.auth.useSubjectCredsOnly=false
        -Dsun.security.krb5.debug=true
        -Djava.security.krb5.conf=C:/Users/mywindowsuser/DBeaver/krb5.ini
        -Djava.security.auth.login.config=C:/Users/mywindowsuser/DBeaver/jaas.conf

    It's very critical to use correct path separator. It has to be **"/"** forward slash character or you have to use **"\\\"** double back slash characters.

    After successful completion of the setup, you need to remove the line.

        :::ini
        -Dsun.security.krb5.debug=true

    The final **beaver.ini** file is

        :::ini
        -startup
        plugins/org.eclipse.equinox.launcher_1.5.600.v20191014-2022.jar
        --launcher.library
        plugins/org.eclipse.equinox.launcher.win32.win32.x86_64_1.1.1100.v20190907-0426
        -vmargs
        -XX:+IgnoreUnrecognizedVMOptions
        --add-modules=ALL-SYSTEM
        -Xms128m
        -Xmx2048m
        -Djavax.security.auth.useSubjectCredsOnly=false
        -Dsun.security.krb5.debug=true
        -Djava.security.krb5.conf=C:/Users/mywindowsuser/DBeaver/krb5.ini
        -Djava.security.auth.login.config=C:/Users/mywindowsuser/DBeaver/jaas.conf

3. Move **krb5.conf** file with new **krb5.ini** name to **C:/Users/mywindowsuser/DBeaver** folder. You don't need to do any changes to the file.

4. Create JAAS configuration file
    
    Option #1. Kerberos credential cache.

        :::ini
        Client {
        com.sun.security.auth.module.Krb5LoginModule required
        debug=true
        doNotPrompt=true
        useKeyTab=true
        keyTab="C:/Users/mywindowsuser/krb5cc_mywindowsuser"
        useTicketCache=true
        renewTGT=true
        principal="mywindowsuser@REGION.SAMPLE.COM";
        };

    Option #2. Kerberos keytab file.

        :::ini
        Client {
        com.sun.security.auth.module.Krb5LoginModule required
        debug=true
        doNotPrompt=true
        useKeyTab=true
        keyTab="mywindowsuser.keytab"
        useTicketCache=false
        renewTGT=false
        principal="mywindowsuser@REGION.SAMPLE.COM";
        };

       After successful completion of the setup, you need to remove the line.

        :::ini
        debug=true

5. Create Kerberos authentication file
    
    The Kerberos tools are part of java installation and you need to figure out the location of your jre installation. It's assumed java 8 is installed in C:\Program Files\Java\jre1.8.0_192 folder.

    Option #1. Kerberos credential cache.
    
    Run Windows Command Prompt with the command to create a cache file with your credentials. You can play with settings to accommodate your requirements.

        :::batch
        "C:\Program Files\Java\jre1.8.0_192\bin\kinit"
        
    The successful completion is.

        :::text
        Password for mywindowsuser@REGION.SAMPLE.COM:
        New ticket is stored in cache file C:\Users\mywindowsuser\krb5cc_mywindowsuser

    Option #2. Kerberos keytab file.
    
    Run Windows Command Prompt with the command create a new keytab file with your credentials.

        :::batch
        "C:\Program Files\Java\jre1.8.0_192\bin\ktab" -a mywindowsuser -k mywindowsuser.keytab
    
    The successful completion is.
       
        :::text
        Password for mywindowsuser@REGION.SAMPLE.COM:
        Done!
        Service key for mywindowsuser is saved in myserviceuser.keytab

6. Create a new driver in DBeaver

    Option #1. Kerberos credential cache.

    * Driver Name: **Hive-Cloudera**
    * Class Name: **com.cloudera.hive.jdbc41.HS2Driver**
    * URL Template: **jdbc:hive2://hivehost:10000/{database};AuthMech=1;KrbRealm=REGION.SAMPLE.COM;KrbServiceName=hive;KrbHostFQDN=hivehost.region.sample.com;KrbAuthType=2;**
    * Default Port: **10000**
    * Category: **Hadoop**
    * Add jar file to the driver: **./hive_jdbc_2.6.5.1007/ClouderaHiveJDBC41-2.6.5.1007/HiveJDBC41.jar**<br><br>

    Option #2. Kerberos keytab file.

    * Driver Name: **Hive-Cloudera**
    * Class Name: **com.cloudera.hive.jdbc41.HS2Driver**
    * URL Template: **jdbc:hive2://hivehost:10000/{database};AuthMech=1;KrbRealm=REGION.SAMPLE.COM;KrbServiceName=hive;KrbHostFQDN=hivehost.region.sample.com;KrbAuthType=1;**
    * Default Port: **10000**
    * Category: **Hadoop**
    * Add jar file to the driver: **./hive_jdbc_2.6.5.1007/ClouderaHiveJDBC41-2.6.5.1007/HiveJDBC41.jar**<br><br>

7. Create a new connection in DBeaver

    Utilize the driver created on step #6. The name is **Hive-Cloudera**. 

    Add **default** to Database/Schema field.

    Flag **Save password locally**.

    Test your new connection clicking **Test Connection** button.

## Troubleshooting

Check the debug log in **C:\Users\mywindowsuser\AppData\Roaming\DBeaverData\workspace6\\.metadata\dbeaver-debug.log** folder.
