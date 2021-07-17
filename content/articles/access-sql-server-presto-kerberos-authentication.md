Title: Access MS SQL Server in Presto with Kerberos Keytab Authentication
Date: 2020-11-12
Category: Trino(Presto), MS SQL Server
Cover: /extra/trino-logo.png

[Presto](https://prestosql.io/) SQL Server connector accesses SQL Server databases using SQL Server credentials. The connector properties contain SQL Server user name and password. This is only an option supported by Presto. Because of using SQL Server jdbc driver in SQL Server connector, it opens more ways to access MS SQL Server. Kerberos keytab is a part of SQL Server jdbc driver setup with Java Authentication and Authorization Service (JAAS).

The sample is based on [Starburst](https://www.starburstdata.com/) 339-e open source distribution with RPM installation and RHEL 7 Linux distribution.

##1. Download SQL Server jdbc driver

* The download page is [here](https://docs.microsoft.com/en-us/sql/connect/jdbc/download-microsoft-jdbc-driver-for-sql-server?view=sql-server-ver15). 
* Pick up either `zip` or `tar.gz` package.
* Extract mssql-jdbc-8.4.1.jre8.jar, mssql-jdbc-8.4.1.jre11.jar, and mssql-jdbc-8.4.1.jre14.jar files.
* Copy a file corresponding your java version to `/etc/presto` folder on a coordinator and workers, for example, if Presto cluster is run in java 11, take mssql-jdbc-8.4.1.jre11.jar file.

##2. Generate keytab file

See [Create keytab File for Kerberos Authentication in Windows]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md) article.

The file location is `/etc/presto` folder on a coordinator and workers, for example, `/etc/presto/sqlserver.keytab`.

##3. Create jaas file

The file has to be deployed to a coordinator and workers in `/etc/presto` folder. the name might be `conf.jaas`.

    :::ini
    SQLJDBCDriver {
    com.sun.security.auth.module.Krb5LoginModule required
    debug=true
    doNotPrompt=true
    useKeyTab=true
    keyTab="/etc/presto/sqlserver.keytab"
    useTicketCache=false
    renewTGT=false
    principal="mywindowsname@SAMPLE.COM";
    };

After successful completion of the setup, remove `debug=true` line.

##3. Modify jvm.config file

Add `-Djava.security.auth.login.config=/etc/presto/conf.jaas` line on a coordinator and workers to `/etc/presto/jvm.config` file.

##4. Create Kerberos configuration file

The file is `krb5.conf` and the location is `/etc` folder on a coordinator and workers.

##5. Create a SQL Server connector file

    :::ini
    connector.name=sqlserver
    connection-url=jdbc:sqlserver://sqlserverserver.sample.com;databaseName=yourdatabasename;integratedSecurity=true;authenticationScheme=JavaKerberos;jaasConfigurationName=SQLJDBCDriver

* Replace **sqlserverserver.sample.com** with your SQL server name. It has to be Fully Qualified Domain Name (FQDN).
* Replace **yourdatabasename** with SQL Server database name.

##6. Deploy the SQL Server connector file to each Presto node in a cluster

The location of the SQL Server connector file is `/etc/presto/catalog/` folder. The name might be `sqlserver.properties`. `properties` is the extension.

**Presto Admin** tool can automate deployment of the connector file to a cluster.

    :::bash
    presto-admin catalog add sqlserver -I -u <user with sudo permissions>

##7. Restart the Presto cluster

**Presto Admin** tool can be handy as well.

    :::bash
    presto-admin server restart -I -u <user with sudo permissions>

## Resources
* [Using Kerberos integrated authentication to connect to SQL Server](https://docs.microsoft.com/en-us/sql/connect/jdbc/using-kerberos-integrated-authentication-to-connect-to-sql-server?view=sql-server-ver15)
* [Setting the connection properties](https://docs.microsoft.com/en-us/sql/connect/jdbc/setting-the-connection-properties?view=sql-server-ver15)
