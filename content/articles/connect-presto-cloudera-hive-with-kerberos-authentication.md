Title: Connect Presto to Cloudera Hive with Kerberos Authentication
Date: 2020-10-12
Category: Trino(Presto), Hive, Kerberos
Cover: /extra/trino-logo.png

Presto includes Hive connector to access Hive data warehouse. Warehouse data is stored in the Hadoop Distributed File System (HDFS) or in S3 compatible storages. Data files located in Hive warehouse are in varieties of formats and data size can be enormous. Presto uses Hive metastore to discover schemas and tables in undelaying data files and runs its own query engine. Kerberos authentication with keytab is applied to access HDFS and Hive metastore.

The sample is based on [Starburst](https://www.starburstdata.com/) 343-e open source distribution, Cloudera CDH 5.7.0 and RHEL 7 Linux distribution.

##1. Extract setup files from Cloudera CDH and copy to Presto nodes

* Open **Cloudera Manager**.

![Cloudera Manager]({static}/images/connect-presto-cloudera-hive-with-kerberos-authentication/cloudera-manager.jpg)</br></br>

* Click on **Hive** service and then select **Download Client Configuration** in **Actions** button.

![Download Hive client configuration]({static}/images/connect-presto-cloudera-hive-with-kerberos-authentication/hive-download-client-configuration.jpg)</br></br>

* Extract `core-site.xml` and `hdfs-site.xml` files from `hive-clientconfig.zip` downloaded archive.

* Copy files to `/etc/presto` folder in Presto nodes.

##2. Copy Kerberos setup file from Cloudera CDH host to Presto nodes

The file name is `krb5.conf` and it is located in `/etc` folder on Cloudera CDH host and it has to be copied to `/etc` folder in Presto nodes.

##3. Create keytab file

The file is supposed to be provided by the security team of your company. Follow the information in [Create keytab File for Kerberos Authentication in Windows]({filename}/articles/create-keytab-file-for-kerberos-authentication-in-windows.md) or [Create keytab File for Kerberos Authentication in Linux]({filename}/articles/create-keytab-file-kerberos-authentication-linux.md) article. 

Distribute `principal_name.keytab` created file to `/etc/presto` folder in Presto nodes.

##4. Create connector file

    :::ini
    connector.name=hive-hadoop2

    hive.metastore.uri=thrift://hive_meta_store_host.sample.com:9083

    hive.metastore.authentication.type=KERBEROS
    hive.metastore.service.principal=hive/_HOST@SAMPLE.COM
    hive.metastore.client.principal=principal_name@SAMPLE.COM
    hive.metastore.client.keytab=/etc/presto/principal_name.keytab

    hive.hdfs.authentication.type=KERBEROS
    hive.hdfs.presto.principal=principal_name@SAMPLE.COM
    hive.hdfs.presto.keytab=/etc/presto/principal_name.keytab

    hive.config.resources=/etc/presto/core-site.xml,/etc/presto/hdfs-site.xml

* Hive metastore host: hive_meta_store_host.sample.com
* Hive metastore default port: 9083
* Company domain: SAMPLE.COM
* Account name in keytab: principal_name
* Keytab location and name: /etc/presto/principal_name.keytab

The same account and keytab are used to access HDFS.

##5. Deploy connector file to Presto cluster

The connector goes to `/etc/presto/catalog` folder in each Presto node and after Presto cluster has to be restarted. 

Presto Admin can accomplish it.

    :::bash
    presto-admin catalog add hive_connector -I -u <user name with sudo access>
    presto-admin server restart -I -u <user name with sudo access>

