Title: Hive Standalone Metastore for Trino in Docker
Date: 2020-10-20
Modified: 2023-02-26
Category: Trino, Hive, Docker
Cover: /extra/trino-logo.png

Hive connector in Trino can retrieve data from both HDFS and S3 compatible storages. The connector requests Hive metastore as a link to those storages. There are two types of metastores. The first one is beefy one which includes multiple services. The second one is light weight and is called standalone metastore. It contains only Hive service. The standalone metastore is used to connect to S3 compatible storages.

When there is one S3 endpoint, a coordination or another server can host Hive standalone metastore. In case of many S3 endpoints, it is requested to have a Hive metastore for each endpoint. It is possible to dedicate one metastore but it should be applied a special Apache Ranger setup to separate each S3 endpoint. 

To save resources, a coordinator might be used to set up Hive standalone meatostores. Docker containers host those Hive metastores. This configuration simplifies setup and maintenance of a Trino cluster.

The sample is based on [Starburst](https://www.starburstdata.com/) 343-e open source distribution with RPM installation, Hive standalone metastoere 3.1.2, MinIO S3 storage, and RHEL 7 Linux distribution.

##1. Install Docker

See [Install Docker CE Edition in CentOS/RHEL 7]({filename}/articles/install-docker-ce-centosrhel-7.md) article.

##2. Create Docker working folder

Log in to a Docker host with sudo user.

    :::bash
    mkdir docker_hive
    cd docker_hive

##3. Download and prepare requested software

* Hive standalone metastore 3.1.2

        :::bash
         wget https://repo1.maven.org/maven2/org/apache/hive/hive-standalone-metastore/3.1.2/hive-standalone-metastore-3.1.2-bin.tar.gz
        tar -xvf hive-standalone-metastore-3.1.2-bin.tar.gz
        rm -f hive-standalone-metastore-3.1.2-bin.tar.gz
        mv apache-hive-metastore-3.1.2-bin metastore

* Hadoop 3.2.1

        :::bash
        wget http://apache.mirrors.hoobly.com/hadoop/common/hadoop-3.2.1/hadoop-3.2.1.tar.gz
        tar -xvf hadoop-3.2.1.tar.gz
        rm -f hadoop-3.2.1.tar.gz

* JDBC connector

     Option #1. Postgres backend. Replace postgresql-<version>.jar with the latest one.

        :::bash
        wget https://jdbc.postgresql.org/download/postgresql-42.2.16.jar

    Option #2. MySQL backend. Replace mysql-connector-java-<version>.el7.noarch.rpm with the latest one. MySQL connector is included in the RPM package, so after installation, the connector can be found in `/usr/share/java` folder.

        :::bash
        wget https://cdn.mysql.com//Downloads/Connector-J/mysql-connector-java-8.0.21-1.el7.noarch.rpm
        yum install mysql-connector-java-8.0.21-1.el7.noarch.rpm
        cp /usr/share/java/mysql-connector-java.jar ./

    There are more options for a backend but those two are tested. See **Resources** section below.

##4. Install backend server and create a database

A backend server can be installed on coordinator or another box. 

A database can be created, for example, 

* Postgres

     Using **pgAdmin** GUI tool.

* MySQL

     Commndline **mysql** tool

        :::bash
        mysql -u root -p
        create database metastore;
        exit

The database name might be `metastore`.

##5. Request corporate Certificate Authority PEM file

Copy `ca.pem` file to `docker_hive` working folder.

##6. Create Dockerfile

    :::dockerfile
    FROM centos:7
        
    ENV container docker
        
    LABEL maintainer="your name here"
        
    # copy Certificate Authority file
    COPY ca.pem /etc/pki/ca-trust/source/anchors/
     
    # copy Hive standalone package
    COPY metastore /opt/metastore/
    
    # copy Hadoop package
    COPY hadoop-3.2.1 /opt/hadoop-3.2.1/
    
    # copy Postgres or MySQL JDBC connector
    COPY postgresql-42.2.16.jar /opt/metastore/lib/
    
    # add Certificate Authority to database
    RUN update-ca-trust
    
    WORKDIR /install
    
    # install Java 1.8 and clean cache
    RUN yum install -y java-1.8.0-openjdk-devel \
      && yum clean all
    
    # environment variables requested by Hive metastore
    ENV JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk
    ENV HADOOP_HOME=/opt/hadoop-3.2.1
    
    # replace a library and add missing libraries
    RUN rm -f /opt/metastore/lib/guava-19.0.jar \
      && cp ${HADOOP_HOME}/share/hadoop/common/lib/guava-27.0-jre.jar /opt/metastore/lib \
      && cp ${HADOOP_HOME}/share/hadoop/tools/lib/hadoop-aws-3.2.1.jar /opt/metastore/lib \
      && cp ${HADOOP_HOME}/share/hadoop/tools/lib/aws-java-sdk-bundle-1.11.375.jar /opt/metastore/lib
    
    WORKDIR /opt/metastore
    
    # copy Hive metastore configuration file
    COPY metastore-site.xml /opt/metastore/conf/
    
    # Hive metastore data folder
    VOLUME ["/user/hive/warehouse"]
    
    # create metastore backend tables and insert data. Replace postgres with mysql if MySQL backend used
    RUN bin/schematool -initSchema -dbType postgres
    
    CMD ["/opt/metastore/bin/start-metastore"]

In case of creating more than one Hive metastore, replace `VOLUME ["/user/hive/warehouse"]` path. Data is not supposed to be stored in that folder as MinIO or S3 compatible storage is aimed as a data storage.

##7. Create Hive metastore setup file

    :::xml
    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <?xml-stylesheet type="text/xsl" href="configuration.xsl"?>
    <configuration>
    	<property>
    		<name>fs.s3a.access.key</name>
    		<value>AccessKey</value>
    	</property>
    	<property>
    		<name>fs.s3a.secret.key</name>
    		<value>SecretKey</value>
    	</property>
    	<property>
    		<name>fs.s3a.connection.ssl.enabled</name>
    		<value>true</value>
    	</property>
    	<property>
    		<name>fs.s3a.path.style.access</name>
    		<value>true</value>
    	</property>
    	<property>
    		<name>fs.s3a.endpoint</name>
    		<value>MinIO URL:9000</value>
    	</property>
    	<property>
    		<name>javax.jdo.option.ConnectionURL</name>
    		<value>jdbc:postgresql://Backend URL or name:30684/metadata?allowPublicKeyRetrieval=true&amp;useSSL=false&amp;serverTimezone=UTC</value>
    	</property>
    	<property>
    		<name>javax.jdo.option.ConnectionDriverName</name>
    		<value>org.postgresql.Driver</value>
    	</property>
    	<property>
    		<name>javax.jdo.option.ConnectionUserName</name>
    		<value>Backend user name</value>
    	</property>
    	<property>
    		<name>javax.jdo.option.ConnectionPassword</name>
    		<value>Backend user password</value>
    	</property>
    	<property>
    		<name>hive.metastore.event.db.notification.api.auth</name>
    		<value>false</value>
    	</property>
    	<property>
    		<name>metastore.thrift.uris</name>
    		<value>thrift://localhost:9083</value>
    		<description>Thrift URI for the remote metastore. Used by metastore client to connect to remote metastore.</description>
    	</property>
    	<property>
    		<name>metastore.task.threads.always</name>
    		<value>org.apache.hadoop.hive.metastore.events.EventCleanerTask</value>
    	</property>
    	<property>
            <name>metastore.expression.proxy</name>
            <value>org.apache.hadoop.hive.metastore.DefaultPartitionExpressionProxy</value>
    	</property>
        <property>
            <name>metastore.warehouse.dir</name>
            <value>/user/hive/warehouse</value>
        </property>
    </configuration>

* Replace `AccessKey` with MinIO access key.
* Replace SecretKey with MinIO secret key.
* Make `fs.s3a.connection.ssl.enabled` property `true` if MinIO is secured with `HTTPS` protocol otherwise `false`.
* Replace `MinIO URL:9000` with MinIO address and port.
* Replace `Backend URL or name:30684` with backend address and port. If it is installed on a coordinator, use `localhost`.
* If MySQL is used as backend, replace `javax.jdo.option.ConnectionURL` property with `jdbc:mysql://Backend URL or name:3306/metastoretrino?allowPublicKeyRetrieval=true&amp;useSSL=false&amp;serverTimezone=UTC`.
* If MySQL is used as backend, replace `javax.jdo.option.ConnectionDriverName` property with `com.mysql.cj.jdbc.Driver`.
* Replace `Backend user name` with backend user name.
* Replace `Backend user password` with backend user password.
* Replace `/user/hive/warehouse` value `metastore.warehouse.dir` property in to match with `VOLUME ["/user/hive/warehouse"]` **Dockerfile**.
* If more than one Hive metastore, add `metastore.warehouse.dir` and `metastore.thrift.port` properties, for example,

        :::xml
        <property>
            <name>metastore.thrift.port</name>
            <value>9084</value>
        </property>

##8. Create Docker image

    :::bash
    docker build -t minio_hiveimage  .

The command adds tables to backend database as well.

##9. Run Hive metastore

    :::bash
    docker run -d -p 9083:9083/tcp --name mini_hive minio_hiveimage

if more than one metastore, replace port for next metastore.

    :::bash
    docker run -d -p 9084:9084/tcp --name mini_hive2 minio_hiveimage2

##10. Test metastore

* Add Hive connector for MinIO storage, for example, `minio_connector`. See [Access MinIO S3 Storage in Trino with Hive Metastore]({filename}/articles/access-minio-s3-storage-prestodb-cluster-hive-metastore.md) article. It is a catalog level in Trino hierarchy.

* Create a backet to store your schema, for example, `finance-department`.

* Create a schema

        :::sql
        CREATE SCHEMA minio_connector.finance_department
        WITH (
           location = 's3a://finance-department/'
        );

     Use dash (`-`) in the MinIO bucket name and underscore (`_`) in the schema name.

* Create a table

        :::sql
        CREATE TABLE minio_connector.finance_department.test_table (col1 varchar, col2 varchar);

* Insert values to the test table

        :::sql
        INSERT INTO minio_connector.finance_department.test_table
        SELECT 'value1.1', 'value1.2';

Everything has to be completed without any error messages and `finance_department` bucket will conatain `test_table` folder with a file.

## Troubleshooting

* Hive metastore accessible

    Validate if Hive metastore is visible from a coordinator and workers.

        :::bash
        curl -v telnet://hive.example.com:9083

    Success

        :::text
        *   Trying xx.x.xx.xx:9083...
        * Connected to hive.example.com (xx.x.xx.xx) port 9083 (#0)

    Failed

        :::text
        *   Trying xx.x.xx.x:9...
        * connect to xx.x.xx.xx port 3 failed: Connection refused
        * Failed to connect to hive.example.com port 3 after 0 ms: Connection refused
        * Closing connection 0
        curl: (7) Failed to connect to hive.example.com port 3 after 0 ms: Connection refused

* [Troubleshooting Access to HTTP/HTTPS Resources in Docker]({filename}/articles/troubleshooting-access-https-resources-docker.md)

## Resources

* [AdminManual Metastore 3.0 Administration](https://cwiki.apache.org/confluence/display/Hive/AdminManual+Metastore+3.0+Administration)
* [Hive Schema Tool](https://cwiki.apache.org/confluence/display/Hive/Hive+Schema+Tool)


