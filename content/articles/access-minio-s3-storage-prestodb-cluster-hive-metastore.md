Title: Access MinIO S3 Storage in Presto with Hive Metastore
Date: 2020-10-21
Category: Presto, MinIO
Cover: /extra/prestodb-logo.png

[Presto](https://prestosql.io/) Hive connector is aimed to access HDFS or S3 compatible storages. One of the key components of the connector is metastore which maps data files with schemas and tables. Two production metastore services are Hive and AWS Glue Data Catalog. Hive metastore works transparently with [MinIO](https://min.io/) S3 compatible system. One more non official metastore is file. The information about file metastore can be find in [Access MinIO S3 Storage in Presto with File Metastore]({filename}/articles/access-minio-s3-storage-prestodb-cluster.md) article.

The sample is based on [Starburst](https://www.starburstdata.com/) 343-e open source distribution with RPM installation and RHEL 7 Linux distribution.

##1. Set up Hive metastore for MinIO

It might be followed [Hive Standalone Metastore for Presto in Docker]({filename}/articles/standalone-hive-metastore-presto-docker.md) manual.

##2. Create a connector file

A set of mandatory parameters are.

    :::ini
    connector.name=hive-hadoop2

    hive.metastore.uri=thrift://URL:9083
    hive.metastore.username=metastore

    hive.s3.aws-access-key=access key
    hive.s3.aws-secret-key=secret key
    hive.s3.endpoint=http://URL:9000
    hive.s3.path-style-access=true

* Replace Hive metastore URL in `hive.metastore.uri` property. The Hive metastore default port is `9083`.
* Replace credentials to access MinIO in `hive.s3.aws-access-key` and `hive.s3.aws-secret-key` properties.
* Replace MinIO URL in `hive.s3.endpoint` property. The default port is `9000`. 
* In case if used `SSL` connection to MinIO server, replace `http` protocol with `https` in `hive.s3.endpoint` property, for example, `hive.s3.endpoint=https://<URL>:9000` and add `hive.s3.ssl.enabled=true` property.

##3. Deploy the connector file to each Presto node in a cluster

The location of the Hive connector file is `/etc/presto/catalog/` folder. The name might be `mini.properties`. `properties` is the extension.

**Presto Admin** tool can automate deployment of the connector file to a cluster.

    :::bash
    presto-admin catalog add minio -I -u <user with sudo permissions>

#4. Restart the Presto cluster

**Presto Admin** tool can be handy as well.

    :::bash
    presto-admin server restart -I -u <user with sudo permissions>

#5. Improvements

* Instruct Presto to submit filter and aggregation statements directly for MinIO execution.

     `hive.s3select-pushdown.enabled=true`

* Make communication between Presto and MinIo more reliable increasing timeout.

     `hive.s3.socket-timeout=1m`

## Resources
* [Hive Connector](https://prestosql.io/docs/current/connector/hive.html)
