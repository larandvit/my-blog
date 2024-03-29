Title: Access MinIO S3 Storage in Trino with Hive Metastore
Date: 2020-10-21
Modified: 2022-12-10
Category: Trino, MinIO
Cover: /extra/trino-logo.png

[Trino](https://trino.io/) Hive connector is aimed to access HDFS or S3 compatible storages. One of the key components of the connector is metastore which maps data files with schemas and tables. Two production metastore services are Hive and AWS Glue Data Catalog. Hive metastore works transparently with [MinIO](https://min.io/) S3 compatible system. One more non official metastore is file. The information about file metastore can be find in [Access MinIO S3 Storage in Trino with File Metastore]({filename}/articles/access-minio-s3-storage-prestodb-cluster.md) article.

The sample is based on [Starburst](https://www.starburstdata.com/) 393-e open source distribution with RPM installation and RHEL 7 Linux distribution.

##1. Set up Hive metastore for MinIO

It might be followed [Hive Standalone Metastore for Trino in Docker]({filename}/articles/standalone-hive-metastore-presto-docker.md) manual.

##2. Create a connector file

A set of mandatory parameters are.

    :::ini
    connector.name=hive

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

##3. Deploy the connector file to each Trino node in a cluster

The location of the Hive connector file is `/etc/trino/catalog/` folder. The name might be `mini.properties`. `properties` is the extension.

##4. Restart the Trino cluster

##5. Tests to access MinIO data

###5.1 Managed table

Trino is responsible for deleting table definition and MinIO data when a table is deleted.

* Create a schema

        :::sql
        CREATE SCHEMA minio.sample_schema
        WITH (
           location = 's3a://sample-bucket/'
        );

     Use dash (`-`) in the MinIO bucket name and underscore (`_`) in the schema name.

* Create a table

        :::sql
        CREATE TABLE minio.sample_schema.sample_table (
           col1 varchar, 
           col2 varchar);

* Insert values to the test table

        :::sql
        INSERT INTO minio.sample_schema.sample_table
        SELECT 'value1.1', 'value1.2';

###5.2 Non-managed table

This type of tables is different from the managed one that when a table is deleted, MinIO data is not deleted.

* Create a schema

        :::sql
        CREATE SCHEMA minio.sample_schema;
 
* Create a table
       
        :::sql
        CREATE TABLE minio.sample_schema.sample_table(
            col1 varchar, 
            col2 varchar)
        WITH (
          external_location = 's3a://sample_bucket/sample_table/',
          format = 'TEXTFILE');

* Insert values to the test table

        :::sql
        INSERT INTO minio.sample_schema.sample_table
        SELECT 'value1.1', 'value1.2';

###5.3 Non-managed table with already existing data in MinIO

It can be a case when data has been added already and a table schema is applied to access data as a table. To make sample simple, one column is defined which incorporates all columns and column delimiters. Each text file has a header line.

* Create a schema

        :::sql
        CREATE SCHEMA minio.sample_schema;
 
* Create a table
       
        :::sql
        CREATE TABLE minio.sample_schema.sample_table(
            all_columns varchar)
        WITH (
          external_location = 's3a://sample_bucket/sample_table/',
          format = 'TEXTFILE',
          skip_header_line_count=1);

##6. Improvements

* Instruct Trino to submit filter and aggregation statements directly for MinIO execution.

     `hive.s3select-pushdown.enabled=true`

* Make communication between Trino and MinIo more reliable increasing timeout.

     `hive.s3.socket-timeout=1m`

## Resources
* [Hive Connector](https://trino.io/docs/current/connector/hive.html)
