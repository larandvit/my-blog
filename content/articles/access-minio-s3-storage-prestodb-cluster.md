Title: Access MinIO S3 Storage in PrestoDB Cluster
Date: 2020-06-28
Category: PrestoDB, MinIO
Cover: /extra/prestodb-logo.png

PrestoDB is aimed to access a variety of data sources by means of connectors. Hive connector is used to access files stored in Hadoop Distributed File System (HDFS) or S3 compatible storages. Metadata can be accessible via Hive metastore. Another option to access metadata is PrestoDB. It simplifies the PrestoDB infrastructure eliminating Hive metastore. Also, in case of Hive metastore, internal tables are stored in HDFS and as a result, it requests installation of Hadoop. PrestoDB can handle both metadata and internal tables.

S3 compatible storages are very good alternatives to store big data. They are lightweight, easy to set up, and support. Many of those storages are open source.

When building an enterprise level system, it is important to set up and tune up PrestoDB to work with a coordinator and one or more workers. The setup is different from single node one.

MinIO S3 compatible storage along with hiveless metadata configuration is used in the sample below. Internal tables are stored in a shared folder.

Hive connector property file is created in `/etc/presto/catalog` folder or it can be deployed by presto-admin tool or other tools. The name might be `minio.properties`. It has to have `.properties` extension name.

A set of mandatory parameters are.

    :::config
    connector.name=hive-hadoop2

    hive.s3.path-style-access=true

    hive.metastore=file
    hive.metastore.catalog.dir=file:///mnt/presto/data/minio

    hive.s3.endpoint=http://minio.sample.com:9000
    hive.s3.aws-access-key=YourAccessKey
    hive.s3.aws-secret-key=YourSercetKey

    hive.temporary-staging-directory-path=file:///mnt/presto/data/tmp

    hive.s3.socket-timeout=1m
    

* `hive.metastore.catalog.dir` - the folder is shared between all nodes: a coordinator and workers. Internal tables are stored in the folder.
* `hive.temporary-staging-directory-path` - the folder is shared between all nodes: a coordinator and workers. The location of temporary staging folder that is used for write operations. Each user has a separate sub folder with the name pattern: `presto-UserName`. If the parameter is missing, `INSERT INTO` or `CREATE TABLE AS` statements will write only a portion of data into destination tables and sporadically, the error message will come up.

        :::text
        Error moving data files from file:/tmp/presto-root/6b5efc64-177e-409f-b34c-aeddbc942a92/20200612_155605_00395_stnes_45252f19-7244-46ec-86f0-88da4c300c3d to final location file:/mnt/presto/data/minio/schema_name/table_name/20200612_155605_00395_stnes_45252f19-7244-46ec-86f0-88da4c300c3d

* `hive.s3.socket-timeout` - default value is 5 seconds and if MinIO is busy, you get the error.

        :::text
        Unable to execute HTTP request: Read time out.

The sample to test access to MinIO data.

    :::sql
    CREATE SCHEMA minio.sample_schema;

    CREATE TABLE sample_table(
       combined_columns VARCHAR)
    WITH (
      external_location = 's3a://your_minio_bucket_name/',
      format = 'TEXTFILE',
      skip_header_line_count=1);

