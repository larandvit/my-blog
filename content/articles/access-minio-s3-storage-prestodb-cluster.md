Title: Access MinIO S3 Storage in Presto with File Metastore
Date: 2020-06-28
Modified: 2020-10-21
Category: Presto, MinIO
Cover: /extra/prestodb-logo.png

[Presto](https://prestodb.io/) accesses a variety of data sources by means of connectors. Hive connector is used to access files stored in Hadoop Distributed File System (HDFS) or S3 compatible systems. Metastore can be configured with two options: Hive or AWS Glue Data Catalog. Hive metastore information can be find in [Access MinIO S3 Storage in Presto with Hive Metastore]({filename}/articles/access-minio-s3-storage-prestodb-cluster-hive-metastore.md) article  There is another undocumented option, it is the file metastore. It was developed by [Dain Sundstrom](https://www.linkedin.com/in/dainsundstrom/) in a weekend. Metadata and data are stored in file system. As a result, the setup is very simple. It is a couple of lines in a configuration file. This setup is not aimed for production usage. The main use cases might be demo or PoC projects.

S3 compatible storages are very good alternatives to store big data. They are lightweight, easy to set up, and support. Many of those storages are open source.

When building an enterprise level system, it is important to set up and tune up Presto to work with a coordinator and one or more workers. The setup is different from single node one.

MinIO S3 compatible storage along with file metadata configuration is used in the sample below. Internal tables are stored in a shared folder.

Hive connector property file is created in `/etc/presto/catalog` folder or it can be deployed by **presto-admin** tool or other tools. The name might be `minio.properties`. It has to have `.properties` extension name.

A set of mandatory parameters are.

    :::ini
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


