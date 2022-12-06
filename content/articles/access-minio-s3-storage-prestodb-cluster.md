Title: Access MinIO S3 Storage in Trino with File Metastore
Date: 2020-06-28
Modified: 2022-12-05
Category: Trino, MinIO
Cover: /extra/trino-logo.png

[Trino](https://trino.io/) accesses a variety of data sources by means of connectors. Hive connector is used to access files stored in Hadoop Distributed File System (HDFS) and S3 compatible systems. Metastore can be configured with two options: Hive or AWS Glue Data Catalog. Hive metastore setup and configuration are described in [Access MinIO S3 Storage in Trino with Hive Metastore]({filename}/articles/access-minio-s3-storage-prestodb-cluster-hive-metastore.md) article. There is another undocumented option, it is the file metastore. It was developed by [Dain Sundstrom](https://www.linkedin.com/in/dainsundstrom/) in a weekend. Metadata and data are stored in file system. As a result, the setup is very simple. It is a couple of lines in a configuration file. This setup is not aimed for production usage. The main use cases might be demo or PoC projects.

S3 compatible storages are very good alternatives to store big data. They are lightweight, easy to set up, and support. Many of those storages are open source.

[Starburst](https://www.starburst.io/) Trino distribution version 393-e and [MinIO](https://min.io/) S3 compatible storage along with file metadata configuration are used in the sample below. External tables are created to access data stored in MinIO. Internal tables are supported as well and they are stored in file system.

Hive connector property file is created in `/etc/starburst/catalog` folder. The name might be `minio.properties`. It has to have `.properties` extension name.

A set of mandatory parameters are.

    :::ini
    connector.name=hive

    hive.metastore=file
    hive.metastore.catalog.dir=file:///mnt/trino/data/minio

    hive.s3.endpoint=http://minio.sample.com:9000
    hive.s3.aws-access-key=YourAccessKey
    hive.s3.aws-secret-key=YourSercetKey
    hive.s3.path-style-access=true
    hive.s3.socket-timeout=1m

    hive.temporary-staging-directory-path=file:///mnt/trino/data/tmp
    
* `hive.metastore.catalog.dir` - the folder is shared between all nodes: a coordinator and workers. Matadata and internal tables are stored in the folder.
* `hive.temporary-staging-directory-path` - the folder is shared between all nodes: a coordinator and workers. The location of temporary staging folder that is used for write operations. Each user has a separate sub folder with the name pattern: `trino-UserName`. If the parameter is missing, `INSERT INTO` or `CREATE TABLE AS` statements will write only a portion of data into destination tables and sporadically, the error message will come up.

        :::text
        Error moving data files from file:/tmp/trino-root/6b5efc64-177e-409f-b34c-aeddbc942a92/20200612_155605_00395_stnes_45252f19-7244-46ec-86f0-88da4c300c3d to final location file:/mnt/trino/data/minio/schema_name/table_name/20200612_155605_00395_stnes_45252f19-7244-46ec-86f0-88da4c300c3d

* `hive.s3.socket-timeout` - default value is 5 seconds, and if MinIO is busy, you get the error.

        :::text
        Unable to execute HTTP request: Read time out.

The sample to test access to MinIO data.

1. Create a schema. It's a subfolder in `hive.metastore.catalog.dir` folder.

        :::sql
        CREATE SCHEMA minio.sample_schema;

2. Create a table to read data from a MinIO bucket. The file is in text format with the first line as a header. The table contains only 1 field representing a line in the file.

        :::sql
        CREATE TABLE minio.sample_schema.sample_table(
            combined_columns VARCHAR)
        WITH (
            external_location = 's3a://your_minio_bucket_name/',
            format = 'TEXTFILE',
            skip_header_line_count=1);
