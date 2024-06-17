Title: Migration to Trino Iceberg Table Format
Date: 2024-06-16
Category: Trino
Cover: /extra/trino-logo.png

[Trino](https://trino.io/) added Iceberg connector some years ago and since that the connector has been getting more popular in Trino community. The major competitor for Iceberg connector is Hive one. Those table formats store data similar. The advantage of Iceberg connector is that it stores metadata along with data. It brings functionality which lacking in Hive connector. As a result, a task migrating of Hive tables to Iceberg ones is considered by many Trino users. 

There are 2 ways to proceed with migration. 

1. Shadow migration.
2. In-place migration.

Both methods request time and efforts to complete. The shadow migration is based on rewriting all tables in Iceberg format whereas in-place migration adds metadata to existing Hive data. Even with in-place migration, it will be downtime for converting existing data.

In this article, it will be described the third approach to migrate data. This method is not covered directly in Trino documentation. I call it as natural evolution migration. The name reflects a process of migration when downtime is eliminated and gradually Hive tables will be replaced with Iceberg ones.

The migration sample is developed in [Starburst](https://starburst.io) open-source distribution with S3 compatible storage.

## Prerequisites

* Current Hive catalog uses Hive metastore.
* Current Hive catalog and Iceberg catalog share the same Hive metastore.
    
##1. Rename current Hive catalog

This step helps eliminated breaking of scripts. 

If your current Hive catalog name is sample\_catalog, rename to sample_catalog_hive.

##2. Create Iceberg catalog

The name of the Iceberg catalog is the same as the current Hive catalog, for example, sample_catalog.

Add a setting to redirect requests to the Hive catalog if tables are in Hive table format.

    :::ini
    iceberg.hive-catalog-name=sample_catalog_hive

##3. Find tables with unsupported data types

There are 3 unsupported data types.

1. tinyint
2. smallint
3. char

Run the query below. Be patient as it might take dozes of minutes to complete query. 

    :::sql
    SELECT 
        c.table_schema,
        c.table_name,
        c.column_name,
        t.table_type,
        c.data_type,
        regexp_extract(c.data_type, '([a-zA-Z]+)\)*.*',1) AS data_type_name
    FROM 
        sample_catalog_hive.information_schema.columns c
    JOIN 
        sample_catalog_hive.information_schema.tables t ON
            c.table_catalog=t.table_catalog
            AND c.table_schema=t.table_schema
            AND c.table_name=t.table_name
    WHERE 
        regexp_extract(c.data_type, '([a-zA-Z]+)\)*.*',1) IN ('tinyint', 'char', 'smallint');

##4. Make changes to scripts to replace unsupported data types

The mapping to replace unsupported data types is.

* tinyint, smallint -> int
* char -> varchar

##5. Run your scripts in Iceberg catalog

Use cases might be.

* DROP TABLE statement. Hive table is deleted and a new Iceberg table can be created after.
* SELECT statement. Iceberg catalog redirects a request to Hive catalog. 
* INSERT INTO statement. Iceberg catalog redirects a request to Hive catalog. The table stays in Hive table format.
* CREATE TABLE statement. Iceberg table is created.
* MERGE statement. Iceberg catalog redirects a request to Hive catalog. The table stays in Hive table format.

## Consideration for migration

* Iceberg catalog does not support external (unmanaged) tables.
* Evaluate if efforts of replacing of unsupported data types less than benefits of using Iceberg table format.

## Resources
* [Iceberg Connector](https://trino.io/docs/current/connector/iceberg.html#connector-iceberg--page-root)
* [Hive to Iceberg - To Migrate, or Not to Migrate](https://www.starburst.io/resources/hive-to-iceberg-to-migrate-or-not-to-migrate/)
