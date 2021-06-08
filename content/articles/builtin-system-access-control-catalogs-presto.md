Title: Built-in System Access Control to Catalogs in Presto
Date: 2021-03-04
Category: Trino(Presto)
Cover: /extra/prestodb-logo.png

[Presto](https://trino.io/) secures access to catalogs with built-in system. This article is not aimed to replace the Presto documentation which provides high level of overview with following details on the implementation of build-in system access control. As any documentation, it does not cover all topics and there are some missing pieces and features. The samples are based on [Trino](https://trino.io/) open-source distribution former known as Presto SQL. 

The default global access for mostly types of catalogs is to create tables and insert data. It is applicable, for example, to MySql, PostgreSQL, SQL Server, and other connectors.

PostgreSQL connector with default access.

    :::ini
    connector.name=postgresql
    connection-url=jdbc:postgresql://sample.com:5432/database_name
    connection-user=user_name
    connection-password=user_password

Adding delete table permission is done with `allow-drop-table` property.

    :::ini
    connector.name=postgresql
    connection-url=jdbc:postgresql://sample.com:5432/database_name
    connection-user=user_name
    connection-password=user_password

    allow-drop-table=true

If a connector is supposed to be read-only, a set of files are requested to implement it. The first file is **access-control.properties** and the location is **/etc/presto** folder. This file turns on the file-based system access control. The second file specifies access control rules and the file format is JSON. The file name and location are customizable.

* access-control.properties

        :::ini
        access-control.name=file
        security.config-file=/etc/presto/global_rules.json

* global_rules.json

        :::json
        {
          "catalogs": [
            {
              "catalog": "(finance|warehouse|system)",
              "allow": "read-only"
            },
            {
              "allow": "all"
            }
          ]
        }

The read-only permission is applied only to finance, warehouse, and system catalogs. The rest of catalogs are not affected.

The beneficial feature of built-in system access control is that a cluster restart is not required if settings are changed. Moreover, Presto can be instructed how often those security settings should be refreshed with `security.refresh-period` property in **access-control.properties** file.

    :::ini
    access-control.name=file
    security.config-file=/etc/presto/global_rules.json

    ecurity.refresh-period

Some connectors have its own security settings, for example, Hive. 

    :::ini
    hive.allow-add-column=true
    hive.allow-drop-column=true
    hive.allow-drop-table=true
    hive.allow-rename-column=true
    hive.allow-rename-table=true

## Resources

* [Built-in system access control](https://trino.io/docs/current/security/built-in-system-access-control.html)
* [File based system access control](https://trino.io/docs/current/security/file-system-access-control.html)
