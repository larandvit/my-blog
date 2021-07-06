Title: Apply Trino (Presto) Configuration Properties in DBeaver Connection
Date: 2021-07-06
Category: Trino(Presto), DBeaver
Cover: /extra/trino-logo.png

Trino configuration properties are used to tune Trino or change its behavior when required. Mostly times, those properties are applied in Trino configuration files. It is required a restarting of a Trino cluster to make it in effect in case if customization is needed. There is another flexible option without restarting a Trino cluster and applying those changes on-fly in a Trino DBeaver connection when a query is submitted for an execution. It can be done as a part of the Trino jdbc driver setup in DBeaver.

The article uses [Starburts](https://www.starburst.io/) open-source distribution with DBeaver 21.1.0 CE installed in Windows.

## 1. Open Trino connection in DBeaver

![DBeaver edit connection]({static}/images/apply-trino-configuration-properties-dbeaver-connection/dbeaver-edit-connection.jpg)</br></br>

## 2. Add **sessionProperties** jdbc driver parameter

![DBeaver add sessionProperties jdbc driver parameter]({static}/images/apply-trino-configuration-properties-dbeaver-connection/dbeaver-add-sessionproperties-jdbc-driver-parameter.jpg)</br></br>

## 3. Apply Trino properties

* One property

  ![DBeaver one Trino property]({static}/images/apply-trino-configuration-properties-dbeaver-connection/dbeaver-one-trino-property.jpg)</br></br>

* Multiple properties separated by semicolon (;)

  ![DBeaver multiple Trino properties]({static}/images/apply-trino-configuration-properties-dbeaver-connection/dbeaver-multiple-trino-properties.jpg)</br></br>

## Resources
* [Trino JDBC driver](https://trino.io/docs/current/installation/jdbc.html)
* [Trino Properties reference](https://trino.io/docs/current/admin/properties.html)
