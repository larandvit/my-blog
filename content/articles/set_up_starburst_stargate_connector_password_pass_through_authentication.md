Title: Set Up Starburst Stargate Connector with Password Pass-Through Authentication
Date: 2021-09-22
Modified: 2022-11-24
Category: Trino
Cover: /extra/trino-logo.png

[Starburst](https://www.starburst.io/) Stargate connector is aimed to utilize any connectors on remote Starburst clusters as local ones. It simplifies access to data applying one connector to different kinds of data. Setup can be done on one cluster and the rest of clusters are consumers of data in a uniform way. A limitation is that the connector accesses data in read only mode. As a side affect, reading data from a remote cluster does not have the same performance as reading data on the remote cluster. It impacts on performance in case of reading a lot of data from a remote cluster. If you use a remote cluster to return results of calculations with a small portion of data, for example, record count, it should serve perfectly.

Password pass-through authentication removes a lot of security concerns when credentials are stored in plain text. Also, it eliminates service accounts and as a result, users connect to a remote cluster with their own credentials.

Before setting the connector make sure that a set of requirements are followed. 

1. Clusters are run with the same version.
2. HTTP or HTTPS protocol are used between the coordinators and clusters.
3. Possession of a valid Starburst Enterprise Elite license.

The article is based on Starburst Enterprise version 306-e.2 with the RPM type installation.

## 1. Set up Password authentication type on a coordinator

It can work only with [password authentication](https://docs.starburst.io/latest/security/authentication-types.html) type. There are 3 password authentication types: password, LDAP and Salesforce.

Create /etc/starburst/password-authenticator.properties file and add an authentication type.

    :::ini
    password-authenticator.name=ldap

or 

    :::ini
    password-authenticator.name=file

or

    :::ini
    password-authenticator.name=salesforce

## 2. Allow password pass-through authentication on a coordinator

Add the setting in /etc/starburst/config.properties file. Do not combine it with any other types, for example password one.

    :::ini
    http-server.authentication.type=DELEGATED-PASSWORD

## 3. Create Stargate connector property file

The location is /etc/starburst/catalog folder. The name might be, for example, hive_remote_catalog.properties. The source remote catalog name is hive_catalog.

    :::ini
    connector.name=stargate
    connection-url=jdbc:trino://sample.com:8443/hive_catalog
    starburst.authentication.type=PASSWORD_PASS_THROUGH
    ssl.enabled=true
    ssl.truststore.path=/etc/starburst/remote_cluster.jks
    ssl.truststore.password=<remote cluster jks password>
    ssl.truststore.type=jks

## Resources
* [Starburst Stargate connector](https://docs.starburst.io/latest/connector/starburst-stargate.html)
* [Authentication types](https://docs.starburst.io/latest/security/authentication-types.html)
* [Password credential pass-through](https://docs.starburst.io/latest/security/password-passthrough.html)
