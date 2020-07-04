Title: PrestoDB Installation and Setup Pitfalls
Date: 2020-07-03
Category: PrestoDB
Cover: /extra/prestodb-logo.png

Like other installations and setups, PrestoDB one can contain steps which cause difficulties. How many times you were stuck with something? In mostly cases, it was a trivial issue but you spent countless time to solve it. It's better to have a cheat sheet for discovering those issues before encountering them. The list of pitfalls is based on [Starburst open source distribution](https://www.starburstdata.com/starburst-presto-sql/) version 332-e.1 and CentOS 7.

## Installation steps for cluster setup

To speed up the process of installation, use Presto Admin tool. It can install java, Presto server and other tools on each node in your cluster including a coordinator. Also, it can deploy setup files to your cluster nodes. The tool is aimed to be run on any Linux computer with `java 8` installed. When you run the tool, you need to be granted sudo access to cluster nodes. Root user is not requested.

1. Download Presto Admin.
2. Download Presto server RPM file.
3. Install Presto Admin.
4. Install `java 11` on each node if requested.
5. Create `config.properties` Presto Admin file.
6. Create coordinator and workers setup files.
7. Install Presto server on each node with Presto Admin.
8. Start your cluster with Presto Admin.
9. Validate your cluster with Starburst Cluster Overview. The address is `http://<coordinator node name>:<coordinator port>/ui`, for example, `http://sample:8080/ui`.
10. Create connector files.
11. Add connectors to Presto cluster with Presto Admin.
12. Restart your cluster with Presto Admin.

## Java 11 installation

[OpenJDK 11](https://openjdk.java.net/projects/jdk/11/) can be used. Java 11 does not have JRE dedicated folder. 

    :::bash
    sudo yum install java-11-openjdk-devel

OpenJDK JRE folder is `/usr/lib/jvm/jre-11`. It points to the same location as JDK one.

The JRE folder is used in Presto Admin `config.properties` file located in `/PrestoDBMaintainer/.prestoadmin`. Based on the setting, `env.sh` file in `/etc/presto` folder is created.

config.properties 

    :::json
    {
      "java_home":"/usr/lib/jvm/jre-11"
    }

env.sh

    :::bash
    JAVA_HOME=/usr/lib/jvm/jre-11

## Coordinator name format in `config.properties` Presto file for coordinator and workers

The correct format is `http://<coordinator node name>:<coordinator port>`, for example, `discovery.uri=http://prestoserver:8080`. Do not use fully qualified domain name (FQDN), for example, `discovery.uri=http://prestoserver.com:8080`.

## Folder format in `config.properties` Presto file

Do not specify `file` prefix, for example, `experimental.spiller-spill-path=/mnt/presto/data/data_spill'.

## Folder format in Hive connector file

Specify `file` prefix, for example, 'hive.metastore.catalog.dir=file:///mnt/presto/data/hive_connector'.
