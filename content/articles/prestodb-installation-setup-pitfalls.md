Title: Presto Installation and Setup Pitfalls
Date: 2020-07-03
Category: Presto
Cover: /extra/prestodb-logo.png

Like other installations and setups, [Presto](https://prestodb.io/) one can contain steps which cause difficulties. How many times you were stuck with something? In mostly cases, it was a trivial issue but you spent countless time to solve it. It's better to have a cheat sheet for discovering those issues before encountering them. The list of pitfalls is based on [Starburst open source distribution](https://www.starburstdata.com/starburst-presto-sql/) version 332-e.1 and CentOS 7.

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

## Disable swap on each node
Presto assumes that swap is not used. Swap can dramatically impact on performance and stability of a Presto cluster. If swap is on, memory consumption will be close to 100% and, as a result, Presto cluster will be slow and many queries will fail.

The typical error messages are.

Error type 1.

    :::text
    io.prestosql.spi.PrestoException: Query 20200720_132906_00038_4smph has not been accessed since 2020-07-20T09:42:25.080-04:00: currentTime 2020-07-20T09:52:25.447-04:00


Error type 2.

    :::text
    io.prestosql.spi.PrestoTransportException: Encountered too many errors talking to a worker node. The node may have crashed or be under too much load. This is probably a transient issue, so please retry your query in a few minutes.

The current swappiness setting can be received.

    :::bash
    cat /proc/sys/vm/swappiness

Turn off swappiness temporary.

    :::bash
    sudo sysctl vm.swappiness=0

Turn off swappiness permanently changing `vm.swappiness=0` setting in the file below.

    :::bash
    sudo nano /etc/sysctl.conf

Swap memory information.

    :::bash
    free -m

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

## Coordinator address in configuration files

The correct format in `config.properties` file is `http://<coordinator node name>:<coordinator port>`, for example, `discovery.uri=http://prestoserver:8080`. Do not use fully qualified domain name (FQDN), for example, `discovery.uri=http://prestoserver.com:8080`. It is applicable to both coordinator and workers.

## Folder format in configuration files

Do not specify `file` prefix in `config.properties` file, for example, `experimental.spiller-spill-path=/mnt/presto/data/data_spill'.

## Folder format in Hive connector file

Specify `file` prefix, for example, 'hive.metastore.catalog.dir=file:///mnt/presto/data/hive_connector'.
