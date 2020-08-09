Title: Memory Configuration in PrestoDB Cluster
Date: 2020-08-09
Category: PrestoDB
Cover: /extra/prestodb-logo.png

PrestoDB cluster is sensitive to memory setup. As PrestoDB is developed in Java, Java is foundation to configure it. In many cases, PrestoDB server is not started because of memory configuration. During PrestoDB server launch, the validation rules are applied to make sure that major memory settings are consistent. It does not guarantee of cluster stability and performance so spending time on initial memory setup can contribute to success of your cluster.

The article is based on CentOS 7 environment and Starburst version 332-e.1.

## Disable Linux swap

PrestoDB assumes that memory swap is disabled and is not mounted.

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
    free -h

Output

    :::text
                  total        used        free      shared  buff/cache   available
    Mem:           503G        182G        297G         10M         23G        319G
    Swap:            0B          0B          0B

## JVM configuration

The setting is defined in `jvm.config` file. It should be set up 70-80% of a server physical memory. If you are tough on resources, it can be at least 12GB less than physical memory. The more setting value is set up, the less stable system you get.

    :::ini
    -Xmx480G

## Default PrestoDB configuration

The list of memory settings are below. If any value is skipped, it is taken as the default one.

`query.max-memory-per-node`<br>
**Default value**: `JVM max memory * 0.1`<br>
**Description**: Max amount of user memory a query can use on a worker.

`query.max-total-memory-per-node`<br>
**Default value**: `JVM max memory * 0.3`<br>
**Description**: Max amount of user and system memory a query can use on a worker.

`query.max-memory`<br>
**Default value**: `20GB`<br>
**Description**: Max amount of user memory a query can use across the entire cluster.

`query.max-total-memory`<br>
**Default value**: `query.max-memory * 2`<br>
Description: Max amount of user and system memory a query can use across the entire cluster.

`memory.heap-headroom-per-node`<br>
**Default value**: `JVM max memory * 0.3`<br>
**Description**: Amount of memory set aside as headroom/buffer in the JVM heap for allocations that are not tracked by Presto.

## Basic memory setup

* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 70% = 358GB
* query.max-memory-per-node: JVM Xmx * 0.5 = 179GB
* query.max-total-memory-per-node: query.max-memory-per-node * 1.2 = 214GB
* memory.heap-headroom-per-node: 50GB
* query.max-memory: workers * query.max-memory-per-node = 1,790GB
* query.max-total-memory: workers * query.max-total-memory-per-node = 2,140GB

## Highly concurrent memory setup
* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 70% = 358GB
* query.max-memory-per-node: JVM Xmx * 0.1 = 36GB
* query.max-total-memory-per-node: query.max-memory-per-node * 1.2 = 43GB
* memory.heap-headroom-per-node = 50GB
* query.max-memory: workers * query.max-memory-per-node = 360GB
* query.max-total-memory: workers * query.max-total-memory-per-node = 430GB

## Large data skew memory setup
* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 80% = 410GB
* query.max-memory-per-node: JVM Xmx * 0.7 = 287GB
* query.max-total-memory-per-node: query.max-memory-per-node * 1.2 = 344GB
* memory.heap-headroom-per-node = 30GB
* query.max-memory: workers * query.max-memory-per-node = 2,870GB
* query.max-total-memory: workers * query.max-total-memory-per-node = 3,440GB

## Validation rule
JVM Xmx > query.max-total-memory-per-node + memory.heap-headroom-per-node

## Killer policy in case of out of memory
Out of memory (OOM) is customizable. The setting is `query.low-memory-killer.policy`. 

## Spill to disk
OOM can be mitigated if spilling memory to disk is enabled. It does not cover all possible cases. The configuration file is `config.properties`. For example,

    :::ini
    experimental.max-spill-per-node=500GB
    experimental.query-max-spill-per-node=200GB
    experimental.spill-enabled=true
    experimental.spiller-spill-path=/mnt/presto/data/spill

## Resources

* [PrestoDB Memory Management Properties](https://prestodb.io/docs/current/admin/properties.html#memory-management-properties)
* [Starburst Configuring Presto](https://docs.starburstdata.com/latest/presto-admin/installation/presto-configuration.html)
* Presto The Definitive Guide by O’Reilly
