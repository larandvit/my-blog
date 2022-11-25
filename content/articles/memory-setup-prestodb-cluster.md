Title: Memory Configuration in Trino Cluster
Date: 2020-08-09
Modified: 2022-11-24
Category: Trino
Cover: /extra/trino-logo.png

[Trino](https://trino.io/) cluster is sensitive to memory setup. As Trino is developed in Java, Java is foundation to configure it. In many cases, Trino server is not started because of memory configuration. During Trino server launch, the validation rules are applied to make sure that major memory settings are consistent. It does not guarantee of cluster stability and performance, so spending time on initial memory setup can contribute to success of your cluster.

The article is based on CentOS 7 environment and Trino Starburst release 393-e.2. Since release 369-e, a breaking change to the memory configuration has happened. `query.max-total-memory-per-node` setting was removed in flavor of `query.max-memory-per-node` one. 

## Disable Linux swap

Trino assumes that memory swap is disabled and is not mounted.

The current swappiness setting can be retrieved.

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

## Default Trino configuration

The list of memory settings is below. If any value is skipped, it is taken as the default one.

`query.max-memory-per-node`<br>
**Default value**: `JVM max memory * 0.3`<br>
**Description**: Max amount of user memory a query can use on a worker.

`query.max-memory`<br>
**Default value**: `20GB`<br>
**Description**: Max amount of user memory a query can use across the entire cluster.

`query.max-total-memory`<br>
**Default value**: `query.max-memory * 2`<br>
Description: Max amount of memory a query can use across the entire cluster, including revocable memory.

`memory.heap-headroom-per-node`<br>
**Default value**: `JVM max memory * 0.3`<br>
**Description**: Amount of memory set aside as headroom/buffer in the JVM heap for allocations that are not tracked by Trino.

## Basic memory setup
* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 70% = 358GB
* query.max-memory-per-node: JVM Xmx * 0.5 = 179GB
* memory.heap-headroom-per-node: 50GB
* query.max-memory: workers * query.max-memory-per-node = 1,790GB

## Highly concurrent memory setup
* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 70% = 358GB
* query.max-memory-per-node: JVM Xmx * 0.1 = 36GB
* memory.heap-headroom-per-node = 50GB
* query.max-memory: workers * query.max-memory-per-node = 360GB

## Large data memory setup
* Physical memory: 512GB
* Workers: 10
* JVM Xmx: physical memory * 80% = 410GB
* query.max-memory-per-node: JVM Xmx * 0.7 = 287GB
* memory.heap-headroom-per-node = 30GB
* query.max-memory: workers * query.max-memory-per-node = 2,870GB

## Validation rules
* JVM Xmx > query.max-memory-per-node + memory.heap-headroom-per-node
* query.max-total-memory > query.max-memory

## Killer policy in case of out of memory
Out of memory (OOM) is customizable. The settings are.

* `query.low-memory-killer.policy`
* `task.low-memory-killer.policy`
* `query.low-memory-killer.delay`

## Spill to disk
OOM can be mitigated if spilling memory to disk is enabled. It does not cover all possible cases. Performance is heavily affected during spill-to-disk. The configuration file is `config.properties`. For example,

    :::ini
    spill-enabled=true
    spiller-spill-path=/mnt/trino/data/spill
    max-spill-per-node=500GB
    query-max-spill-per-node=200GB

## Revocable memory

To reduce number of OOM failures and improve performance, the concept of revocable memory is explored. A query can be assigned more memory than outlined in memory settings when cluster is idle, but the additional memory can be revoked by the memory manager at any time. If memory is revoked, the spill to disk is the next step to solve shortage of memory.

## Resources

* [Resource management properties](https://docs.starburst.io/latest/admin/properties-resource-management.html)
* [Trino The Definitive Guide by O’Reilly](https://www.starburst.io/info/oreilly-trino-guide/)
