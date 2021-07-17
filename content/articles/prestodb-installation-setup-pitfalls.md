Title: Trino(Presto) Installation and Setup Pitfalls
Date: 2020-07-03
Modified: 2021-07-16
Category: Trino(Presto)
Cover: /extra/trino-logo.png

Like other installations and setups, [Trino](https://trino.io/) formerly PrestoSQL can contain steps which cause difficulties. How many times you were stuck with something? In mostly cases, it was a trivial issue but you spent countless time to solve it. It's better to have a cheat sheet for discovering those issues before encountering them. The list of pitfalls is based on [Starburst](https://www.starburst.io/) open-source distribution.

## History

[Trino](https://trino.io/) formerly PrestoSQL was originated in 2012 year as [PrestoDB](https://prestodb.io/) open-source project in Facebook. PrestoSQL was started in 2019 by the PrestoDB founders. Facebook forced to rebrand PrestoSQL into Trino in 2020. One of the successful commercial distribution based on Trino is [Starburst](https://www.starburst.io/). Starburst includes both open-source and commercial products.

## Disable swap on each node
Trino assumes that swap is not used. Swap can dramatically impact on performance and stability of a Trino cluster. If swap is on, memory consumption will be close to 100% and, as a result, Trino cluster will be slow and many queries will fail.

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

## Folder format in configuration files

Do not specify `file` prefix in `config.properties` file, for example, `spiller-spill-path=/mnt/trino/data/data_spill'.

## Folder format in Hive connector file

Specify `file` prefix, for example, 'hive.metastore.catalog.dir=file:///mnt/trino/data/hive_connector'.

## SQL Server connector overwhelms SQL Server

When data is written to SQL Server, Trino tries to do it as fast as possible. It will utilize all workers to push data to SQL Server. As a result, it opens a lot of connections at least one per worker and SQL Server can crash. Wideness of an exported table impacts on it as well. The more columns is in your table, the more chances to encounter the issue can be. Also, the number of records in a destination table contributes to the issue.

The error message is 

    :::text
    io.prestosql.spi.PrestoException: There is insufficient system memory in resource pool 'default' to run this query. 
       at io.prestosql.plugin.jdbc.JdbcPageSink.appendPage(JdbcPageSink.java:117)
       at io.prestosql.operator.TableWriterOperator.addInput(TableWriterOperator.java:257)
       at io.prestosql.operator.Driver.processInternal(Driver.java:384)
       at io.prestosql.operator.Driver.lambda$processFor$8(Driver.java:283)
       at io.prestosql.operator.Driver.tryWithLock(Driver.java:675)
       at io.prestosql.operator.Driver.processFor(Driver.java:276)
       at io.prestosql.execution.SqlTaskExecution$DriverSplitRunner.processFor(SqlTaskExecution.java:1076)
       at io.prestosql.execution.executor.PrioritizedSplitRunner.process(PrioritizedSplitRunner.java:163)
       at io.prestosql.execution.executor.TaskExecutor$TaskRunner.run(TaskExecutor.java:484)
       at io.prestosql.$gen.Presto_348_e____20210219_123137_2.run(Unknown Source)
       at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1128)
       at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:628)
       at java.base/java.lang.Thread.run(Thread.java:834)
    Caused by: com.microsoft.sqlserver.jdbc.SQLServerException: There is insufficient system memory in resource pool 'default' to run this query.
       at com.microsoft.sqlserver.jdbc.SQLServerException.makeFromDatabaseError(SQLServerException.java:254)
       at com.microsoft.sqlserver.jdbc.SQLServerStatement.getNextResult(SQLServerStatement.java:1608)
       at com.microsoft.sqlserver.jdbc.SQLServerPreparedStatement.doExecutePreparedStatementBatch(SQLServerPreparedStatement.java:2766)
       at com.microsoft.sqlserver.jdbc.SQLServerPreparedStatement$PrepStmtBatchExecCmd.doExecute(SQLServerPreparedStatement.java:2641)
       at com.microsoft.sqlserver.jdbc.TDSCommand.execute(IOBuffer.java:7240)
       at com.microsoft.sqlserver.jdbc.SQLServerConnection.executeCommand(SQLServerConnection.java:2869)
       at com.microsoft.sqlserver.jdbc.SQLServerStatement.executeCommand(SQLServerStatement.java:243)
       at com.microsoft.sqlserver.jdbc.SQLServerStatement.executeStatement(SQLServerStatement.java:218)
       at com.microsoft.sqlserver.jdbc.SQLServerPreparedStatement.executeBatch(SQLServerPreparedStatement.java:2056)
       at io.prestosql.plugin.jdbc.JdbcPageSink.appendPage(JdbcPageSink.java:109)
       ... 12 more

To solve the issue, RAM of SQL Server should be pumped up. You can try to increase SQL Server memory until the issue is gone. For example, if you export a table with 100 columns and your record count is some hundred million records, RAM can be set up to 96GB with 90GB dedicated to SQL Server.
