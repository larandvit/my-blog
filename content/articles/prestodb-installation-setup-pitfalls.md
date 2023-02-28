Title: Trino Installation and Setup Pitfalls
Date: 2020-07-03
Modified: 2023-02-27
Category: Trino
Cover: /extra/trino-logo.png

Like other installations and setups, [Trino](https://trino.io/) formerly PrestoSQL can contain steps which cause difficulties. How many times you were stuck with something? In mostly cases, it was a trivial issue but you spent countless time to solve it. It's better to have a cheat sheet for discovering those issues before encountering them. The list of pitfalls is based on [Starburst](https://www.starburst.io/) open-source distribution.

## History

[Trino](https://trino.io/) formerly PrestoSQL was originated in 2012 year as [PrestoDB](https://prestodb.io/) open-source project in Facebook. PrestoSQL was started in 2019 by the PrestoDB founders. Facebook forced to rebrand PrestoSQL into Trino in 2020. One of the successful commercial distributions based on Trino is [Starburst](https://www.starburst.io/). Starburst includes both open-source and commercial products.

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

## SQL Server connector overwhelms SQL Server

When data is written to SQL Server, Trino tries to do it as fast as possible. It will utilize all workers to push data to SQL Server. As a result, it opens a lot of connections at least one per worker and SQL Server can crash. Wideness of an exported table impacts on it as well. The more columns are in your table, the more chances to encounter the issue can be. Also, the number of records in a destination table contributes to the issue.

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

## Permissions for /tmp folder if Hive connector used

`/tmp` folder has to have the permissions in case of using Hive connector.

    :::bash
    ls -ld /tmp
    drwxrwxrwx. 13 root root 4096 Jul 30 15:08 /tmp

Trino copies Hive connector files in `/tmp` folder during Trino server starup.

The location of the temporary folder can be changes with `-Djava.io.tmpdir` property in jvm.config file.

If `/tmp` folder is not granted enough permissions, Trino server will not start.

server.log error message when Hive connector is being loaded.

    :::text
    2021-07-30T14:09:22.395-0400 INFO main io.trino.metadata.StaticCatalogStore -- Loading catalog /etc/starburst/catalog/hive_connector.properties --
    ...
    2021-07-30T14:09:23.815-0400	ERROR	main	io.trino.server.Server	null
    java.lang.ExceptionInInitializerError
    	at io.trino.plugin.hive.HdfsEnvironment$$FastClassByGuice$$e99ee3bd.newInstance(<generated>)
    	at com.google.inject.internal.DefaultConstructionProxyFactory$FastClassProxy.newInstance(DefaultConstructionProxyFactory.java:89)
    	at com.google.inject.internal.ConstructorInjector.provision(ConstructorInjector.java:114)
    	at com.google.inject.internal.ConstructorInjector.access$000(ConstructorInjector.java:32)
    	at com.google.inject.internal.ConstructorInjector$1.call(ConstructorInjector.java:98)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:112)
    	at io.airlift.bootstrap.LifeCycleModule.provision(LifeCycleModule.java:54)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:120)
    	at com.google.inject.internal.ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:66)
    	at com.google.inject.internal.ConstructorInjector.construct(ConstructorInjector.java:93)
    	at com.google.inject.internal.ConstructorBindingImpl$Factory.get(ConstructorBindingImpl.java:306)
    	at com.google.inject.internal.ProviderToInternalFactoryAdapter.get(ProviderToInternalFactoryAdapter.java:40)
    	at com.google.inject.internal.SingletonScope$1.get(SingletonScope.java:168)
    	at com.google.inject.internal.InternalFactoryToProviderAdapter.get(InternalFactoryToProviderAdapter.java:39)
    	at com.google.inject.internal.SingleParameterInjector.inject(SingleParameterInjector.java:42)
    	at com.google.inject.internal.SingleParameterInjector.getAll(SingleParameterInjector.java:65)
    	at com.google.inject.internal.ConstructorInjector.provision(ConstructorInjector.java:113)
    	at com.google.inject.internal.ConstructorInjector.access$000(ConstructorInjector.java:32)
    	at com.google.inject.internal.ConstructorInjector$1.call(ConstructorInjector.java:98)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:112)
    	at io.airlift.bootstrap.LifeCycleModule.provision(LifeCycleModule.java:54)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:120)
    	at com.google.inject.internal.ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:66)
    	at com.google.inject.internal.ConstructorInjector.construct(ConstructorInjector.java:93)
    	at com.google.inject.internal.ConstructorBindingImpl$Factory.get(ConstructorBindingImpl.java:306)
    	at com.google.inject.internal.FactoryProxy.get(FactoryProxy.java:62)
    	at com.google.inject.internal.ProviderToInternalFactoryAdapter.get(ProviderToInternalFactoryAdapter.java:40)
    	at com.google.inject.internal.SingletonScope$1.get(SingletonScope.java:168)
    	at com.google.inject.internal.InternalFactoryToProviderAdapter.get(InternalFactoryToProviderAdapter.java:39)
    	at com.google.inject.internal.InternalInjectorCreator.loadEagerSingletons(InternalInjectorCreator.java:213)
    	at com.google.inject.internal.InternalInjectorCreator.injectDynamically(InternalInjectorCreator.java:184)
    	at com.google.inject.internal.InternalInjectorCreator.build(InternalInjectorCreator.java:111)
    	at com.google.inject.Guice.createInjector(Guice.java:87)
    	at io.airlift.bootstrap.Bootstrap.initialize(Bootstrap.java:276)
    	at io.trino.plugin.hive.InternalHiveConnectorFactory.createConnector(InternalHiveConnectorFactory.java:117)
    	at io.trino.plugin.hive.InternalHiveConnectorFactory.createConnector(InternalHiveConnectorFactory.java:77)
    	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
    	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
    	at java.base/java.lang.reflect.Method.invoke(Method.java:566)
    	at io.trino.plugin.hive.HiveConnectorFactory.create(HiveConnectorFactory.java:63)
    	at io.trino.connector.ConnectorManager.createConnector(ConnectorManager.java:359)
    	at io.trino.connector.ConnectorManager.createCatalog(ConnectorManager.java:216)
    	at io.trino.connector.ConnectorManager.createCatalog(ConnectorManager.java:208)
    	at io.trino.connector.ConnectorManager.createCatalog(ConnectorManager.java:194)
    	at io.trino.metadata.StaticCatalogStore.loadCatalog(StaticCatalogStore.java:88)
    	at io.trino.metadata.StaticCatalogStore.loadCatalogs(StaticCatalogStore.java:68)
    	at io.trino.server.Server.doStart(Server.java:119)
    	at io.trino.server.Server.lambda$start$0(Server.java:73)
    	at io.trino.$gen.Trino_354_e____20210730_180904_1.run(Unknown Source)
    	at io.trino.server.Server.start(Server.java:73)
    	at com.starburstdata.presto.StarburstTrinoServer.main(StarburstTrinoServer.java:50)
    Caused by: java.lang.RuntimeException: failed to load Hadoop native library
    	at io.trino.hadoop.HadoopNative.requireHadoopNative(HadoopNative.java:59)
    	at io.trino.plugin.hive.HdfsEnvironment.<clinit>(HdfsEnvironment.java:39)
    	... 52 more
    Caused by: java.io.IOException: Permission denied
    	at java.base/java.io.UnixFileSystem.createFileExclusively(Native Method)
    	at java.base/java.io.File.createTempFile(File.java:2129)
    	at java.base/java.io.File.createTempFile(File.java:2175)
    	at io.trino.hadoop.HadoopNative.loadLibrary(HadoopNative.java:92)
    	at io.trino.hadoop.HadoopNative.requireHadoopNative(HadoopNative.java:47)
    	... 53 more

## Error "FATAL: remaining connection slots are reserved for non-replication superuser connections"

This issue might be caused by Event Logger with Postgres database as a backend when a Trino cluster is not stopped during shutting down Linux computers. In that case, Postgres connections are not closed and after Trino start, enough connections can't be allocated.

To solve it, restart your Trino cluster. It will release Postgres connections after stopping the cluster.

The issue was encountered in Starburst Enterprise edition 360-e.2.

The error message found in the server log.

    :::text
    2021-10-02T10:47:21.873-0400	INFO	main	io.trino.eventlistener.EventListenerManager	-- Loading event listener etc/event-listener.properties --
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	PROPERTY                           DEFAULT     RUNTIME                                                                                DESCRIPTION
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.connection-pool.idle-timeout  10.00m      10.00m                                                                                 Maximum amount of time a connection can sit idle in the pool
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.password                      [REDACTED]  [REDACTED]                                                                             Password of the user connecting to the database
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.url                           ----        jdbc:postgresql://sample.com:5432/event_logger                                        URL of the database; for MySQL, include sessionVariables=sql_mode=ANSI
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.user                          ----        sample_user                                                                            User connecting to the database
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.connection-pool.max-size      10          10                                                                                     Maximum number of connections in the pool
    2021-10-02T10:47:22.424-0400	INFO	main	Bootstrap	jdbc.connection-pool.min-size      1           1                                                                                      Minimum number of connections in the pool
    2021-10-02T10:47:22.500-0400	INFO	main	com.zaxxer.hikari.HikariDataSource	HikariPool-1 - Starting...
    2021-10-02T10:47:22.583-0400	ERROR	main	com.zaxxer.hikari.pool.HikariPool	HikariPool-1 - Exception during pool initialization.
    org.postgresql.util.PSQLException: FATAL: remaining connection slots are reserved for non-replication superuser connections
    	at org.postgresql.Driver$ConnectThread.getResult(Driver.java:410)
    	at org.postgresql.Driver.connect(Driver.java:268)
    	at java.sql/java.sql.DriverManager.getConnection(DriverManager.java:677)
    	at java.sql/java.sql.DriverManager.getConnection(DriverManager.java:228)
    	at org.postgresql.ds.common.BaseDataSource.getConnection(BaseDataSource.java:98)
    	at org.postgresql.ds.common.BaseDataSource.getConnection(BaseDataSource.java:83)
    	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:353)
    	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:201)
    	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:473)
    	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:562)
    	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115)
    	at com.zaxxer.hikari.HikariDataSource.<init>(HikariDataSource.java:81)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule.createDatasource(PersistenceModule.java:77)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule$$FastClassByGuice$$670968.GUICE$TRAMPOLINE(<generated>)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule$$FastClassByGuice$$670968.apply(<generated>)
    	at com.google.inject.internal.ProviderMethod$FastClassProviderMethod.doProvision(ProviderMethod.java:260)
    	at com.google.inject.internal.ProviderMethod.doProvision(ProviderMethod.java:171)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.provision(InternalProviderInstanceBindingImpl.java:185)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.access$300(InternalProviderInstanceBindingImpl.java:139)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory$1.call(InternalProviderInstanceBindingImpl.java:169)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:109)
    	at io.airlift.bootstrap.LifeCycleModule.provision(LifeCycleModule.java:54)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:117)
    	at com.google.inject.internal.ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:66)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.get(InternalProviderInstanceBindingImpl.java:164)
    	at com.google.inject.internal.ProviderToInternalFactoryAdapter.get(ProviderToInternalFactoryAdapter.java:40)
    	at com.google.inject.internal.SingletonScope$1.get(SingletonScope.java:169)
    	at com.google.inject.internal.InternalFactoryToProviderAdapter.get(InternalFactoryToProviderAdapter.java:45)
    	at com.google.inject.internal.InternalInjectorCreator.loadEagerSingletons(InternalInjectorCreator.java:213)
    	at com.google.inject.internal.InternalInjectorCreator.injectDynamically(InternalInjectorCreator.java:186)
    	at com.google.inject.internal.InternalInjectorCreator.build(InternalInjectorCreator.java:113)
    	at com.google.inject.Guice.createInjector(Guice.java:87)
    	at io.airlift.bootstrap.Bootstrap.initialize(Bootstrap.java:275)
    	at com.starburstdata.presto.eventlogger.QueryLoggerEventListenerFactory.create(QueryLoggerEventListenerFactory.java:40)
    	at com.starburstdata.presto.license.LicenceCheckingEventListenerFactory.create(LicenceCheckingEventListenerFactory.java:50)
    	at io.trino.eventlistener.EventListenerManager.createEventListener(EventListenerManager.java:117)
    	at java.base/java.util.stream.ReferencePipeline$3$1.accept(ReferencePipeline.java:195)
    	at java.base/java.util.Collections$2.tryAdvance(Collections.java:4747)
    	at java.base/java.util.Collections$2.forEachRemaining(Collections.java:4755)
    	at java.base/java.util.stream.AbstractPipeline.copyInto(AbstractPipeline.java:484)
    	at java.base/java.util.stream.AbstractPipeline.wrapAndCopyInto(AbstractPipeline.java:474)
    	at java.base/java.util.stream.ReduceOps$ReduceOp.evaluateSequential(ReduceOps.java:913)
    	at java.base/java.util.stream.AbstractPipeline.evaluate(AbstractPipeline.java:234)
    	at java.base/java.util.stream.ReferencePipeline.collect(ReferencePipeline.java:578)
    	at io.trino.eventlistener.EventListenerManager.configuredEventListeners(EventListenerManager.java:100)
    	at io.trino.eventlistener.EventListenerManager.loadEventListeners(EventListenerManager.java:85)
    	at io.trino.server.Server.doStart(Server.java:134)
    	at io.trino.server.Server.lambda$start$0(Server.java:77)
    	at io.trino.$gen.Trino_360_e_2____20211002_144649_1.run(Unknown Source)
    	at io.trino.server.Server.start(Server.java:77)
    	at com.starburstdata.presto.StarburstTrinoServer.main(StarburstTrinoServer.java:40)
    
    
    2021-10-02T10:47:22.584-0400	ERROR	main	com.starburstdata.presto.eventlogger.api.db.PersistenceModule	Failed to create connection pool; query event logger will be disabled
    com.zaxxer.hikari.pool.HikariPool$PoolInitializationException: Failed to initialize pool: FATAL: remaining connection slots are reserved for non-replication superuser connections
    	at com.zaxxer.hikari.pool.HikariPool.throwPoolInitializationException(HikariPool.java:597)
    	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:576)
    	at com.zaxxer.hikari.pool.HikariPool.<init>(HikariPool.java:115)
    	at com.zaxxer.hikari.HikariDataSource.<init>(HikariDataSource.java:81)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule.createDatasource(PersistenceModule.java:77)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule$$FastClassByGuice$$670968.GUICE$TRAMPOLINE(<generated>)
    	at com.starburstdata.presto.eventlogger.api.db.PersistenceModule$$FastClassByGuice$$670968.apply(<generated>)
    	at com.google.inject.internal.ProviderMethod$FastClassProviderMethod.doProvision(ProviderMethod.java:260)
    	at com.google.inject.internal.ProviderMethod.doProvision(ProviderMethod.java:171)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.provision(InternalProviderInstanceBindingImpl.java:185)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.access$300(InternalProviderInstanceBindingImpl.java:139)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory$1.call(InternalProviderInstanceBindingImpl.java:169)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:109)
    	at io.airlift.bootstrap.LifeCycleModule.provision(LifeCycleModule.java:54)
    	at com.google.inject.internal.ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:117)
    	at com.google.inject.internal.ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:66)
    	at com.google.inject.internal.InternalProviderInstanceBindingImpl$CyclicFactory.get(InternalProviderInstanceBindingImpl.java:164)
    	at com.google.inject.internal.ProviderToInternalFactoryAdapter.get(ProviderToInternalFactoryAdapter.java:40)
    	at com.google.inject.internal.SingletonScope$1.get(SingletonScope.java:169)
    	at com.google.inject.internal.InternalFactoryToProviderAdapter.get(InternalFactoryToProviderAdapter.java:45)
    	at com.google.inject.internal.InternalInjectorCreator.loadEagerSingletons(InternalInjectorCreator.java:213)
    	at com.google.inject.internal.InternalInjectorCreator.injectDynamically(InternalInjectorCreator.java:186)
    	at com.google.inject.internal.InternalInjectorCreator.build(InternalInjectorCreator.java:113)
    	at com.google.inject.Guice.createInjector(Guice.java:87)
    	at io.airlift.bootstrap.Bootstrap.initialize(Bootstrap.java:275)
    	at com.starburstdata.presto.eventlogger.QueryLoggerEventListenerFactory.create(QueryLoggerEventListenerFactory.java:40)
    	at com.starburstdata.presto.license.LicenceCheckingEventListenerFactory.create(LicenceCheckingEventListenerFactory.java:50)
    	at io.trino.eventlistener.EventListenerManager.createEventListener(EventListenerManager.java:117)
    	at java.base/java.util.stream.ReferencePipeline$3$1.accept(ReferencePipeline.java:195)
    	at java.base/java.util.Collections$2.tryAdvance(Collections.java:4747)
    	at java.base/java.util.Collections$2.forEachRemaining(Collections.java:4755)
    	at java.base/java.util.stream.AbstractPipeline.copyInto(AbstractPipeline.java:484)
    	at java.base/java.util.stream.AbstractPipeline.wrapAndCopyInto(AbstractPipeline.java:474)
    	at java.base/java.util.stream.ReduceOps$ReduceOp.evaluateSequential(ReduceOps.java:913)
    	at java.base/java.util.stream.AbstractPipeline.evaluate(AbstractPipeline.java:234)
    	at java.base/java.util.stream.ReferencePipeline.collect(ReferencePipeline.java:578)
    	at io.trino.eventlistener.EventListenerManager.configuredEventListeners(EventListenerManager.java:100)
    	at io.trino.eventlistener.EventListenerManager.loadEventListeners(EventListenerManager.java:85)
    	at io.trino.server.Server.doStart(Server.java:134)
    	at io.trino.server.Server.lambda$start$0(Server.java:77)
    	at io.trino.$gen.Trino_360_e_2____20211002_144649_1.run(Unknown Source)
    	at io.trino.server.Server.start(Server.java:77)
    	at com.starburstdata.presto.StarburstTrinoServer.main(StarburstTrinoServer.java:40)
    Caused by: org.postgresql.util.PSQLException: FATAL: remaining connection slots are reserved for non-replication superuser connections
    	at org.postgresql.Driver$ConnectThread.getResult(Driver.java:410)
    	at org.postgresql.Driver.connect(Driver.java:268)
    	at java.sql/java.sql.DriverManager.getConnection(DriverManager.java:677)
    	at java.sql/java.sql.DriverManager.getConnection(DriverManager.java:228)
    	at org.postgresql.ds.common.BaseDataSource.getConnection(BaseDataSource.java:98)
    	at org.postgresql.ds.common.BaseDataSource.getConnection(BaseDataSource.java:83)
    	at com.zaxxer.hikari.pool.PoolBase.newConnection(PoolBase.java:353)
    	at com.zaxxer.hikari.pool.PoolBase.newPoolEntry(PoolBase.java:201)
    	at com.zaxxer.hikari.pool.HikariPool.createPoolEntry(HikariPool.java:473)
    	at com.zaxxer.hikari.pool.HikariPool.checkFailFast(HikariPool.java:562)
    	... 41 more

## Expected a string or numeric value for field 'field_name' of type VARCHAR: [value1, value2] [ArrayList]

The error is encountered when Elasticsearch connector is used. The connector can handle only [simple data types](https://trino.io/docs/current/connector/elasticsearch.html#data-types) but Elasticsearch includes list of values in a field.

The message is.

    :::text
    SQL Error [58]: Query failed (#20211105_183725_88248_kz74f): Expected a string or numeric value for field 'field_name' of type VARCHAR: [value1, value2, value3] [ArrayList]

To fix the issue, a command should be run to notify Trino about those fields in the _meta section of the index mapping. Replace those place holders with your values: elastic.sample.com:9200, index_name, and field_name.

    :::bash
    curl --request PUT \
        --url elastic.sample.com:9200/index_name/doc/_mapping \
        --header 'content-type: application/json' \
        --data '
    {
        "_meta": {
            "presto":{
                "field_name":{
                    "isArray":true
                }
            }
        }
    }'


The testing can be done running any of those SQL statements.

    :::sql
    SELECT field_name FROM elastic_catalog."default".index_name WHERE field_name IS NOT NULL;
    SELECT DISTINCT field_name FROM elastic_catalog."default".index_name;

Trino documentation reference is [Array types](https://trino.io/docs/current/connector/elasticsearch.html#array-types).

## Failed to list directory. Unable to execute HTTP request: Read timed out

The issue might be caused by MinIO or other S3 storage. Hive metastore tries to access S3 storage (list objects in a bucket), but response time is longer than timeout setting in Hive connector. For example, there are 100k files in a bucket, and it might take over 1 minute to list those files. The case might get worse if S3 cluster is busy.

A solution is to increase Hive metastore timeout to 1-5 minutes in your Hive catalog which accessing S3 storage. The setting is `hive.s3.socket-timeout=3m`.

The message is.

    :::json
    {
        "type": "io.trino.spi.TrinoException",
        "message": "Failed to list directory: s3a://bucket_name/",
        "cause": {
            "type": "java.io.IOException",
            "message": "com.amazonaws.SdkClientException: Unable to execute HTTP request: Read timed out",
            "cause": {
                "type": "com.amazonaws.SdkClientException",
                "message": "Unable to execute HTTP request: Read timed out",
                "cause": {
                    "type": "java.net.SocketTimeoutException",
                    "message": "Read timed out",
                    "suppressed": [
                    ],
                    "stack": [
                        "java.base/sun.nio.ch.NioSocketImpl.timedRead(NioSocketImpl.java:283)",
                        "java.base/sun.nio.ch.NioSocketImpl.implRead(NioSocketImpl.java:309)",
                        "java.base/sun.nio.ch.NioSocketImpl.read(NioSocketImpl.java:350)",
                        "java.base/sun.nio.ch.NioSocketImpl$1.read(NioSocketImpl.java:803)",
                        "java.base/java.net.Socket$SocketInputStream.read(Socket.java:966)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.read(SSLSocketInputRecord.java:478)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.readHeader(SSLSocketInputRecord.java:472)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.bytesInCompletePacket(SSLSocketInputRecord.java:70)",
                        "java.base/sun.security.ssl.SSLSocketImpl.readApplicationRecord(SSLSocketImpl.java:1460)",
                        "java.base/sun.security.ssl.SSLSocketImpl$AppInputStream.read(SSLSocketImpl.java:1064)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.streamRead(SessionInputBufferImpl.java:137)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.fillBuffer(SessionInputBufferImpl.java:153)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.readLine(SessionInputBufferImpl.java:280)",
                        "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:138)",
                        "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:56)",
                        "org.apache.http.impl.io.AbstractMessageParser.parse(AbstractMessageParser.java:259)",
                        "org.apache.http.impl.DefaultBHttpClientConnection.receiveResponseHeader(DefaultBHttpClientConnection.java:163)",
                        "org.apache.http.impl.conn.CPoolProxy.receiveResponseHeader(CPoolProxy.java:157)",
                        "org.apache.http.protocol.HttpRequestExecutor.doReceiveResponse(HttpRequestExecutor.java:273)",
                        "com.amazonaws.http.protocol.SdkHttpRequestExecutor.doReceiveResponse(SdkHttpRequestExecutor.java:82)",
                        "org.apache.http.protocol.HttpRequestExecutor.execute(HttpRequestExecutor.java:125)",
                        "org.apache.http.impl.execchain.MainClientExec.execute(MainClientExec.java:272)",
                        "org.apache.http.impl.execchain.ProtocolExec.execute(ProtocolExec.java:186)",
                        "org.apache.http.impl.client.InternalHttpClient.doExecute(InternalHttpClient.java:185)",
                        "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:83)",
                        "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:56)",
                        "com.amazonaws.http.apache.client.impl.SdkHttpClient.execute(SdkHttpClient.java:72)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeOneRequest(AmazonHttpClient.java:1343)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeHelper(AmazonHttpClient.java:1154)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.doExecute(AmazonHttpClient.java:811)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeWithTimer(AmazonHttpClient.java:779)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.execute(AmazonHttpClient.java:753)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.access$500(AmazonHttpClient.java:713)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutionBuilderImpl.execute(AmazonHttpClient.java:695)",
                        "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:559)",
                        "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:539)",
                        "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5453)",
                        "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5400)",
                        "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5394)",
                        "com.amazonaws.services.s3.AmazonS3Client.listObjectsV2(AmazonS3Client.java:971)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$1.computeNext(TrinoS3FileSystem.java:678)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$1.computeNext(TrinoS3FileSystem.java:669)",
                        "com.google.common.collect.AbstractSequentialIterator.next(AbstractSequentialIterator.java:74)",
                        "com.google.common.collect.TransformedIterator.next(TransformedIterator.java:52)",
                        "com.google.common.collect.Iterators$ConcatenatedIterator.hasNext(Iterators.java:1400)",
                        "com.google.common.collect.Iterators$5.computeNext(Iterators.java:671)",
                        "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
                        "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$S3ObjectsV2RemoteIterator.hasNext(TrinoS3FileSystem.java:414)",
                        "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.fetchNextCachedFile(TransactionScopeCachingDirectoryLister.java:252)",
                        "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.getCachedFile(TransactionScopeCachingDirectoryLister.java:236)",
                        "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$1.hasNext(TransactionScopeCachingDirectoryLister.java:150)",
                        "io.trino.plugin.hive.fs.HiveFileIterator$FileStatusIterator.hasNext(HiveFileIterator.java:203)",
                        "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:80)",
                        "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:39)",
                        "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
                        "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
                        "java.base/java.util.Spliterators$IteratorSpliterator.tryAdvance(Spliterators.java:1855)",
                        "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.lambda$initPartialTraversalState$0(StreamSpliterators.java:292)",
                        "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.fillBuffer(StreamSpliterators.java:206)",
                        "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.doAdvance(StreamSpliterators.java:169)",
                        "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.tryAdvance(StreamSpliterators.java:298)",
                        "java.base/java.util.Spliterators$1Adapter.hasNext(Spliterators.java:681)",
                        "io.trino.plugin.hive.BackgroundHiveSplitLoader.loadSplits(BackgroundHiveSplitLoader.java:353)",
                        "io.trino.plugin.hive.BackgroundHiveSplitLoader$HiveSplitLoaderTask.process(BackgroundHiveSplitLoader.java:274)",
                        "io.trino.plugin.hive.util.ResumableTasks$1.run(ResumableTasks.java:38)",
                        "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
                        "io.airlift.concurrent.BoundedExecutor.drainQueue(BoundedExecutor.java:80)",
                        "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
                        "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
                        "java.base/java.lang.Thread.run(Thread.java:833)"
                    ],
                    "errorCode": {
                        "code": 65536,
                        "name": "GENERIC_INTERNAL_ERROR",
                        "type": "INTERNAL_ERROR"
                    }
                },
                "suppressed": [
                ],
                "stack": [
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.handleRetryableException(AmazonHttpClient.java:1216)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeHelper(AmazonHttpClient.java:1162)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.doExecute(AmazonHttpClient.java:811)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeWithTimer(AmazonHttpClient.java:779)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.execute(AmazonHttpClient.java:753)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutor.access$500(AmazonHttpClient.java:713)",
                    "com.amazonaws.http.AmazonHttpClient$RequestExecutionBuilderImpl.execute(AmazonHttpClient.java:695)",
                    "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:559)",
                    "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:539)",
                    "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5453)",
                    "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5400)",
                    "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5394)",
                    "com.amazonaws.services.s3.AmazonS3Client.listObjectsV2(AmazonS3Client.java:971)",
                    "io.trino.plugin.hive.s3.TrinoS3FileSystem$1.computeNext(TrinoS3FileSystem.java:678)",
                    "io.trino.plugin.hive.s3.TrinoS3FileSystem$1.computeNext(TrinoS3FileSystem.java:669)",
                    "com.google.common.collect.AbstractSequentialIterator.next(AbstractSequentialIterator.java:74)",
                    "com.google.common.collect.TransformedIterator.next(TransformedIterator.java:52)",
                    "com.google.common.collect.Iterators$ConcatenatedIterator.hasNext(Iterators.java:1400)",
                    "com.google.common.collect.Iterators$5.computeNext(Iterators.java:671)",
                    "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
                    "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
                    "io.trino.plugin.hive.s3.TrinoS3FileSystem$S3ObjectsV2RemoteIterator.hasNext(TrinoS3FileSystem.java:414)",
                    "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.fetchNextCachedFile(TransactionScopeCachingDirectoryLister.java:252)",
                    "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.getCachedFile(TransactionScopeCachingDirectoryLister.java:236)",
                    "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$1.hasNext(TransactionScopeCachingDirectoryLister.java:150)",
                    "io.trino.plugin.hive.fs.HiveFileIterator$FileStatusIterator.hasNext(HiveFileIterator.java:203)",
                    "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:80)",
                    "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:39)",
                    "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
                    "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
                    "java.base/java.util.Spliterators$IteratorSpliterator.tryAdvance(Spliterators.java:1855)",
                    "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.lambda$initPartialTraversalState$0(StreamSpliterators.java:292)",
                    "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.fillBuffer(StreamSpliterators.java:206)",
                    "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.doAdvance(StreamSpliterators.java:169)",
                    "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.tryAdvance(StreamSpliterators.java:298)",
                    "java.base/java.util.Spliterators$1Adapter.hasNext(Spliterators.java:681)",
                    "io.trino.plugin.hive.BackgroundHiveSplitLoader.loadSplits(BackgroundHiveSplitLoader.java:353)",
                    "io.trino.plugin.hive.BackgroundHiveSplitLoader$HiveSplitLoaderTask.process(BackgroundHiveSplitLoader.java:274)",
                    "io.trino.plugin.hive.util.ResumableTasks$1.run(ResumableTasks.java:38)",
                    "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
                    "io.airlift.concurrent.BoundedExecutor.drainQueue(BoundedExecutor.java:80)",
                    "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
                    "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
                    "java.base/java.lang.Thread.run(Thread.java:833)"
                ],
                "errorCode": {
                    "code": 65536,
                    "name": "GENERIC_INTERNAL_ERROR",
                    "type": "INTERNAL_ERROR"
                }
            },
            "suppressed": [
            ],
            "stack": [
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$S3ObjectsV2RemoteIterator.hasNext(TrinoS3FileSystem.java:417)",
                "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.fetchNextCachedFile(TransactionScopeCachingDirectoryLister.java:252)",
                "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$FetchingValueHolder.getCachedFile(TransactionScopeCachingDirectoryLister.java:236)",
                "io.trino.plugin.hive.fs.TransactionScopeCachingDirectoryLister$1.hasNext(TransactionScopeCachingDirectoryLister.java:150)",
                "io.trino.plugin.hive.fs.HiveFileIterator$FileStatusIterator.hasNext(HiveFileIterator.java:203)",
                "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:80)",
                "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:39)",
                "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
                "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
                "java.base/java.util.Spliterators$IteratorSpliterator.tryAdvance(Spliterators.java:1855)",
                "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.lambda$initPartialTraversalState$0(StreamSpliterators.java:292)",
                "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.fillBuffer(StreamSpliterators.java:206)",
                "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.doAdvance(StreamSpliterators.java:169)",
                "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.tryAdvance(StreamSpliterators.java:298)",
                "java.base/java.util.Spliterators$1Adapter.hasNext(Spliterators.java:681)",
                "io.trino.plugin.hive.BackgroundHiveSplitLoader.loadSplits(BackgroundHiveSplitLoader.java:353)",
                "io.trino.plugin.hive.BackgroundHiveSplitLoader$HiveSplitLoaderTask.process(BackgroundHiveSplitLoader.java:274)",
                "io.trino.plugin.hive.util.ResumableTasks$1.run(ResumableTasks.java:38)",
                "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
                "io.airlift.concurrent.BoundedExecutor.drainQueue(BoundedExecutor.java:80)",
                "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
                "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
                "java.base/java.lang.Thread.run(Thread.java:833)"
            ],
            "errorCode": {
                "code": 65536,
                "name": "GENERIC_INTERNAL_ERROR",
                "type": "INTERNAL_ERROR"
            }
        },
        "suppressed": [
        ],
        "stack": [
            "io.trino.plugin.hive.fs.HiveFileIterator$FileStatusIterator.processException(HiveFileIterator.java:227)",
            "io.trino.plugin.hive.fs.HiveFileIterator$FileStatusIterator.hasNext(HiveFileIterator.java:206)",
            "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:80)",
            "io.trino.plugin.hive.fs.HiveFileIterator.computeNext(HiveFileIterator.java:39)",
            "com.google.common.collect.AbstractIterator.tryToComputeNext(AbstractIterator.java:146)",
            "com.google.common.collect.AbstractIterator.hasNext(AbstractIterator.java:141)",
            "java.base/java.util.Spliterators$IteratorSpliterator.tryAdvance(Spliterators.java:1855)",
            "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.lambda$initPartialTraversalState$0(StreamSpliterators.java:292)",
            "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.fillBuffer(StreamSpliterators.java:206)",
            "java.base/java.util.stream.StreamSpliterators$AbstractWrappingSpliterator.doAdvance(StreamSpliterators.java:169)",
            "java.base/java.util.stream.StreamSpliterators$WrappingSpliterator.tryAdvance(StreamSpliterators.java:298)",
            "java.base/java.util.Spliterators$1Adapter.hasNext(Spliterators.java:681)",
            "io.trino.plugin.hive.BackgroundHiveSplitLoader.loadSplits(BackgroundHiveSplitLoader.java:353)",
            "io.trino.plugin.hive.BackgroundHiveSplitLoader$HiveSplitLoaderTask.process(BackgroundHiveSplitLoader.java:274)",
            "io.trino.plugin.hive.util.ResumableTasks$1.run(ResumableTasks.java:38)",
            "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
            "io.airlift.concurrent.BoundedExecutor.drainQueue(BoundedExecutor.java:80)",
            "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
            "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
            "java.base/java.lang.Thread.run(Thread.java:833)"
        ],
        "errorCode": {
            "code": 16777232,
            "name": "HIVE_FILESYSTEM_ERROR",
            "type": "EXTERNAL"
        }
    }

## Unable to execute HTTP request: Read timed out

This error is so generic, but the core statement in the error message is `com.amazonaws.SdkClientException` which pointing out to MinIO or other S3 storage. Hive connector tries to access S3 storage, but response time is longer than timeout setting in Hive connector.

A solution is to increase Hive metastore timeout to 1-5 minutes in your Hive catalog for accessing S3 storage. The setting is `hive.s3.socket-timeout=3m`.

The message is.

    :::json
    {
        "type": "com.amazonaws.SdkClientException",
        "message": "Unable to execute HTTP request: Read timed out",
        "cause": {
            "type": "java.net.SocketTimeoutException",
            "message": "Read timed out",
            "suppressed": [
            ],
            "stack": [
                "java.base/sun.nio.ch.NioSocketImpl.timedRead(NioSocketImpl.java:283)",
                "java.base/sun.nio.ch.NioSocketImpl.implRead(NioSocketImpl.java:309)",
                "java.base/sun.nio.ch.NioSocketImpl.read(NioSocketImpl.java:350)",
                "java.base/sun.nio.ch.NioSocketImpl$1.read(NioSocketImpl.java:803)",
                "java.base/java.net.Socket$SocketInputStream.read(Socket.java:966)",
                "java.base/sun.security.ssl.SSLSocketInputRecord.read(SSLSocketInputRecord.java:478)",
                "java.base/sun.security.ssl.SSLSocketInputRecord.readHeader(SSLSocketInputRecord.java:472)",
                "java.base/sun.security.ssl.SSLSocketInputRecord.bytesInCompletePacket(SSLSocketInputRecord.java:70)",
                "java.base/sun.security.ssl.SSLSocketImpl.readApplicationRecord(SSLSocketImpl.java:1460)",
                "java.base/sun.security.ssl.SSLSocketImpl$AppInputStream.read(SSLSocketImpl.java:1064)",
                "org.apache.http.impl.io.SessionInputBufferImpl.streamRead(SessionInputBufferImpl.java:137)",
                "org.apache.http.impl.io.SessionInputBufferImpl.fillBuffer(SessionInputBufferImpl.java:153)",
                "org.apache.http.impl.io.SessionInputBufferImpl.readLine(SessionInputBufferImpl.java:280)",
                "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:138)",
                "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:56)",
                "org.apache.http.impl.io.AbstractMessageParser.parse(AbstractMessageParser.java:259)",
                "org.apache.http.impl.DefaultBHttpClientConnection.receiveResponseHeader(DefaultBHttpClientConnection.java:163)",
                "org.apache.http.impl.conn.CPoolProxy.receiveResponseHeader(CPoolProxy.java:157)",
                "org.apache.http.protocol.HttpRequestExecutor.doReceiveResponse(HttpRequestExecutor.java:273)",
                "com.amazonaws.http.protocol.SdkHttpRequestExecutor.doReceiveResponse(SdkHttpRequestExecutor.java:82)",
                "org.apache.http.protocol.HttpRequestExecutor.execute(HttpRequestExecutor.java:125)",
                "org.apache.http.impl.execchain.MainClientExec.execute(MainClientExec.java:272)",
                "org.apache.http.impl.execchain.ProtocolExec.execute(ProtocolExec.java:186)",
                "org.apache.http.impl.client.InternalHttpClient.doExecute(InternalHttpClient.java:185)",
                "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:83)",
                "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:56)",
                "com.amazonaws.http.apache.client.impl.SdkHttpClient.execute(SdkHttpClient.java:72)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeOneRequest(AmazonHttpClient.java:1343)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeHelper(AmazonHttpClient.java:1154)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.doExecute(AmazonHttpClient.java:811)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeWithTimer(AmazonHttpClient.java:779)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.execute(AmazonHttpClient.java:753)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutor.access$500(AmazonHttpClient.java:713)",
                "com.amazonaws.http.AmazonHttpClient$RequestExecutionBuilderImpl.execute(AmazonHttpClient.java:695)",
                "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:559)",
                "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:539)",
                "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5453)",
                "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5400)",
                "com.amazonaws.services.s3.AmazonS3Client.getObject(AmazonS3Client.java:1524)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.lambda$openStream$2(TrinoS3FileSystem.java:1335)",
                "io.trino.plugin.hive.util.RetryDriver.run(RetryDriver.java:130)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.openStream(TrinoS3FileSystem.java:1330)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.openStream(TrinoS3FileSystem.java:1315)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.seekStream(TrinoS3FileSystem.java:1308)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.lambda$read$1(TrinoS3FileSystem.java:1252)",
                "io.trino.plugin.hive.util.RetryDriver.run(RetryDriver.java:130)",
                "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.read(TrinoS3FileSystem.java:1251)",
                "java.base/java.io.BufferedInputStream.read1(BufferedInputStream.java:282)",
                "java.base/java.io.BufferedInputStream.read(BufferedInputStream.java:343)",
                "java.base/java.io.DataInputStream.read(DataInputStream.java:151)",
                "java.base/java.io.DataInputStream.read(DataInputStream.java:151)",
                "org.apache.hadoop.io.compress.DecompressorStream.getCompressedData(DecompressorStream.java:179)",
                "org.apache.hadoop.io.compress.DecompressorStream.decompress(DecompressorStream.java:163)",
                "org.apache.hadoop.io.compress.DecompressorStream.read(DecompressorStream.java:105)",
                "java.base/java.io.InputStream.read(InputStream.java:218)",
                "org.apache.hadoop.util.LineReader.fillBuffer(LineReader.java:200)",
                "org.apache.hadoop.util.LineReader.readDefaultLine(LineReader.java:237)",
                "org.apache.hadoop.util.LineReader.readLine(LineReader.java:193)",
                "org.apache.hadoop.mapred.LineRecordReader.skipUtfByteOrderMark(LineRecordReader.java:215)",
                "org.apache.hadoop.mapred.LineRecordReader.next(LineRecordReader.java:253)",
                "org.apache.hadoop.mapred.LineRecordReader.next(LineRecordReader.java:48)",
                "org.apache.hadoop.hive.ql.exec.Utilities.skipHeader(Utilities.java:3802)",
                "io.trino.plugin.hive.util.HiveUtil.createRecordReader(HiveUtil.java:263)",
                "io.trino.plugin.hive.GenericHiveRecordCursorProvider.lambda$createRecordCursor$1(GenericHiveRecordCursorProvider.java:96)",
                "io.trino.hdfs.authentication.NoHdfsAuthentication.doAs(NoHdfsAuthentication.java:25)",
                "io.trino.hdfs.HdfsEnvironment.doAs(HdfsEnvironment.java:94)",
                "io.trino.plugin.hive.GenericHiveRecordCursorProvider.createRecordCursor(GenericHiveRecordCursorProvider.java:95)",
                "io.trino.plugin.hive.HivePageSourceProvider.createHivePageSource(HivePageSourceProvider.java:330)",
                "io.trino.plugin.hive.HivePageSourceProvider.createPageSource(HivePageSourceProvider.java:196)",
                "com.starburstdata.dynamicfiltering.DynamicRowFilteringPageSourceProvider.createPageSource(DynamicRowFilteringPageSourceProvider.java:54)",
                "io.trino.plugin.base.classloader.ClassLoaderSafeConnectorPageSourceProvider.createPageSource(ClassLoaderSafeConnectorPageSourceProvider.java:49)",
                "io.trino.split.PageSourceManager.createPageSource(PageSourceManager.java:62)",
                "io.trino.operator.ScanFilterAndProjectOperator$SplitToPages.process(ScanFilterAndProjectOperator.java:268)",
                "io.trino.operator.ScanFilterAndProjectOperator$SplitToPages.process(ScanFilterAndProjectOperator.java:196)",
                "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:338)",
                "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:325)",
                "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:325)",
                "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                "io.trino.operator.WorkProcessorUtils.getNextState(WorkProcessorUtils.java:240)",
                "io.trino.operator.WorkProcessorUtils.lambda$processStateMonitor$3(WorkProcessorUtils.java:219)",
                "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                "io.trino.operator.WorkProcessorUtils.getNextState(WorkProcessorUtils.java:240)",
                "io.trino.operator.WorkProcessorUtils.lambda$finishWhen$4(WorkProcessorUtils.java:234)",
                "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                "io.trino.operator.WorkProcessorSourceOperatorAdapter.getOutput(WorkProcessorSourceOperatorAdapter.java:150)",
                "io.trino.operator.Driver.processInternal(Driver.java:410)",
                "io.trino.operator.Driver.lambda$process$10(Driver.java:313)",
                "io.trino.operator.Driver.tryWithLock(Driver.java:703)",
                "io.trino.operator.Driver.process(Driver.java:305)",
                "io.trino.operator.Driver.processForDuration(Driver.java:276)",
                "io.trino.execution.SqlTaskExecution$DriverSplitRunner.processFor(SqlTaskExecution.java:737)",
                "io.trino.execution.executor.PrioritizedSplitRunner.process(PrioritizedSplitRunner.java:164)",
                "io.trino.execution.executor.TaskExecutor$TaskRunner.run(TaskExecutor.java:490)",
                "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
                "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
                "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
                "java.base/java.lang.Thread.run(Thread.java:833)"
            ],
            "errorCode": {
                "code": 65536,
                "name": "GENERIC_INTERNAL_ERROR",
                "type": "INTERNAL_ERROR"
            }
        },
        "suppressed": [
            {
                "type": "com.amazonaws.SdkClientException",
                "message": "Unable to execute HTTP request: Read timed out",
                "cause": {
                    "type": "java.net.SocketTimeoutException",
                    "message": "Read timed out",
                    "suppressed": [
                    ],
                    "stack": [
                        "java.base/sun.nio.ch.NioSocketImpl.timedRead(NioSocketImpl.java:283)",
                        "java.base/sun.nio.ch.NioSocketImpl.implRead(NioSocketImpl.java:309)",
                        "java.base/sun.nio.ch.NioSocketImpl.read(NioSocketImpl.java:350)",
                        "java.base/sun.nio.ch.NioSocketImpl$1.read(NioSocketImpl.java:803)",
                        "java.base/java.net.Socket$SocketInputStream.read(Socket.java:966)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.read(SSLSocketInputRecord.java:478)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.readHeader(SSLSocketInputRecord.java:472)",
                        "java.base/sun.security.ssl.SSLSocketInputRecord.bytesInCompletePacket(SSLSocketInputRecord.java:70)",
                        "java.base/sun.security.ssl.SSLSocketImpl.readApplicationRecord(SSLSocketImpl.java:1460)",
                        "java.base/sun.security.ssl.SSLSocketImpl$AppInputStream.read(SSLSocketImpl.java:1064)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.streamRead(SessionInputBufferImpl.java:137)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.fillBuffer(SessionInputBufferImpl.java:153)",
                        "org.apache.http.impl.io.SessionInputBufferImpl.readLine(SessionInputBufferImpl.java:280)",
                        "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:138)",
                        "org.apache.http.impl.conn.DefaultHttpResponseParser.parseHead(DefaultHttpResponseParser.java:56)",
                        "org.apache.http.impl.io.AbstractMessageParser.parse(AbstractMessageParser.java:259)",
                        "org.apache.http.impl.DefaultBHttpClientConnection.receiveResponseHeader(DefaultBHttpClientConnection.java:163)",
                        "org.apache.http.impl.conn.CPoolProxy.receiveResponseHeader(CPoolProxy.java:157)",
                        "org.apache.http.protocol.HttpRequestExecutor.doReceiveResponse(HttpRequestExecutor.java:273)",
                        "com.amazonaws.http.protocol.SdkHttpRequestExecutor.doReceiveResponse(SdkHttpRequestExecutor.java:82)",
                        "org.apache.http.protocol.HttpRequestExecutor.execute(HttpRequestExecutor.java:125)",
                        "org.apache.http.impl.execchain.MainClientExec.execute(MainClientExec.java:272)",
                        "org.apache.http.impl.execchain.ProtocolExec.execute(ProtocolExec.java:186)",
                        "org.apache.http.impl.client.InternalHttpClient.doExecute(InternalHttpClient.java:185)",
                        "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:83)",
                        "org.apache.http.impl.client.CloseableHttpClient.execute(CloseableHttpClient.java:56)",
                        "com.amazonaws.http.apache.client.impl.SdkHttpClient.execute(SdkHttpClient.java:72)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeOneRequest(AmazonHttpClient.java:1343)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeHelper(AmazonHttpClient.java:1154)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.doExecute(AmazonHttpClient.java:811)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.executeWithTimer(AmazonHttpClient.java:779)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.execute(AmazonHttpClient.java:753)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutor.access$500(AmazonHttpClient.java:713)",
                        "com.amazonaws.http.AmazonHttpClient$RequestExecutionBuilderImpl.execute(AmazonHttpClient.java:695)",
                        "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:559)",
                        "com.amazonaws.http.AmazonHttpClient.execute(AmazonHttpClient.java:539)",
                        "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5453)",
                        "com.amazonaws.services.s3.AmazonS3Client.invoke(AmazonS3Client.java:5400)",
                        "com.amazonaws.services.s3.AmazonS3Client.getObject(AmazonS3Client.java:1524)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.lambda$openStream$2(TrinoS3FileSystem.java:1335)",
                        "io.trino.plugin.hive.util.RetryDriver.run(RetryDriver.java:130)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.openStream(TrinoS3FileSystem.java:1330)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.openStream(TrinoS3FileSystem.java:1315)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.seekStream(TrinoS3FileSystem.java:1308)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.lambda$read$1(TrinoS3FileSystem.java:1252)",
                        "io.trino.plugin.hive.util.RetryDriver.run(RetryDriver.java:130)",
                        "io.trino.plugin.hive.s3.TrinoS3FileSystem$TrinoS3InputStream.read(TrinoS3FileSystem.java:1251)",
                        "java.base/java.io.BufferedInputStream.read1(BufferedInputStream.java:282)",
                        "java.base/java.io.BufferedInputStream.read(BufferedInputStream.java:343)",
                        "java.base/java.io.DataInputStream.read(DataInputStream.java:151)",
                        "java.base/java.io.DataInputStream.read(DataInputStream.java:151)",
                        "org.apache.hadoop.io.compress.DecompressorStream.getCompressedData(DecompressorStream.java:179)",
                        "org.apache.hadoop.io.compress.DecompressorStream.decompress(DecompressorStream.java:163)",
                        "org.apache.hadoop.io.compress.DecompressorStream.read(DecompressorStream.java:105)",
                        "java.base/java.io.InputStream.read(InputStream.java:218)",
                        "org.apache.hadoop.util.LineReader.fillBuffer(LineReader.java:200)",
                        "org.apache.hadoop.util.LineReader.readDefaultLine(LineReader.java:237)",
                        "org.apache.hadoop.util.LineReader.readLine(LineReader.java:193)",
                        "org.apache.hadoop.mapred.LineRecordReader.skipUtfByteOrderMark(LineRecordReader.java:215)",
                        "org.apache.hadoop.mapred.LineRecordReader.next(LineRecordReader.java:253)",
                        "org.apache.hadoop.mapred.LineRecordReader.next(LineRecordReader.java:48)",
                        "org.apache.hadoop.hive.ql.exec.Utilities.skipHeader(Utilities.java:3802)",
                        "io.trino.plugin.hive.util.HiveUtil.createRecordReader(HiveUtil.java:263)",
                        "io.trino.plugin.hive.GenericHiveRecordCursorProvider.lambda$createRecordCursor$1(GenericHiveRecordCursorProvider.java:96)",
                        "io.trino.hdfs.authentication.NoHdfsAuthentication.doAs(NoHdfsAuthentication.java:25)",
                        "io.trino.hdfs.HdfsEnvironment.doAs(HdfsEnvironment.java:94)",
                        "io.trino.plugin.hive.GenericHiveRecordCursorProvider.createRecordCursor(GenericHiveRecordCursorProvider.java:95)",
                        "io.trino.plugin.hive.HivePageSourceProvider.createHivePageSource(HivePageSourceProvider.java:330)",
                        "io.trino.plugin.hive.HivePageSourceProvider.createPageSource(HivePageSourceProvider.java:196)",
                        "com.starburstdata.dynamicfiltering.DynamicRowFilteringPageSourceProvider.createPageSource(DynamicRowFilteringPageSourceProvider.java:54)",
                        "io.trino.plugin.base.classloader.ClassLoaderSafeConnectorPageSourceProvider.createPageSource(ClassLoaderSafeConnectorPageSourceProvider.java:49)",
                        "io.trino.split.PageSourceManager.createPageSource(PageSourceManager.java:62)",
                        "io.trino.operator.ScanFilterAndProjectOperator$SplitToPages.process(ScanFilterAndProjectOperator.java:268)",
                        "io.trino.operator.ScanFilterAndProjectOperator$SplitToPages.process(ScanFilterAndProjectOperator.java:196)",
                        "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:338)",
                        "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                        "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:325)",
                        "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                        "io.trino.operator.WorkProcessorUtils$3.process(WorkProcessorUtils.java:325)",
                        "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                        "io.trino.operator.WorkProcessorUtils.getNextState(WorkProcessorUtils.java:240)",
                        "io.trino.operator.WorkProcessorUtils.lambda$processStateMonitor$3(WorkProcessorUtils.java:219)",
                        "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                        "io.trino.operator.WorkProcessorUtils.getNextState(WorkProcessorUtils.java:240)",
                        "io.trino.operator.WorkProcessorUtils.lambda$finishWhen$4(WorkProcessorUtils.java:234)",
                        "io.trino.operator.WorkProcessorUtils$ProcessWorkProcessor.process(WorkProcessorUtils.java:391)",
                        "io.trino.operator.WorkProcessorSourceOperatorAdapter.getOutput(WorkProcessorSourceOperatorAdapter.java:150)",
                        "io.trino.operator.Driver.processInternal(Driver.java:410)",
                        "io.trino.operator.Driver.lambda$process$10(Driver.java:313)",
                        "io.trino.operator.Driver.tryWithLock(Driver.java:703)",
                        "io.trino.operator.Driver.process(Driver.java:305)",
                        "io.trino.operator.Driver.processForDuration(Driver.java:276)",
                        "io.trino.execution.SqlTaskExecution$DriverSplitRunner.processFor(SqlTaskExecution.java:737)",
                        "io.trino.execution.executor.PrioritizedSplitRunner.process(PrioritizedSplitRunner.java:164)",
                        "io.trino.execution.executor.TaskExecutor$TaskRunner.run(TaskExecutor.java:490)",
                        "io.trino.$gen.Trino_393_e_1____20221126_012135_2.run(Unknown Source)",
                        "java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)",
                        "java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)",
                        "java.base/java.lang.Thread.run(Thread.java:833)"
                    ],
                    "errorCode": {
                        "code": 65536,
                        "name": "GENERIC_INTERNAL_ERROR",
                        "type": "INTERNAL_ERROR"
                    }
               ...

## Error loading PEM key store

The error might be caused by a pem file with encrypted private key. Trino does not accept it in some cases.

There are some solutions to convert your pem file into different format or type. The first one is to remove encryption from a private key keeping it as a pem file. The second 
option is to convert pem file into Java keystore or PKCS #12 format with .pfx or .p12 extension.

The message is.

    :::text
    Caused by: IllegalArgumentException: Error loading PEM key store: /certificate/trino.pem
            at ReloadableSslContextFactoryProvider.loadKeyStore(ReloadableSslContextFactoryProvider.java:207)
            at ReloadableSslContextFactoryProvider.loadContextFactory(ReloadableSslContextFactoryProvider.java:107)
            at ReloadableSslContextFactoryProvider.<init>(ReloadableSslContextFactoryProvider.java:99)
            at HttpServer.createReloadingSslContextFactory(HttpServer.java:534)
            at HttpServer.lambda$new$0(HttpServer.java:231)
            at java.base/Optional.orElseGet(Optional.java:364)
            at HttpServer.<init>(HttpServer.java:231)
            at HttpServerProvider.get(HttpServerProvider.java:149)
            at HttpServerProvider.get(HttpServerProvider.java:46)
            at ProviderInternalFactory.provision(ProviderInternalFactory.java:86)
            at BoundProviderFactory.provision(BoundProviderFactory.java:72)
            at ProviderInternalFactory$1.call(ProviderInternalFactory.java:67)
            at ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:109)
            at LifeCycleModule.provision(LifeCycleModule.java:54)
            at ProvisionListenerStackCallback$Provision.provision(ProvisionListenerStackCallback.java:117)
            at ProvisionListenerStackCallback.provision(ProvisionListenerStackCallback.java:66)
            at ProviderInternalFactory.circularGet(ProviderInternalFactory.java:62)
            at BoundProviderFactory.get(BoundProviderFactory.java:59)
            at ProviderToInternalFactoryAdapter.get(ProviderToInternalFactoryAdapter.java:40)
            at SingletonScope$1.get(SingletonScope.java:169)
            at InternalFactoryToProviderAdapter.get(InternalFactoryToProviderAdapter.java:45)
            at InternalInjectorCreator.loadEagerSingletons(InternalInjectorCreator.java:213)
            at InternalInjectorCreator.injectDynamically(InternalInjectorCreator.java:186)
            at InternalInjectorCreator.build(InternalInjectorCreator.java:113)
            at Guice.createInjector(Guice.java:87)
            at Bootstrap.initialize(Bootstrap.java:270)
            at Server.doStart(Server.java:130)
            at Server.lambda$start$0(Server.java:88)
            at io.trino.$gen.Trino_393_e_6____20230113_210746_1.run(Unknown Source)
            at Server.start(Server.java:88)
            at StarburstTrinoServer.main(StarburstTrinoServer.java:46)
    Caused by: NoSuchAlgorithmException: PBES2 SecretKeyFactory not available
            at java.base/SecretKeyFactory.<init>(SecretKeyFactory.java:118)
            at java.base/SecretKeyFactory.getInstance(SecretKeyFactory.java:164)
            at PemReader.loadPrivateKey(PemReader.java:239)
            at PemReader.loadPrivateKey(PemReader.java:211)
            at PemReader.loadKeyStore(PemReader.java:151)
            at ReloadableSslContextFactoryProvider.loadKeyStore(ReloadableSslContextFactoryProvider.java:203)
