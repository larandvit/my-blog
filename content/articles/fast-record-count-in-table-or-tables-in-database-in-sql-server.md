Title: Fast Record Count in Table or Tables in Database in SQL Server
Date: 2020-04-06
Category: MS SQL Server
Cover: /extra/sql-server-logo.png

As data size is growing each year, count record in a table takes more and more time if you use old fashion methods, for example, `SELECT COUNT(*) FROM table`. It's the most reliable method to get record count but if you deal with multi-billion record table, it might take hours to get your result. Also, you need to consider that `COUNT(*)` operator is resource consuming as MS SQL Server has to scan every record in worst scenario. Moreover, `COUNT(*)` calculation locks a table which impacts on overall performance.

Meta data is a good source of information without workloading our SQL Server and getting results in no time. One drawback is that we might get "dirty" numbers because meta data is not aware of transactions which might occur during retrieving of record count. If we look at "dirty" numbers from another point of view, we can accept it. Let's imagine that we need to check our progress on inserting record in a table, we don't need exact number, we need to get a number to evaluate our current progress. Another case is when we have completed data transformation on a big table, we need to validate our result with record count. We know exactly that we don't do any data modification on the table and we are safe to use meta data.

To get record count fast, we need to consider a case when meta data might be locked and getting record count quickly will be problematic. It can be solved by reading uncommitted ("dirty") data with `NOLOCK` or `READUNCOMMITTED` hint.

An exception is when `TABLOCK` hint is used on your table. It will lock access to the table completely.

## 1. Count Records in a Table

    :::sql
    SELECT SCHEMA_NAME(schema_id) + '.' + t.name AS [Table Name],
           FORMAT(SUM(p.[rows]), '#,#') AS [Row Count]
    FROM sys.tables t WITH (NOLOCK) JOIN sys.partitions p WITH (NOLOCK) ON t.[object_id] = p.[object_id]
                                                                           AND p.index_id IN (0, 1)
    WHERE t.name='$(Table name)'
    GROUP BY t.[schema_id]
            ,t.[name];

Replace **$(Table name)** place holder with your table name.

## 2. Count Record in User Tables in a Database

    :::sql
    DECLARE @SqlText NVARCHAR(MAX);

    SELECT @SqlText = COALESCE(@SqlText + CHAR(13) + 'UNION ALL' + CHAR(13),'')
           + 'SELECT SCHEMA_NAME(schema_id) + ''.'' + t.name AS [Table Name],'
           +        'FORMAT(SUM(p.[rows]), ''#,#'') AS [Row Count] '
           + 'FROM sys.tables t WITH (NOLOCK) JOIN sys.partitions p WITH (NOLOCK) ON t.[object_id] = p.[object_id] '
           +                                                                        'AND p.index_id IN (0, 1) '
           + 'WHERE t.name=''' + l.[name] + ''' '
           +       'AND t.[schema_id]=' + CAST(l.[schema_id] AS VARCHAR(10)) + ' '
           + 'GROUP BY [schema_id]'
           +         ',t.[name]'
    FROM sys.tables l WITH (NOLOCK)
    WHERE [type] = 'U'
    ORDER BY SCHEMA_NAME([schema_id])
            ,[name];

    EXEC sp_executesql @SqlText
