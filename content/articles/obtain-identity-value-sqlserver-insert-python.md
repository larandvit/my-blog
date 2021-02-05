Title: Obtain Identity Value for SQL Server Insert in Python
Date: 2021-02-04
Category: Python, MS SQL Server
Cover: /extra/python-logo.png

Extracting of an identity value when data inserted into a SQL Server table in Python is done in the same way as in Microsoft Transact-SQL. The difference is that SQL Server statements are wrapped up in Python commands. **pypyodbc** is a good candidate to communicate with SQL Server in Python. There are 2 major methods in Transact-SQL to accomplish it. The first method is based on `OUTPUT` clause in `INSERT` statements. It is the most reliable way and it works in varieties of scenarios. The second method is simple and faster one but it might not work properly in triggers and stored procedures. It retrieves a value from `@@IDENTITY` or other variables.

The samples are run with Python 3.6, pypyodbc 1.3.4, and SQL Server 2016.

## Prerequisites

* Create the test table in SQL Server

        :::sql 
        CREATE TABLE [dbo].[book]( 
            [title] [varchar](100) NOT NULL,
            [author] [varchar](50) NOT NULL,
            [book_id] [int] IDENTITY(1,1) NOT NULL
        );

## Option #1

    :::python
    import pypyodbc

    connection_string = 'Driver={{SQL Server Native Client 11.0}};Server={server_name};Database={database_name};Trusted_Connection=yes;'.format(server_name='SAMPLE', database_name='BookStore')

    with pypyodbc.connect(connection_string) as con:
        
        cur = con.cursor()
    
        sql_text = 'SET NOCOUNT ON;' + \
                   'DECLARE @table_identity TABLE(book_id int);' + \
                   'INSERT INTO {table_name} '.format(table_name='book') + \
                   '(title, author) ' + \
                   'OUTPUT inserted.book_id INTO @table_identity(book_id) ' +\
                   'VALUES(?,?);' +\
                   'SELECT book_id FROM @table_identity;'
              
        cur.execute(sql_text, ('Don Quixote', 'Miguel de Cervantes'))
        book_id = cur.fetchone()[0]
        print(book_id)

## Option #2

    :::python
    import pypyodbc

    connection_string = 'Driver={{SQL Server Native Client 11.0}};Server={server_name};Database={database_name};Trusted_Connection=yes;'.format(server_name='SAMPLE', database_name='BookStore')

    with pypyodbc.connect(connection_string) as con:
        
        cur = con.cursor()
        
        sql_text = 'SET NOCOUNT ON;' + \
                   'INSERT INTO {table_name} '.format(table_name='book') + \
                   '(title, author) ' + \
                   'VALUES(?,?);' +\
                   'SELECT @@IDENTITY AS book_id;'
               
        cur.execute(sql_text, ('War and Peace', 'Leo Tolstoy'))
        book_id = cur.fetchone()[0]
        print(book_id)

`@@IDENTITY` variable can be replaced with `@@SCOPE_IDENTITY` and `@@IDENT_CURRENT('table_or_view_name')`. It depends on a case. The description of each variable is.

* `@@IDENTITY`: Returns the last identity value inserted into an identity column in the same session.
* `@@SCOPE_IDENTITY`: Returns the last identity value inserted into an identity column in the same session and scope. A scope is a module: a stored procedure, trigger, function, or batch.
* `@@IDENT_CURRENT('table_or_view_name')`: Returns the last identity value generated for a specified table or view. The last identity value generated can be for any session and any scope.

A Python connection is equal to a SQL Server session.

## Resources
* [Transact-SQL Reference](https://docs.microsoft.com/en-us/sql/t-sql/language-reference?view=sql-server-ver15)

