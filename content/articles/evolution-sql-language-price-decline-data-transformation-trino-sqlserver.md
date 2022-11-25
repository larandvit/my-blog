Title: Evolution of SQL Language with Price Decline Data Transformation in Trino and SQL Server
Date: 2022-02-19
Modified: 2022-11-24
Category: Trino, MS SQL Server
Cover: /extra/sql-logo.png

SQL language is the most loved and supported language to work with data. It goes back in the early 1970s when it first hit the ground running. Since that time, the language has been developing with fast pace. The latest language standard is SQL 2016 which includes a great deal of new features.

Data transformation is well supported by SQL language. Price decline is one of the transformation topics used in recovery audit. Source data which contains a snapshot of prices in time is transformed into short form where extracted only moments of time with price decreases. It can be achieved with comparing previous price with current one in a loop. 

The evolution of SQL language can be represented with price decline transformation. The transformation might be done by means of different techniques and SQL language helps in it perfectly. It will be considered 3 different methods to accomplish the price decline transformation. Each method shows improvements in SQL language. We start from the oldest, slowest, and time-consuming process and finish with the latest way included in SQL 2016 standard. I would say that the latest one is not easy to understand comparing with the oldest one which is the way how people naturally proceed with data transformation.

[Starburst 369-e](https://www.starburst.io) distribution of [Trino](https://trino.io/) and [Microsoft SQL Server 2019](https://www.microsoft.com/en-us/sql-server/sql-server-2019) are used as SQL engines to transform data.

## Source data

There are 2 different customers who have ordered an item.

![Source data]({static}/images/evolution-sql-language-price-decline-data-transformation-trino-sqlserver/source-data.jpg)</br></br>

## Data transformation result

The output is partitioned by customers, so each customer has his/her price changes.

![Data transformation result]({static}/images/evolution-sql-language-price-decline-data-transformation-trino-sqlserver/data-transformation-result.jpg)</br></br>

## Cursor

The sample is developed only in Microsoft SQL Server as Trino doesn't support cursors and structured programming.

The first step is to sort data, and then loop through data comparing customer_id and the previous price with the current one. If the current price is less than previous one, this record is stored in an output table. As you can see, we need to create multiple variables and an output table. Also, `WHILE` and `IF` structure control calculation logic.

    :::sql
    DECLARE @CustomerID VARCHAR(10), @OrderDate AS DATE, @Price AS INTEGER;
    DECLARE @CurrentCustomerID VARCHAR(10), @CurrentOrderDate AS DATE, @CurrentPrice AS INTEGER;
    
    DECLARE @tblResult AS TABLE(customer_id VARCHAR(10), old_price INTEGER, old_price_date DATE, new_price INTEGER, price_decrease_date DATE, difference INTEGER);
    
    DECLARE cur_orders CURSOR FOR
    SELECT * 
    FROM (VALUES
            ('cust_1', CAST('2020-05-11' AS DATE), 100),
            ('cust_1', CAST('2020-05-12' AS DATE), 200),
            ('cust_2', CAST('2020-05-13' AS DATE),   8),
            ('cust_1', CAST('2020-05-14' AS DATE), 100),
            ('cust_2', CAST('2020-05-15' AS DATE),   4),
            ('cust_1', CAST('2020-05-16' AS DATE),  50),
            ('cust_1', CAST('2020-05-17' AS DATE), 100),
            ('cust_2', CAST('2020-05-18' AS DATE),   6),
            ('cust_2', CAST('2020-05-20' AS DATE),   3)) AS orders(customer_id, order_date, price)
    ORDER BY customer_id, order_date;
    
    SET @CurrentCustomerID=NULL;
    SET @CurrentPrice=999999999;
    
    OPEN cur_orders;
      
    FETCH NEXT FROM cur_orders INTO @CustomerID, @OrderDate, @Price;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        
        IF @CurrentCustomerID=@CustomerID
        BEGIN
            IF @CurrentPrice>@Price
            BEGIN
                INSERT INTO @tblResult (customer_id, old_price, old_price_date, new_price, price_decrease_date, difference) VALUES(@CustomerID, @CurrentPrice, @CurrentOrderDate, @Price, @OrderDate, @CurrentPrice-@Price);
            END
            
            SET @CurrentPrice=@Price;
            SET @CurrentOrderDate=@OrderDate;
        END 
        ELSE
        BEGIN
            SET @CurrentCustomerID=@CustomerID;
            SET @CurrentPrice=@Price;
            SET @CurrentOrderDate=@OrderDate;
        END
        
        FETCH NEXT FROM cur_orders INTO @CustomerID, @OrderDate, @Price
    END;
      
    CLOSE cur_orders;
    DEALLOCATE cur_orders;
    
    SELECT * FROM @tblResult ORDER BY customer_id, old_price_date;

## Recursive SQL statement

SQL Server and Trino statements are identical. The difference is how a source table is created.

There are 2 `SELECT` statements inside of `WITH` clause. The first statement is called an anchor or a recursive base and the second one is called a recursive member or a recursion step. The terminology is taken from corresponding documentation. The anchor is starting point of a recursion. The recursive member is the recursion definition. The logic requests to create an addition "id" field.

* SQL Server

        :::sql
        DROP TABLE IF EXISTS project.dbo.orders;
         
        SELECT *,
               ROW_NUMBER() OVER(PARTITION BY customer_id ORDER BY order_date) AS id
        INTO project.dbo.orders
        FROM (VALUES
                ('cust_1', CAST('2020-05-11' AS DATE), 100),
                ('cust_1', CAST('2020-05-12' AS DATE), 200),
                ('cust_2', CAST('2020-05-13' AS DATE),   8),
                ('cust_1', CAST('2020-05-14' AS DATE), 100),
                ('cust_2', CAST('2020-05-15' AS DATE),   4),
                ('cust_1', CAST('2020-05-16' AS DATE),  50),
                ('cust_1', CAST('2020-05-17' AS DATE), 100),
                ('cust_2', CAST('2020-05-18' AS DATE),   6),
                ('cust_2', CAST('2020-05-20' AS DATE),   3)
            ) AS orders(customer_id, order_date, price);
        
        WITH t(customer_id, old_price, old_order_date, new_price, new_order_date, difference, id) AS (
            SELECT
                customer_id, CAST(NULL AS integer), CAST(NULL AS date), price, order_date, 0, id
            FROM
                project.dbo.orders
            WHERE
                id=1
            UNION ALL
            SELECT
                o.customer_id,
                CASE 
                    WHEN o.price<t.new_price THEN t.new_price
                    ELSE NULL
                END AS old_price, 
                t.new_order_date AS old_order_date,
                o.price AS new_price,
                o.order_date AS new_order_date, 
                t.new_price-o.price AS difference,
                o.id
            FROM
                project.dbo.orders o JOIN t ON o.id=t.id+1
                                                AND o.customer_id=t.customer_id 
        )
        SELECT t.* 
        FROM t 
        WHERE old_price IS NOT NULL
        ORDER BY customer_id, new_order_date;   

* Trino

        :::sql
        DROP TABLE IF EXISTS memory."default".orders;
        CREATE TABLE memory."default".orders AS 
        SELECT *,
               ROW_NUMBER() OVER(PARTITION BY customer_id ORDER BY order_date) AS id
        FROM (VALUES
                ('cust_1', CAST('2020-05-11' AS DATE), 100),
                ('cust_1', CAST('2020-05-12' AS DATE), 200),
                ('cust_2', CAST('2020-05-13' AS DATE),   8),
                ('cust_1', CAST('2020-05-14' AS DATE), 100),
                ('cust_2', CAST('2020-05-15' AS DATE),   4),
                ('cust_1', CAST('2020-05-16' AS DATE),  50),
                ('cust_1', CAST('2020-05-17' AS DATE), 100),
                ('cust_2', CAST('2020-05-18' AS DATE),   6),
                ('cust_2', CAST('2020-05-20' AS DATE),   3)
            ) AS orders(customer_id, order_date, price);
        
        WITH RECURSIVE t(customer_id, old_price, old_order_date, new_price, new_order_date, difference, id) AS (
            SELECT
                customer_id, CAST(NULL AS integer), CAST(NULL AS date), price, order_date, 0, id
            FROM
                memory."default".orders
            WHERE
                id=1
            UNION ALL
            SELECT
                o.customer_id,
                CASE 
                    WHEN o.price<t.new_price THEN t.new_price
                    ELSE NULL
                END AS old_price, 
                t.new_order_date AS old_order_date,
                o.price AS new_price,
                o.order_date AS new_order_date, 
                t.new_price-o.price AS difference,
                o.id
            FROM
                memory."default".orders o JOIN t ON o.id=t.id+1
                                                    AND o.customer_id=t.customer_id 
        )
        SELECT t.* 
        FROM t 
        WHERE old_price IS NOT NULL
        ORDER BY customer_id, new_order_date;

## Match recognize SQL statement

There is one Trino sample. SQL Server 2019 doesn't support it.

Match recognize clause is aimed to identify patterns in a sequence of rows with regular expressions.

    :::sql
    WITH orders(customer_id, order_date, price) AS (VALUES
        ('cust_1', DATE '2020-05-11', 100),
        ('cust_1', DATE '2020-05-12', 200),
        ('cust_2', DATE '2020-05-13',   8),
        ('cust_1', DATE '2020-05-14', 100),
        ('cust_2', DATE '2020-05-15',   4),
        ('cust_1', DATE '2020-05-16',  50),
        ('cust_1', DATE '2020-05-17', 100),
        ('cust_2', DATE '2020-05-18',   6),
        ('cust_2', DATE '2020-05-20',   3))
    SELECT customer_id, old_price, old_price_date, new_price, price_decrease_date, old_price-new_price AS difference
        FROM orders
            MATCH_RECOGNIZE (
                PARTITION BY customer_id
                ORDER BY order_date
                MEASURES
                    PREV(DOWN.price) AS old_price,
                    PREV(DOWN.order_date) AS old_price_date,
                    LAST(DOWN.price) AS new_price,
                    DOWN.order_date AS price_decrease_date
                ALL ROWS PER MATCH
                AFTER MATCH SKIP PAST LAST ROW
                PATTERN (DOWN+)
                DEFINE
                    DOWN AS price < PREV(price)
                )
        ORDER BY customer_id, price_decrease_date;

## Resources
* [Cursors (Transact-SQL)](https://docs.microsoft.com/en-us/sql/t-sql/language-elements/cursors-transact-sql?view=sql-server-ver15)
* [WITH common_table_expression (Transact-SQL)](https://docs.microsoft.com/en-us/sql/t-sql/queries/with-common-table-expression-transact-sql?view=sql-server-ver15) 
* [Trino  with recursive clause](https://trino.io/docs/current/sql/select.html#with-recursive-clause)
* [Trino match recognize](https://trino.io/docs/current/sql/match-recognize.html#sql-match-recognize--page-root)
* [Trino row pattern recognition with MATCH_RECOGNIZE](https://trino.io/blog/2021/05/19/row_pattern_matching.html)

