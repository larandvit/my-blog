Title: Common Table Expressions in Trino
Date: 2024-09-28
Modified: 2024-10-06
Category: Trino
Cover: /extra/trino-logo.png

Common table expression (CTE) helps organize your code and make it more efficient utilizing the same data multiple times. It is how it works in relational database management systems (RDBMSs). [Trino](https://trino.io/) handles CTEs differently because it is a query engine and it can't store temporary named result sets. CTEs are inlined before sending for execution by Trino. This limitation makes usage of CTE in Trino inefficient. Moreover, CTE can produce incorrect results as named result set is non-deterministic and the results may be different each time.

Instead of using a CTE in Trino, it can be created a temporary table. This way, you make your code more efficient retrieving data only once if a named result set is going to be used more than one time in your CTE statement. Otherwise, Trino executes the same named result set as many times as the named result set is referred in your statement.

Some products rely on CTEs heavily, for example, [dbt](https://www.getdbt.com/) and it impacts on usage of those products with Trino.


##CTE explanation

`filtered_cte` and `deduped_cte` are inlined when executed by Trino.

Submitted to Trino statement

    :::sql
    WITH 
    filtered_cte AS (
        SELECT 
            region
            ,city
            ,population
            ,create_date
        FROM  
            data_source   
        WHERE  
            create_date>timestamp '2024-12-31')
    deduped_cte AS (
        SELECT 
            region
            ,city
            ,population
            ,ROW_NUMBER() OVER(PARTITION BY region,city ORDER BY create_date DESC) AS dedupe_group
        FROM filtered_CTE)
    SELECT
        *
    FROM 
        deduped_cte
    WHERE 
        dedupe_group = 1;

Executed Trino statement

    :::sql
    SELECT
        *
    FROM 
        (SELECT 
            region
            ,city
            ,population
            ,ROW_NUMBER() OVER(PARTITION BY region,city ORDER BY create_date DESC) AS dedupe_group
        FROM 
            (SELECT 
                region
                ,city
                ,population
                ,create_date
            FROM  
                data_source   
            WHERE  
                create_date>timestamp '2024-12-31') a
        ) b
    WHERE 
        dedupe_group = 1;

###CTE inefficiency

`filtered_cte` named result set is executed 2 times by Trino.

Submitted to Trino statement

    :::sql
    WITH 
    filter_cte AS (
        SELECT 
            region_id
            ,return_id
        FROM 
            data_source   
        WHERE  
            invoice_status='completed'
    calc_cte1 AS (
        SELECT 
            region_id
            ,MAX(return_id) AS return_id
            ,COUNT(*) AS region_count
        FROM 
            filter_CTE
        GROUP BY
            region_id),
    calc_cte2 AS (
        SELECT 
            return_id
            ,MAX(region_id) AS region_id
            ,COUNT(*) AS return_count
        FROM 
            filter_CTE
        GROUP BY
            return_id),
    SELECT
        c1.region_id
        ,c1.region_count
        ,c2.return_id
        ,c2.return_count
    FROM 
        calc_cte1 c1 
    JOIN calc_cte2 c2 ON 
        c1.region_id=c2.region_id
        AND c1.return_id=c2.return_id;

Executed Trino statement

    :::sql
    SELECT
        c1.region_id
        ,c1.region_count
        ,c2.return_id
        ,c2.return_count
    FROM 
        (SELECT 
            region_id
            ,MAX(return_id) AS return_id
            ,COUNT(*) AS return_count
        FROM 
            (SELECT 
                region_id
                ,return_id 
            FROM
                data_source   
            WHERE
                invoice_status='completed') filter_cte
        GROUP BY 
            region_id) c1 
    JOIN 
        (SELECT 
            return_id
            ,MAX(region_id) AS region_id
            ,COUNT(*) AS return_count
        FROM 
            (SELECT 
                region_id
                ,return_id 
            FROM  
                data_source   
            WHERE 
                invoice_status='completed') filter_cte
        GROUP BY
            return_id) c2 
        ON c1.region_id=c2.region_id
           AND c1.return_id=c2.return_id;

### CTE with incorrect results

`id` column calculated 2 times, so it can't be used as a join column.

Submitted to Trino statement

    :::sql
    WITH 
    cte1 AS (
        SELECT
            region_id
            ,return_id
            ,CAST(UUID() AS VARCHAR) AS id
        FROM
            data_source),
    cte21 AS (
        SELECT
            region_id
            ,id
        FROM
            cte1),
    cte22 AS (
        SELECT
            return_id
            ,id
        FROM
            cte1)
    SELECT
        cte21region_id
        ,cte22.return_id
        ,cte21.id
    FROM
        cte21
    JOIN
        cte22 
        ON cte21.id=cte22.id;

Executed by Trino statement

    :::sql
    SELECT
        cte21.region_id
        ,cte22.return_id
        ,cte21.id
    FROM
        (SELECT
            region_id
            ,id
        FROM
            (SELECT
                region_id
                ,return_id
                ,CAST(UUID() AS VARCHAR) AS id
            FROM
                data_source) cte1
        ) cte21
    JOIN
        (SELECT
            return_id
            ,id
        FROM
            (SELECT
                region_id
                ,return_id
                ,CAST(UUID() AS VARCHAR) AS id
            FROM
                data_source) cte1
        ) cte22
        ON cte21.id=cte22.id;

## Resources
* [WITH clause in Trino](https://trino.io/docs/current/sql/select.html#with-clause)
