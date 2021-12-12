Title: Trino Insights Database Maintenance
Date: 2021-12-12
Category: Trino(Presto)
Cover: /extra/trino-logo.png

Insights is a part of functionality included in [Starburst](https://www.starburst.io/) Enterprise platform (SEP). Insights shows cluster history, usage metrics, and query history and details. Data is persisted in the insights database. Postgres, Oracle, or MySql is a list of supported backend database management system which is used to store collected data.

Since insights data is generated pretty frequently, the size of the database is going to grow quickly and you will encounter two major issues. The first one is the space to store data and the second one is performance to retrieve data. To solve those issues, you need to set up a process to purge data on regular basis.

As per release 363-e, there are 5 tables for purging in the insights database. It includes some dependent tables where filter can be applied through another parent table.

1. cluster_metrics - cluster history.
2. completed_queries - details of completed queries.
3. query_tables - number of records completed queries. It depends on completed_queries table.
4. query_user_group - user groups to run queries. It depends on completed_queries table.
5. usage_metrics - cluster usage metrics.

## 1. Develop script to purge data

The sample of the script is for Postgres database. DBeaver database administration tool is aimed for executing of the script as it is implemented script variables. 

`DAYS_RETENTION` parameters defines how many days back we want to keep in database, for example, 60.

`TEST_MODE` parameter helps run it in test run mode with `SELECT COUNT(*)` value or in purge mode with `DELETE` value.

    :::sql
    @set DAYS_RETENTION = 60
    -- Options
    --  1. test run: SELECT COUNT(*)
    --  2. run: DELETE
    @set TEST_MODE = DELETE

    -- process query_tables table
    ${TEST_MODE} FROM 
        public.query_tables 
    WHERE 
        query_id IN (SELECT 
                        query_id 
                     FROM 
                        public.completed_queries
                     WHERE 
                        create_time < CURRENT_DATE - ${DAYS_RETENTION});

    -- process query_user_group table
    ${TEST_MODE} FROM 
        public.query_user_group 
    WHERE 
        query_id IN (SELECT 
                        query_id 
                     FROM 
                        public.completed_queries
                     WHERE 
                        create_time < CURRENT_DATE - ${DAYS_RETENTION});

    -- process completed_queries table
    ${TEST_MODE} FROM 
        public.completed_queries
    WHERE 
        create_time < CURRENT_DATE - ${DAYS_RETENTION};
        
    -- process cluster_metrics table
    ${TEST_MODE} FROM 
        public.cluster_metrics
    WHERE 
        sample_interval_start < CURRENT_DATE - ${DAYS_RETENTION};
        
    -- process usage_metrics table
    ${TEST_MODE} FROM 
        public.usage_metrics
    WHERE 
        start_time < CURRENT_DATE - ${DAYS_RETENTION};
    
The script can be enhanced adding additional filters if needed, for example, if you need to delete queries belonging to a specific user, `usr='user_name'`.

## 2. Figure out a connection to insights database

* Open `config.properties` file on your coordinator. 
* Find the settings.

        :::ini
        insights.jdbc.url=jdbc:postgresql://postgresql.example.com:5432/sepquerylogger
        insights.jdbc.user=test_psql
        insights.jdbc.password=test12 

## 3. Set up automatic process run on schedule

DBeaver script variables can be hardcoded and the script might be run as a crontab job in Linux, pgAgent agent, or pg_cron extension.

## Resources
* [Insights introduction](https://docs.starburst.io/latest/insights/index.html#insights-configuration)
* [Query logger](https://docs.starburst.io/latest/admin/query-logger.html)
* [An Overview of Job Scheduling Tools for PostgreSQL](https://severalnines.com/database-blog/overview-job-scheduling-tools-postgresql)

