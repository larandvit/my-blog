Title: Multi-Character Field Delimiter in Apache Hive Table
Date: 2020-04-29
Category: Hive
Cover: /extra/apache-hive-logo.png

Multi-character field delimiter is not implemented in `LazySimpleSerDe` and `OpenCSVSerde` text file SerDe classes. There are two options to use multi-character field delimiter in Hive. The first option is `MultiDelimitSerDe` class specially developed to handle multi-character field delimiters. The second one is to use `RegexSerDe` class as a workaround. Those two options don't work as expected because of limitations.

## MultiDelimitSerDe implementation

`MultiDelimitSerDe`SerDe is considered as experimental one until Hive release 4.0.0. It's included in `hive-contrib-<version>.jar` library and you have to add the library to the class path. If `hive-contrib-<version>.jar` library is not included in the class path, the functionality is limited to run only `SELECT * FROM table_name;` queries. The limitation is caused by map/reduce jobs which don't have access to the library. The issue should be fixed in Hive 4.0.0 when `MultiDelimitSerDe`class is supposed to be included in `org.apache.hadoop.hive.serde2` library. Currently, `MultiDelimitSerDe`class is a part of `org.apache.hadoop.hive.contrib.serde2` library.

One more limitation is that skip header lines functionality (`TBLPROPERTIES ("skip.header.line.count"="1")`) doesn't work.

* Sample of experimental version with `~|` field delimiter. The code is run on Hive 1.1.0.

        :::sql
        CREATE EXTERNAL TABLE `sample_table`(
           `column1` string,
           `column2` int,
           `column3` decimal(10,2),
           `column4` timestamp)
        ROW FORMAT SERDE 'org.apache.hadoop.hive.contrib.serde2.MultiDelimitSerDe' 
        WITH SERDEPROPERTIES ("field.delim"="~|");
        LOCATION
         '/folder/folder2'

* Sample of final version with `~|` field delimiter. The code can't be validated.

        :::sql
        CREATE EXTERNAL TABLE `sample_table`(
           `column1` string,
           `column2` int,
           `column3` decimal(10,2),
           `column4` timestamp)
        ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.MultiDelimitSerDe' 
        WITH SERDEPROPERTIES ("field.delim"="~|");
        LOCATION
         '/folder/folder2'

## RegexSerDe implementation

`RegexSerDe` SerDe limitation is to support only `string` data type in Hive tables. Also, It's expected performance overhead.

* Sample with `~|` field delimiter.

        :::sql
        CREATE EXTERNAL TABLE `sample_table`(
           `column1` string,
           `column2` string,
           `column3` string,
           `column4` string)
        ROW FORMAT SERDE 'org.apache.hadoop.hive.contrib.serde2.RegexSerDe'
        WITH SERDEPROPERTIES (
          "input.regex" = "(.*)[~][|](.*)[~][|](.*)[~][|](.*)"
        )
        LOCATION
         '/folder/folder2'
        TBLPROPERTIES (
         "skip.header.line.count"="1"
        );

## Resources
* [MultiDelimitSerDe](https://cwiki.apache.org/confluence/display/Hive/MultiDelimitSerDe)
* [Use multiple-characters as field delimiter](https://issues.apache.org/jira/browse/HIVE-5871)
* [Include MultiDelimitSerDe in HIveServer2 By Default](https://issues.apache.org/jira/browse/HIVE-20619)
* [Hive contrib jar should not be in lib](https://issues.apache.org/jira/browse/HIVE-20020)
