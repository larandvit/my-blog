Title: Field Delimiter in Apache Hive Table
Date: 2020-04-25
Category: Hive
Cover: /extra/apache-hive-logo.png

Not too much official documentation can be found on how to define a field delimiter in a create or an alter Apache Hive statement. This setting is requested for delimited text files placed as source of Hive tables. When a field delimiter is not assigned properly, Hive can't split data into columns, and as a result, the first column will contain all data and the rest of columns will have NULL values. Also, it's critical to know a default field delimiter if field delimiter setting is missed in a create statement.

There are 2 major SerDe (Serializer/Deserializer) for text data. SerDe defines input/output (IO) interface which handles: (1) read data from a Hive table and (2) write it back out to HDFS. `org.apache.hadoop.hive.serde2` is the Hive SerDe library including `TEXTFILE` formats.

1. `org.apache.hadoop.hive.serde2.lazy.LazySimpleSerDe`. The default field delimiter value is `'\001'`.
2. `org.apache.hadoop.hive.serde2.OpenCSVSerde`The default field delimiter value is `','`.

`LazySimpleSerDe` is more efficient in terms of performance. `OpenCSVSerde has` a limitation to handle only string data type in Hive tables. The default format is `LazySimpleSerDe`.

The main issue with field delimiter is that Java `char` data type is used as an argument to assign a field delimiter. It can hold only 2 bytes. Java `char` data type can understand both ASCII and Unicode characters but it can handle Unicode characters which belong to ASCII table.

The rules to assign a filed delimiter are.

1. Any visible ASCII character can be assigned directly, for example, `'1'`, `'a'`, or `'!'`.
2. It can be used special predefined characters, for example. `'\t'`, `'\r'`, and `'\n'`.
3. If a character belongs to ASCII set and invisible, it can be used octal or Unicode notations.

Octal starts from back slash and contains 3 digits, for example, `'\001'`. Character `'a'` is `'\040'`.

Hex has '\u' prefix and includes 4 digits. It represents a Unicode code but you have to use decimal ASCII code, for example, `'\u0010'` definition is converted to `'\000a'` Hive table field delimiter. Another sample is visible ASCII character `'a'`, `'\u0032'` field delimiter definition is converted to`'\0020'` in Hive table.

Those commands can be used to retrieve field delimiter for a table from Hive meta data.

* Show statement.

        :::sql
        SHOW CREATE TABLE sample_table_name;

* Describe statement #1.

        :::sql
        DESCRIBE FORMATTED sample_table_name;

* Describe statement #2.

        :::sql
        DESCRIBE EXTENDED sample_table_name;

Field delimiter can be assigned or changed in those Hive statements.

* CREATE statement with `LazySimpleSerDe` interface.
   
        :::sql
        CREATE TABLE sample (
            column1 string,
            column2 string,
            column3 string)
        ROW FORMAT DELIMITED 
           FIELDS TERMINATED BY ',' 
           LINES TERMINATED BY '\n'
        STORED AS TEXTFILE
        LOCATION '/folder1/folder2'

* CREATE statement with `OpenCSVSerde` interface.

        :::sql
        CREATE TABLE sample (
            column1 string,
            column2 string,
            column3 string)
        ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
        WITH SERDEPROPERTIES (
           'separatorChar'=',')
        STORED AS TEXTFILE
        LOCATION '/folder1/folder2'
   
* ALTER statement with `LazySimpleSerDe` interface.

        :::sql
        ALTER TABLE multi_char_field_delim SET SERDEPROPERTIES('field.delim'=',')

* ALTER statement with `OpenCSVSerde` interface.

        :::sql
        ALTER TABLE multi_char_field_delim SET SERDEPROPERTIES('separatorChar'=',')

    or

        :::sql
        ALTER TABLE multi_char_field_delim SET SERDEPROPERTIES('field.delim'=',')
