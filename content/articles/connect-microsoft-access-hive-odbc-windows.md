Title: Connect Microsoft Access to Hive with ODBC in Windows
Date: 2020-07-05
Category: Microsoft Access, Hive
Cover: /extra/microsoft-access-logo.png

Microsoft Access can be used to connect to Big Data. Cloudera Hadoop cluster is one of Big Data platforms. Those two systems can easily interact with each other by means of an ODBC connection. Microsoft Access natively supports ODBC drivers and Cloudera develops ODBC drivers for many Operation Systems. Hive data size might impact on a way how to work with data in Microsoft Access.

## 1. Check Microsoft Access version, if it is 32 or 64-bit.

* Open **Microsoft Access**.
* Go to **File** menu.
* Click **Account** item.
* Click **About Access** button.

![Microsoft Access version]({static}/images/connect-microsoft-access-hive-odbc-windows/microsoft-access-version.png)</br></br>

The image shows Microsoft Access 64-bit.

## 2. Download Cloudera Hive ODBC driver.

The latest driver can be found on Cloudera web site. Let's download version [2.6.4 64-bit driver](https://www.cloudera.com/downloads/connectors/hive/odbc/2-6-4.html).

![Cloudera Hive driver download]({static}/images/connect-microsoft-access-hive-odbc-windows/cloudera-hive-driver-download.png)</br></br>

The file name is `ClouderaHiveODBC64.msi`.

## 3. Create new ODBC Data Source

* Click on **Windows Start** button and type in **ODBC** in **Search** bar, then run **ODBC Data Sources (64-bit)**.
* Go to **System DSN** tab. After installing Cloudera Hive driver, it will be 2 new entries: Sample Cloudera Hive DSN 32-bit and 64-bit.

![ODBC Data Source Administrator 64-bit]({static}/images/connect-microsoft-access-hive-odbc-windows/odbc-data-source-administrator-64bit.png)</br></br>

* Create a new data source

![Create New ODBC Data Source]({static}/images/connect-microsoft-access-hive-odbc-windows/create-new-odbc-data-source.png)</br></br>

![Cloudera ODBC driver for Apache Hive DSN Setup]({static}/images/connect-microsoft-access-hive-odbc-windows/cloudera-odbc-driver-for-apache-hive-dsn-setup.png)</br></br>

Fill out all fields and click **Test** button to make sure it works. The sample reveals one of the possible configurations.

Hive Server Type: **Hive Server 2**

Service Discovery Mode: **No Service Discovery**

Host(s): **samplehive**

Port: **10000**

Database: **default**

Mechanism: **Kerberos**

Host FQDN: **samplehive.com**

Service: **Hive**

Thrift Transport: **SASL**

## 4. Link Hive table to Microsoft Access

* Open **Microsoft Access**.
* Go to **External Data** menu, then **New Data Source**, after **From Other Sources**, and finally **ODBC Database**.

![Microsoft Access new data source]({static}/images/connect-microsoft-access-hive-odbc-windows/microsoft-access-new-data-source.png)</br></br>

* Select **Link to the data source by creating a linked table** option.

![Link to the data source by creating a linked table]({static}/images/connect-microsoft-access-hive-odbc-windows/link-to-the-data-source-by-creating-a-linked-table.png)</br></br>

* Pick up **Sample Hive** Machine Data Source created on step #3.

![Select data source]({static}/images/connect-microsoft-access-hive-odbc-windows/select-data-source.png)</br></br>

* Be patient. It might take a few minutes to show a list of all tables in all Hive databases.

* Select your table, and then a unique column or columns to finish it up.

## 5. Change Microsoft Access timeout when opening a Hive linked table

In some cases, the timeout error can come up. It depends on how busy is Hive or how much data is in a linked table.

* Open **Registry Editor** in Windows.

* Find `HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\Office\15.0\Access Connectivity Engine\Engines\ODBC` entry and change `QueryTimeout` value.

![Registry Editor query timeout setting]({static}/images/connect-microsoft-access-hive-odbc-windows/registry-editor-query-timeout-setting.png)</br></br>

## 6. Features to access Hive data

The size is the main limitation to use Hive data so you might need to follow those guidelines.

* Before a table usage, figure out the record count in your table.
* If size is up to 100,000 records, the table can be used as a linked one with all available operations in Microsoft Access.
* If size is larger than 100,000 records or so, create a query to filter data to the acceptable size and export data to a Microsoft Access table.

