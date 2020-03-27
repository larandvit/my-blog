Title: Connect DBeaver to MS SQL Server with Kerberos Ticket Cache in Windows
Date: 2020-03-25
Category: DBeaver, MS SQL Server, Windows, Kerberos, Authentication

The easiest way to connect DBeaver to MS SQL Server with Kerberos authentication is Kerberos ticket cache. It requests only 2 steps. The first step is to create Kerberos ticket cache and the second one is to add a new connection to DBeaver with default settings. There are variety of tool in Windows to create Kerberos ticket cache. Also, you can apply different techniques to generate ticket cache.

## 1. Create Kerberos Ticket Cache File

There are some ways to proceed with it. 

1. Run `kinit` java tool located in **C:\Program Files\Java\jre[version]\bin** folder. It's generated **C:\Users\windowsuser\krb5cc_windowsuser** Kerberos ticket cache file.

    a) If Kerberos ticket cache is created for a user currently logged in to a Windows computer

        :::bash
        kinit

    Output
 
        :::text
        Password for windowsuser@SAMPLE.COM:
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser

    b) If Kerberos ticket cache is created for a different user from currently logged in to a Windows computer

        :::bash
        kinit windowsuser@SAMPLE.COM

    Output

        :::text
        Password for windowsuser@SAMPLE.COM:
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser

2. Use **MIT Kerberos Ticket Manager** interactive tool with user interface. It can be loaded from [MIT Kerberos Distribution Page](https://web.mit.edu/kerberos/dist/).

3. Utilize Kerberos keytab file with kerberized Windows service account provided by your administrator. Notice that a Kerberos ticket cache file is created in context of a current user logged in to a Windows computer. It's the best location as it's the default one.

        :::bash
        kinit servicewindowsaccount@SAMPLE.COM -k -t keytabname.keytab

    Output

        :::text
        New ticket is stored in cache file C:\Users\windowsuser\krb5cc_windowsuser
        
The created cache file can be validated with `klist` command

    :::bash
    klist

Output

    :::text
    Credentials cache: C:\Users\windowsuser\krb5cc_windowsuser
    
    Default principal: windowsuser@SAMPLE.COM, 1 entry found.
    
    [1]  Service Principal:  krbtgt/SAMPLE.COM@SAMPLE.COM
         Valid starting:     Mar 26, 2020 21:35:00
         Expires:            Mar 27, 2020 07:35:00
  
## 2. Add New MS SQL Connection

1. Open **New Database Connection** wizard from **Database** menu and select **MS SQL Server** driver.

    ![DBeaver Select Database Step]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/select-your-database-step.png)</br></br>

2. On the next step, replace **Host** with your server name and choose **Kerberos** from **Authentication** list.

    ![DBeaver MS SQL Server Connection Settings Step]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/sqlserver-connection-settings-step.png)</br></br>

3. Click **Finish** button to complete wizard.

4. Next screen will request to download drivers. Just click **Download** button.

    ![DBeaver Download Driver Files]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/download-driver-files.png)</br></br>

5. Be patient, it takes time to retrieve SQL server meta data.

    ![DBeaver MS SQL Server Connection Established]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/mssql-server-connection-established.png)</br></br>

## 3. Upgrade MS SQL Drivers

It's an optional step if you are able to connect to MS SQL Server.

1. Download [Microsoft JDBC Driver for SQL Server](https://docs.microsoft.com/en-us/sql/connect/jdbc/download-microsoft-jdbc-driver-for-sql-server)

2. Unzip driver and store in **C:\Users\windowsuser\AppData\Roaming\DBeaverData\drivers** DBeaver settings folder, for example, **C:\Users\windowsuser\AppData\Roaming\DBeaverData\drivers\sqljdbc_8.2**

3. Edit created connection

    ![DBeaver Edit Connection Context Menu]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/edit-connection-context-menu.png)</br></br>

4. Click **Edit Driver Settings** button

    ![DBeaver MS SQL Server Connection Settings]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/sqlserver-connection-settings-step.png)</br></br>

5. Delete all files in **Libraries** tab

    ![DBeaver Edit MS SQL Server Driver]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/edit-driver-sqlserver.png)</br></br>

6. Click **Add File** button and select downloaded files

    a) JDBC driver corresponding your java version, for example, java 1.8 - mssql-jdbc-8.2.2.jre8.jar.

    b) Authentication library. It should be 64 bit - mssql-jdbc_auth-8.2.2.x64.dll.

    ![DBeaver MS SQL Server Driver Added]({static}/images/connect-dbeaver-to-mssql-server-with-kerberos-ticket-cache-in-windows/sqlserver-drivers-added.png)</br></br>

7. Click **OK** button twice to complete setup.

8. Restart DBeaver to apply new drivers.

